open Lwt.Infix
module Fs = FS_unix

module Xml = Webdav_xml

type t = Fs.t

let (>>==) a f = a >>= function
  | Error e -> Lwt.return (Error e)
  | Ok res  -> f res

let (>>|=) a f = a >|= function
  | Error e -> Error e
  | Ok res  -> f res

let isdir fs name =
  Fs.stat fs name >>|= fun stat ->
  Ok stat.Mirage_fs.directory

type file = [ `File of string list ]
type dir = [ `Dir of string list ]

type file_or_dir = [ file | dir ]

let basename = function
  | `File path | `Dir path ->
    match List.rev path with
    | base::_ -> base
    | [] -> invalid_arg "basename of root directory not allowed"

let create_file (`Dir data) name =
  `File (data @ [name])

(* TODO: no handling of .. done here yet *)
let data str = Astring.String.cuts ~empty:false ~sep:"/" str

let dir_from_string str = `Dir (data str)

let file_from_string str = `File (data str)

let from_string fs str =
  isdir fs str >>|= fun dir ->
  Ok (if dir then `Dir (data str) else `File (data str))

let to_string =
  let a = Astring.String.concat ~sep:"/" in
  function
  | `File data -> a data
  | `Dir data -> a data ^ "/"

let parent f_or_d =
  let parent p =
    match List.rev p with
    | _ :: tl -> `Dir (List.rev tl)
    | [] -> `Dir []
  in
  match f_or_d with
  | `Dir d -> parent d
  | `File f -> parent f

let propfilename =
  let ext = ".prop.xml" in
  function
  | `Dir data -> `File (data @ [ ext ])
  | `File data -> match List.rev data with
    | filename :: path -> `File (List.rev path @ [ filename ^ ext ])
    | [] -> assert false (* no file without a name *)

type propmap = (Xml.attribute list * Xml.tree list) Xml.PairMap.t

let get_properties fs f_or_d =
  let propfile = to_string (propfilename f_or_d) in
  Fs.size fs propfile >>== fun size ->
  Fs.read fs propfile 0 (Int64.to_int size)

(* TODO: check call sites, used to do:
    else match Xml.get_prop "resourcetype" map with
      | Some (_, c) when List.exists (function `Node (_, "collection", _) -> true | _ -> false) c -> name ^ "/"
      | _ -> name in
*)
let write_property_map fs f_or_d map =
  let map' = match f_or_d with
    | `File _ -> map
    | `Dir _ -> Xml.PairMap.remove (Xml.dav_ns, "getetag") map
  in
  let data = Xml.props_to_string map' in
  let filename = to_string (propfilename f_or_d) in
  Printf.printf "writing property map %s: %s\n%!" filename data ;
  Fs.destroy fs filename >>= fun _ ->
  Fs.write fs filename 0 (Cstruct.of_string data)

let size fs (`File file) =
  let name = to_string (`File file) in
  Fs.size fs name

let stat fs f_or_d = Fs.stat fs (to_string f_or_d)

let exists fs str =
  Fs.stat fs str >|= function
  | Ok _ -> true
  | Error _ -> false

let dir_exists fs (`Dir dir) =
  Fs.stat fs (to_string (`Dir dir)) >|= function
  | Ok s when s.Mirage_fs.directory -> true
  | _ -> false

let listdir fs (`Dir dir) =
  let dir_string = to_string (`Dir dir) in
  Fs.listdir fs dir_string >>== fun files ->
  Lwt_list.fold_left_s (fun acc fn ->
      if Astring.String.is_suffix ~affix:".prop.xml" fn then
        Lwt.return acc
      else
        let str = dir_string ^ fn in
        isdir fs str >|= function
        | Error _ -> acc
        | Ok is_dir ->
          let f_or_d =
            if is_dir
            then dir_from_string str
            else file_from_string str
          in
          f_or_d :: acc)
    [] files >|= fun files ->
  Ok files

let get_raw_property_map fs f_or_d =
  get_properties fs f_or_d >|= function
  | Error e ->
    Format.printf "error while getting properties %a\n%!" Fs.pp_error e ;
    None
  | Ok data ->
    let str = Cstruct.(to_string @@ concat data) in
    match Xml.string_to_tree str with
    | None ->
      Printf.printf "couldn't convert %s to xml tree\n%!" str ;
      None
    | Some t -> Some (Xml.prop_tree_to_map t)

(* property getlastmodified does not exist for directories *)
let last_modified_as_ptime fs f_or_d =
  get_raw_property_map fs f_or_d >|= function
  | None ->
    Printf.printf "invalid XML!\n" ;
    None
  | Some map ->
    match Xml.get_prop (Xml.dav_ns, "getlastmodified") map with
    | Some (_, [ Xml.Pcdata last_modified ]) ->
      begin match Ptime.of_rfc3339 last_modified with
        | Error _ ->
          Printf.printf "invalid data!\n" ;
          None
        | Ok (ts, _, _) -> Some ts
      end
    | _ -> None

(* we only take depth 1 into account when computing the overall last modified *)
let last_modified_of_dir map fs (`Dir dir) =
  let start = match Xml.get_prop (Xml.dav_ns, "creationdate") map with
    | Some (_, [ Xml.Pcdata date ]) ->
      begin match Ptime.of_rfc3339 date with
        | Error _ -> assert false
        | Ok (ts, _, _) -> ts
      end
    | _ -> Ptime.epoch
  in
  listdir fs (`Dir dir) >>= function
  | Error _ -> Lwt.return (Xml.ptime_to_http_date start)
  | Ok files ->
    Lwt_list.map_p (last_modified_as_ptime fs) files >>= fun last_modifieds ->
    let lms = List.fold_left (fun acc -> function None -> acc | Some lm -> lm :: acc) [] last_modifieds in
    let max_mtime a b = if Ptime.is_later ~than:a b then b else a
    in
    Lwt.return (Xml.ptime_to_http_date @@ List.fold_left max_mtime start lms)

let get_etag fs f_or_d =
  get_raw_property_map fs f_or_d >|= function
  | None ->
    Printf.printf "invalid XML!\n" ;
    None
  | Some map -> match Xml.get_prop (Xml.dav_ns, "getetag") map with
    | Some (_, [ Xml.Pcdata etag ]) -> Some etag
    | _ -> Some (to_string f_or_d)

let etag_of_dir fs (`Dir dir) =
  listdir fs (`Dir dir) >>= function
  | Error _ -> Lwt.return ""
  | Ok files ->
    Lwt_list.map_p (get_etag fs) files >>= fun etags ->
    let some_etags = List.fold_left (fun acc -> function None -> acc | Some x -> x :: acc) [] etags in
    let data = String.concat ":" some_etags in
    Lwt.return (Digest.to_hex @@ Digest.string data)

(* let open_fs_error x =
 *   (x : ('a, Fs.error) result Lwt.t :> ('a, [> Fs.error ]) result Lwt.t) *)

let get_property_map fs f_or_d =
  get_raw_property_map fs f_or_d >>= function
  | None -> Lwt.return None
  | Some map -> match f_or_d with
    | `File _ ->
      begin match Xml.PairMap.find_opt (Xml.dav_ns, "getlastmodified") map with
        | Some (_, [ Xml.Pcdata rfc3339 ]) ->
          begin match Ptime.of_rfc3339 rfc3339 with
            | Error _ ->
              Printf.printf "invalid data!\n" ;
              assert false
            | Ok (ts, _, _) ->
              let http_date = Xml.ptime_to_http_date ts in
              let map' =
                Xml.PairMap.add (Xml.dav_ns, "getlastmodified")
                  ([], [ Xml.pcdata http_date ]) map
              in
              Lwt.return (Some map')
          end
        | _ -> Lwt.return (Some map)
      end
    | `Dir d ->
      last_modified_of_dir map fs (`Dir d) >>= fun last_modified ->
      etag_of_dir fs (`Dir d) >|= fun etag ->
      Some
        (Xml.PairMap.add (Xml.dav_ns, "getlastmodified") ([], [ Xml.pcdata last_modified ])
           (Xml.PairMap.add (Xml.dav_ns, "getetag") ([], [ Xml.pcdata etag ]) map))

let read fs (`File file) =
  let name = to_string (`File file) in
  Fs.size fs name >>== fun length ->
  Fs.read fs name 0 (Int64.to_int length) >>== fun data ->
  get_property_map fs (`File file) >|= function
  | None -> assert false
  | Some props -> Ok (Cstruct.concat data, props)

let mkdir fs (`Dir dir) propmap =
  Fs.mkdir fs (to_string (`Dir dir)) >>== fun () ->
  write_property_map fs (`Dir dir) propmap

let write fs (`File file) data propmap =
  let filename = to_string (`File file) in
  Fs.destroy fs filename >>= fun _ ->
  Fs.write fs filename 0 data >>== fun () ->
  write_property_map fs (`File file) propmap

let destroy fs f_or_d =
  let propfile = propfilename f_or_d in
  Fs.destroy fs (to_string propfile) >>== fun () ->
  Fs.destroy fs (to_string f_or_d)

let connect = Fs.connect

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
