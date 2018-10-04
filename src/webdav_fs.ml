type file = [ `File of string list ]

type dir = [ `Dir of string list ]

type file_or_dir = [ file | dir ]

module type S =

sig

  val (>>==) : ('a, 'b) result Lwt.t -> ('a -> ('c, 'b) result Lwt.t) -> ('c, 'b) result Lwt.t

  type t

  type error

  type write_error

  val basename : file_or_dir -> string

  val create_file : dir -> string -> file

  val dir_from_string : string -> dir

  val file_from_string : string -> file

  val from_string : t -> string -> (file_or_dir, error) result Lwt.t

  val to_string : file_or_dir -> string

  val parent : file_or_dir -> dir

  val get_property_map : t -> file_or_dir -> Properties.t Lwt.t

  val write_property_map : t -> file_or_dir -> Properties.t ->
    (unit, write_error) result Lwt.t

  val size : t -> file -> (int64, error) result Lwt.t

  val read : t -> file -> (string * Properties.t, error) result Lwt.t

  val exists : t -> string -> bool Lwt.t

  val dir_exists : t -> dir -> bool Lwt.t

  val listdir : t -> dir -> (file_or_dir list, error) result Lwt.t

  val mkdir : t -> dir -> Properties.t -> (unit, write_error) result Lwt.t

  val write : t -> file -> string -> Properties.t -> (unit, write_error) result Lwt.t

  val destroy : t -> file_or_dir -> (unit, write_error) result Lwt.t

  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t
end

module Make (Fs:Irmin.KV with type contents = string) = struct

  open Lwt.Infix

  module Xml = Webdav_xml

  type t = Fs.t
  type error = unit
  type write_error = unit
  let pp_error _ _ = ()
  let pp_write_error _ _ = ()

  let (>>==) a f = a >>= function
    | Error e -> Lwt.return (Error e)
    | Ok res  -> f res

  let (>>|=) a f = a >|= function
    | Error e -> Error e
    | Ok res  -> f res

  let isdir fs name =
    Fs.kind fs name >|= function
    | None -> Error ()
    | Some `Contents -> Ok false
    | Some `Node -> Ok true

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
    let key = data str in
    isdir fs key >>|= fun dir ->
    Ok (if dir then `Dir key else `File key)

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
    | `Dir data -> data @ [ ext ]
    | `File data -> match List.rev data with
      | filename :: path -> List.rev path @ [ filename ^ ext ]
      | [] -> assert false (* no file without a name *)

  let get_properties fs f_or_d =
    let propfile = propfilename f_or_d in
    Fs.find fs propfile >|= function
    | None -> Error ()
    | Some data -> Ok data

  let info () =
    let date = 0L in
    let author = "caldav" in
    let message = "a calendar change" in
    Irmin.Info.v ~date ~author message
 
  (* TODO: check call sites, used to do:
      else match Xml.get_prop "resourcetype" map with
        | Some (_, c) when List.exists (function `Node (_, "collection", _) -> true | _ -> false) c -> name ^ "/"
        | _ -> name in
  *)
  let write_property_map fs f_or_d map =
    let map' = match f_or_d with
      | `File _ -> map
      | `Dir _ -> Properties.prepare_for_disk map
    in
    let data = Properties.to_string map' in
    let filename = propfilename f_or_d in
    Printf.printf "writing property map %s: %s\n%!" (String.concat "/" filename) data ;
    Fs.set ~info fs filename data >|= fun () -> Ok ()

  let size fs (`File file) =
    Fs.find fs file >|= function
    | None -> Error ()
    | Some data -> Ok (Int64.of_int @@ String.length data)

  let exists fs str =
    let file = data str in
    Fs.mem fs file

  let dir_exists fs (`Dir dir) =
    Fs.kind fs dir >|= function
    | None -> false
    | Some `Contents -> false
    | Some `Node -> true

  let listdir fs (`Dir dir) =
    Fs.list fs dir >|= fun files ->
    let files = List.fold_left (fun acc (step, kind) ->
        if Astring.String.is_suffix ~affix:".prop.xml" step then
          acc
        else
          let file = dir @ [step] in
          match kind with
          | `Contents -> `File file :: acc
          | `Node -> `Dir file :: acc)
      [] files in
    Ok files

  let get_raw_property_map fs f_or_d =
    get_properties fs f_or_d >|= function
    | Error e ->
      Format.printf "error while getting properties for %s %a\n%!" (to_string f_or_d) pp_error e ;
      None
    | Ok data ->
      match Xml.string_to_tree data with
      | None ->
        Printf.printf "couldn't convert %s to xml tree\n%!" data ;
        None
      | Some t -> Some (Properties.from_tree t)

  (* property getlastmodified does not exist for directories *)
  (* careful: unsafe_find *)
  let last_modified_as_ptime fs f_or_d =
    get_raw_property_map fs f_or_d >|= function
    | None ->
      Printf.printf "invalid XML!\n" ;
      None
    | Some map ->
      match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
      | Some (_, [ Xml.Pcdata last_modified ]) ->
        begin match Ptime.of_rfc3339 last_modified with
          | Error _ ->
            Printf.printf "invalid data!\n" ;
            None
          | Ok (ts, _, _) -> Some ts
        end
      | _ -> None

  (* we only take depth 1 into account when computing the overall last modified *)
  (* careful: unsafe_find *)
  let last_modified_of_dir map fs (`Dir dir) =
    let start = match Properties.unsafe_find (Xml.dav_ns, "creationdate") map with
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

  (* careful: unsafe_find *)
  let get_etag fs f_or_d =
    get_raw_property_map fs f_or_d >|= function
    | None ->
      Printf.printf "invalid XML!\n" ;
      None
    | Some map -> match Properties.unsafe_find (Xml.dav_ns, "getetag") map with
      | Some (_, [ Xml.Pcdata etag ]) -> Some etag
      | _ -> Some (to_string f_or_d)

  (* careful: unsafe_find (when calling get_etag) *)
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

  (* careful: unsafe_find, unsafe_add *)
  let get_property_map fs f_or_d =
    get_raw_property_map fs f_or_d >>= function
    | None -> Lwt.return Properties.empty
    | Some map -> match f_or_d with
      | `File _ ->
        begin match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
          | Some (_, [ Xml.Pcdata rfc3339 ]) ->
            begin match Ptime.of_rfc3339 rfc3339 with
              | Error _ ->
                Printf.printf "invalid data on get property map!\n" ;
                assert false
              | Ok (ts, _, _) ->
                let http_date = Xml.ptime_to_http_date ts in
                let map' =
                  Properties.unsafe_add (Xml.dav_ns, "getlastmodified")
                    ([], [ Xml.pcdata http_date ]) map
                in
                Lwt.return map'
            end
          | _ -> Lwt.return map
        end
      | `Dir d ->
        last_modified_of_dir map fs (`Dir d) >>= fun last_modified ->
        etag_of_dir fs (`Dir d) >|= fun etag ->
        (* inverse of Properties.prepare_for_disk *)
        Properties.unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Xml.pcdata last_modified ])
          (Properties.unsafe_add (Xml.dav_ns, "getetag") ([], [ Xml.pcdata etag ]) map)

  let read fs (`File file) =
    Fs.find fs file >>= function
    | None -> Lwt.return (Error ())
    | Some data -> 
      get_property_map fs (`File file) >|= fun props ->
      Ok (data, props)

  let mkdir fs (`Dir dir) propmap =
    write_property_map fs (`Dir dir) propmap

  let write fs (`File file) data propmap =
    Fs.set ~info fs file data >>= fun () ->
    write_property_map fs (`File file) propmap

  let destroy_file_or_empty_dir fs f_or_d =
    let propfile = propfilename f_or_d in
    Fs.remove ~info fs propfile >>= fun () ->
    let file = match f_or_d with
    | `File file | `Dir file -> file in
    Fs.remove ~info fs file >|= fun () -> Ok ()

  let destroy fs f_or_d =
    destroy_file_or_empty_dir fs f_or_d

end
