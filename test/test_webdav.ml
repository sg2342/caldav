module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api
module Fs = Caldav.Webdav_fs

let header = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"

let tree xml = match Xml.string_to_tree xml with Some t -> t | None -> Alcotest.fail "Invalid xml."

let prop =
  let module M = struct
    type t = Xml.propfind
    let pp = Xml.pp_propfind
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let str_err =
  let module M = struct
    type t = string
    let pp = Fmt.string
    let equal = String.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_propfind () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "parsing <propfind/>"
              (Error "broken") (Xml.parse_propfind_xml (tree xml)))

let propname () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/></propfind>"
              (Ok `Propname) (Xml.parse_propfind_xml (tree xml)))

let two_propname () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><propname/><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/><propname/></propfind>"
              (Error "broken") (Xml.parse_propfind_xml (tree xml)))

let allprop () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><allprop/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><allprop/></propfind>"
              (Ok (`All_prop [])) (Xml.parse_propfind_xml (tree xml)))

let allprop_include () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><allprop/><include><foo/><bar/></include></propfind>" in
  Alcotest.(check (result prop str_err) "parsing all prop with includes"
              (Ok (`All_prop ["foo";"bar"])) (Xml.parse_propfind_xml (tree xml)))

let invalid_xml () =
  let error_tree xml = match Xml.string_to_tree xml with
    | Some _ -> ()
    | None -> invalid_arg "Invalid xml" in
  Alcotest.(check_raises "parsing header only" (Invalid_argument "Invalid xml")
              (fun () -> error_tree header));
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\">" in
  Alcotest.(check_raises "hanging paren"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propfind" in
  Alcotest.(check_raises "missing bracket"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propname xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "missing propfind"
              (Error "expected propfind, but found propname")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "empty propfind"
              (Error "broken")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><foo/></propfind>" in
  Alcotest.(check (result prop str_err) "unexpected element foo"
              (Error "broken")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><prop><foo><bar/></foo></prop></propfind>" in
  Alcotest.(check (result prop str_err) "non-flat property list"
              (Ok (`Props [ (Xml.dav_ns, "foo") ]))
              (Xml.parse_propfind_xml (tree xml)))

let parse_propfind_xml_tests = [
  "Empty", `Quick, empty_propfind ;
  "Propname", `Quick, propname ;
  "Two propnames", `Quick, two_propname ;
  "Allprop", `Quick, allprop ;
  "Allprop with includes", `Quick, allprop_include ;
  "Invalid XML", `Quick, invalid_xml ;
]

let calendar_query =
  let module M = struct
    type t = Xml.calendar_query
    let pp = Xml.pp_calendar_query
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let parse_simple_report_query () =
  let xml = header ^ {_|
   <C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
     </D:prop>
     <C:filter><C:comp-filter name="VCALENDAR"/></C:filter>
   </C:calendar-query>
|_} in
  let expected = Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag") ]),
                     ("VCALENDAR", `Is_defined)) in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_simple_report_query_with_calendar_data () =
  let xml = header ^ {_|
   <C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
         <C:comp name="VCALENDAR">
           <C:prop name="VERSION"/>
           <C:comp name="VEVENT">
             <C:prop name="SUMMARY"/>
             <C:prop name="UID"/>
             <C:prop name="DTSTART"/>
             <C:prop name="DTEND"/>
             <C:prop name="DURATION"/>
             <C:prop name="RRULE"/>
             <C:prop name="RDATE"/>
             <C:prop name="EXRULE"/>
             <C:prop name="EXDATE"/>
             <C:prop name="RECURRENCE-ID"/>
           </C:comp>
           <C:comp name="VTIMEZONE"/>
         </C:comp>
       </C:calendar-data>
     </D:prop>
     <C:filter><C:comp-filter name="VCALENDAR"/></C:filter>
   </C:calendar-query>
|_} in
  let expected =
    Ok (Some (`Proplist [
        `Prop (Xml.dav_ns, "getetag") ;
        `Calendar_data 
          (Some ("VCALENDAR",
           `Prop [ ("VERSION", false) ],
           `Comp [ ("VEVENT",
                    `Prop [
                      ("SUMMARY", false) ;
                      ("UID", false) ;
                      ("DTSTART", false) ;
                      ("DTEND", false) ;
                      ("DURATION", false) ;
                      ("RRULE", false);
                      ("RDATE", false) ;
                      ("EXRULE", false) ;
                      ("EXDATE", false) ;
                      ("RECURRENCE-ID", false) ],
                    `Comp []) ;
                   ("VTIMEZONE", `Prop [], `Comp []) ])
            , None, None) ] ),
        ("VCALENDAR", `Is_defined))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_1 () =
  let xml = header ^ {|<C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
         <C:comp name="VCALENDAR">
           <C:prop name="VERSION"/>
           <C:comp name="VEVENT">
             <C:prop name="SUMMARY"/>
             <C:prop name="UID"/>
             <C:prop name="DTSTART"/>
             <C:prop name="DTEND"/>
             <C:prop name="DURATION"/>
             <C:prop name="RRULE"/>
             <C:prop name="RDATE"/>
             <C:prop name="EXRULE"/>
             <C:prop name="EXDATE"/>
             <C:prop name="RECURRENCE-ID"/>
           </C:comp>
           <C:comp name="VTIMEZONE"/>
         </C:comp>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060104T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
                          </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [
        `Prop (Xml.dav_ns, "getetag") ;
        `Calendar_data (Some 
          ("VCALENDAR",
           `Prop [ ("VERSION", false) ],
           `Comp [ ("VEVENT",
                    `Prop [
                      ("SUMMARY", false) ;
                      ("UID", false) ;
                      ("DTSTART", false) ;
                      ("DTEND", false) ;
                      ("DURATION", false) ;
                      ("RRULE", false);
                      ("RDATE", false) ;
                      ("EXRULE", false) ;
                      ("EXDATE", false) ;
                      ("RECURRENCE-ID", false) ],
                    `Comp []) ;
                   ("VTIMEZONE", `Prop [], `Comp []) ]), None, None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some ("20060104T000000Z", "20060105T000000Z"), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_2 () =
  let xml = header ^ {|<C:calendar-query xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <C:calendar-data>
         <C:limit-recurrence-set start="20060103T000000Z"
                                 end="20060105T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060103T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
                          </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [
        `Calendar_data (None, Some (`Limit_recurrence_set ("20060103T000000Z", "20060105T000000Z")), None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some ("20060103T000000Z", "20060105T000000Z"), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_3 () =
  let xml = header ^ {|<C:calendar-query xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <C:calendar-data>
         <C:expand start="20060103T000000Z"
                   end="20060105T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060103T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [
        `Calendar_data (None, Some (`Expand ("20060103T000000Z", "20060105T000000Z")), None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some ("20060103T000000Z", "20060105T000000Z"), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_4 () =
  let xml = header ^ {|<C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <C:calendar-data>
         <C:limit-freebusy-set start="20060102T000000Z"
                                 end="20060103T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VFREEBUSY">
           <C:time-range start="20060102T000000Z"
                           end="20060103T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [
        `Calendar_data (None, None, Some (`Limit_freebusy_set ("20060102T000000Z", "20060103T000000Z"))) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VFREEBUSY", `Comp_filter (Some ("20060102T000000Z", "20060103T000000Z"), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_5 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VTODO">
           <C:comp-filter name="VALARM">
             <C:time-range start="20060106T100000Z"
                             end="20060107T100000Z"/>
           </C:comp-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VTODO", `Comp_filter ((None, [],
                                    [("VALARM",
                                      `Comp_filter (((Some ("20060106T100000Z",
                                                            "20060107T100000Z")),
                                                     [], [])))
                                      ])))
                     ]))) 
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_6 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="UID">
             <C:text-match collation="i;octet"
             >DC6C50A017428C5216A2F1CD@example.com</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [("VEVENT",
                     `Comp_filter ((None,
                                    [("UID",
                                      `Text (("DC6C50A017428C5216A2F1CD@example.com" , "i;octet", false), []))
                                      ],
                                    [])))
                     ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_7 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="ATTENDEE">
             <C:text-match collation="i;ascii-casemap"
              >mailto:lisa@example.com</C:text-match>
             <C:param-filter name="PARTSTAT">
               <C:text-match collation="i;ascii-casemap"
                >NEEDS-ACTION</C:text-match>
             </C:param-filter>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [("VEVENT",
                     `Comp_filter ((None, [("ATTENDEE",
                                      `Text ((("mailto:lisa@example.com",
                                               "i;ascii-casemap", false),
                                              [`Param_filter (("PARTSTAT",
                                                               `Text_match (
                                                                 ("NEEDS-ACTION",
                                                                  "i;ascii-casemap",
                                                                  false))
                                                                 ))
                                                ])))
                                      ],
                                    [])))
                     ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))


let parse_report_query_7_8_8 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT"/>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [], [("VEVENT", `Is_defined)]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_9 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VTODO">
           <C:prop-filter name="COMPLETED">
             <C:is-not-defined/>
           </C:prop-filter>
           <C:prop-filter name="STATUS">
             <C:text-match
                negate-condition="yes">CANCELLED</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [],
                   [("VTODO",
                     `Comp_filter ((None,
                                    [("COMPLETED", `Is_not_defined);
                                      ("STATUS",
                                       `Text ((("CANCELLED",
                                                "i;ascii-casemap", true),
                                               [])))
                                      ],
                                    [])))
                     ]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_10 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="X-ABC-GUID">
             <C:text-match>ABC</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [],
                   [("VEVENT",
                     `Comp_filter ((None,
                                    [("X-ABC-GUID",
                                      `Text ((("ABC", "i;ascii-casemap",
                                               false),
                                              [])))
                                      ],
                                    [])))
                     ]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_tests = [
  "Parse simple report query", `Quick, parse_simple_report_query ;
  "Parse simple report query with calendar data", `Quick, parse_simple_report_query_with_calendar_data ;
  "Parse report query from section 7.8.1", `Quick, parse_report_query_7_8_1 ;
  "Parse report query from section 7.8.2", `Quick, parse_report_query_7_8_2 ;
  "Parse report query from section 7.8.3", `Quick, parse_report_query_7_8_3 ;
  "Parse report query from section 7.8.4", `Quick, parse_report_query_7_8_4 ;
  "Parse report query from section 7.8.5", `Quick, parse_report_query_7_8_5 ;
  "Parse report query from section 7.8.6", `Quick, parse_report_query_7_8_6 ;
  "Parse report query from section 7.8.7", `Quick, parse_report_query_7_8_7 ;
  "Parse report query from section 7.8.8", `Quick, parse_report_query_7_8_8 ;
  "Parse report query from section 7.8.9", `Quick, parse_report_query_7_8_9 ;
  "Parse report query from section 7.8.10", `Quick, parse_report_query_7_8_10
]


let propupdate =
  let module M = struct
    type t = Xml.propupdate 
    let pp = Xml.pp_propupdate
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let proppatch () =
  let xml = header ^
    {|<D:propertyupdate xmlns:D="DAV:"
             xmlns:Z="http://ns.example.com/standards/z39.50/">
       <D:set>
         <D:prop>
           <Z:Authors>
             <Z:Author>Jim Whitehead</Z:Author>
             <Z:Author>Roy Fielding</Z:Author>
           </Z:Authors>
         </D:prop>
       </D:set>
       <D:remove>
         <D:prop><Z:Copyright-Owner/></D:prop>
       </D:remove>
      </D:propertyupdate>|}
  in
  Alcotest.(check (result (list propupdate) str_err) __LOC__
              (Ok [`Set ([],
                           ("http://ns.example.com/standards/z39.50/", "Authors"),
                           [Node ("http://ns.example.com/standards/z39.50/", "Author", [], [ Pcdata "Jim Whitehead"]) ;
                            Node ("http://ns.example.com/standards/z39.50/", "Author", [], [ Pcdata "Roy Fielding" ]) ]) ;
                     `Remove ("http://ns.example.com/standards/z39.50/", "Copyright-Owner") ])
    (Xml.parse_propupdate_xml (tree xml)))

let parse_propupdate_xml_tests = [
  "propertyupdate RFC example", `Quick, proppatch
]

let state_testable =
  let module M = struct
    type t = Mirage_fs_mem.t
    let pp = Mirage_fs_mem.pp
    let equal = Mirage_fs_mem.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let err_testable =
  let module M = struct
    type t = [ `Bad_request | `Conflict | `Forbidden of Xml.tree ]
    let pp ppf = function
      | `Bad_request -> Fmt.string ppf "bad request"
      | `Conflict -> Fmt.string ppf "conflict"
      | `Forbidden _ -> Fmt.string ppf "forbidden"
    let equal a b = match a, b with
      | `Bad_request, `Bad_request -> true
      | `Conflict, `Conflict -> true
      | `Forbidden x, `Forbidden y ->
        let to_string a = Xml.tree_to_string a
        in
        String.equal (to_string x) (to_string y)
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let mkcol_success () =
  let open Lwt.Infix in
  let body = {|<?xml version="1.0" encoding="utf-8" ?>
   <D:mkcol xmlns:D="DAV:"
                 xmlns:E="http://example.com/ns/">
     <D:set>
       <D:prop>
         <D:resourcetype>
           <D:collection/>
           <E:special-resource/>
         </D:resourcetype>
         <D:displayname>Special Resource</D:displayname>
       </D:prop>
     </D:set>
               </D:mkcol>|}
  in
  let res_fs, r =
    Lwt_main.run (
      let now = Ptime.v (1, 0L) in
      Mirage_fs_mem.connect "" >>= fun res_fs ->
      let content = {_|<?xml version="1.0" encoding="utf-8" ?>
<A:prop xmlns:A="DAV:"><A:resourcetype><A:collection></A:collection><B:special-resource xmlns:B="http://example.com/ns/"></B:special-resource></A:resourcetype><A:getlastmodified>1970-01-02T00:00:00-00:00</A:getlastmodified><A:getcontenttype>text/directory</A:getcontenttype><A:getcontentlength>0</A:getcontentlength><A:getcontentlanguage>en</A:getcontentlanguage><A:displayname>Special Resource</A:displayname><A:creationdate>1970-01-02T00:00:00-00:00</A:creationdate></A:prop>|_}
      in
      Mirage_fs_mem.write res_fs "home/special/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.connect "" >>= fun fs ->
      Mirage_fs_mem.mkdir fs "home" >>= fun _ ->
      Dav.mkcol ~now fs (Fs.dir_from_string "home/special/") (Some (tree body)) >|= fun r ->
      (res_fs, r))
  in
  Alcotest.(check (result state_testable err_testable) __LOC__
              (Ok res_fs) r)

let webdav_api_tests = [
  "successful mkcol", `Quick, mkcol_success
]

let tests = [
  "Read propfind", parse_propfind_xml_tests ;
  "Read propertyupdate", parse_propupdate_xml_tests ;
  "Report tests", report_tests ;
  "Webdav API", webdav_api_tests
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
