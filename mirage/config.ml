open Mirage

let net = generic_stackv4 default_network

let seed =
  let doc = Key.Arg.info ~doc:"Seed for the ssh private key." ["seed"] in
  Key.(create "seed" Arg.(opt string "" doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator for SSH." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt string "" doc))

(* set ~tls to false to get a plain-http server *)
let http_srv = cohttp_server @@ conduit_direct ~tls:true net

(* TODO: make it possible to enable and disable schemes without providing a port *)
let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] ~docv:"PORT" in
  Key.(create "http_port" Arg.(opt (some int) None doc))

let https_port =
  let doc = Key.Arg.info ~doc:"Listening HTTPS port." ["https"] ~docv:"PORT" in
  Key.(create "https_port" Arg.(opt (some int) None doc))

let zap = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "caldavzap"

let admin_password =
  let doc = Key.Arg.info ~doc:"Password for the administrator." ["admin-password"] ~docv:"STRING" in
  Key.(create "admin_password" Arg.(opt (some string) None doc))

let remote =
  let doc = Key.Arg.info ~doc:"Location of calendar data." [ "remote" ] ~docv:"REMOTE" in
  Key.(create "remote" Arg.(opt string "" doc))

let tofu =
  let doc = Key.Arg.info ~doc:"If a user does not exist, create them and give them a new calendar." [ "tofu" ] in
  Key.(create "tofu" Arg.(flag doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"dns server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"certificate key seed" ["key-seed"] in
  Key.(create "key-seed" Arg.(required string doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "calendar.robur.coop" doc))

let apple_testable =
  let doc = Key.Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Key.(create "apple_testable" Arg.(flag doc))

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")

let main =
  let direct_dependencies = [
    package "uri" ;
    package "caldav" ;
    package ~min:"0.1.2" "icalendar" ;
    package "irmin-mirage-git" ;
    package "irmin-mem" ;
    package ~sublibs:["mirage"] "dns-certify";
    package ~sublibs:["mirage"] "logs-syslog";
    package "monitoring-experiments";
  ] in
  let keys =
    [ Key.abstract seed ; Key.abstract authenticator ;
      Key.abstract http_port ; Key.abstract https_port ;
      Key.abstract admin_password ; Key.abstract remote ;
      Key.abstract tofu ; Key.abstract dns_key ; Key.abstract dns_server ;
      Key.abstract dns_port ; Key.abstract key_seed ;
      Key.abstract name ; Key.abstract syslog ; Key.abstract monitor ;
      Key.abstract apple_testable ]
  in
  foreign
    ~deps:[ abstract app_info ] ~packages:direct_dependencies ~keys
    "Unikernel.Main" (console @-> random @-> time @-> pclock @-> mclock @-> stackv4 @-> http @-> resolver @-> conduit @-> kv_ro @-> stackv4 @-> job)

let () =
  register "caldav" [main $ default_console $ default_random $ default_time $ default_posix_clock $ default_monotonic_clock $ net $ http_srv $ resolver_dns net $ conduit_direct ~tls:true net $ zap $ management_stack ]
