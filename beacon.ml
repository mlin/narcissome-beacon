open Batteries
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Printf

Printexc.record_backtrace true

module Data = struct
  (* allele on a chromosome: ((position,alternateBases),referenceBases *)
  type allele = ((int*string)*(string Set.t))
  (* map chromosome to sorted allele array. sorting by position will facilitate
     overlap lookups in the future *)
  type t = (string,allele array) Map.t

  let query ?reference_bases data chromosome position alternate_bases =
    try
      let alleles = Map.find chromosome data
      (* binary search the allele array *)
      let pos_alt = (position,alternate_bases)
      let rec bs lo hi =
        if lo >= hi then `False
        else
          let mid = lo + (hi-lo)/2
          let allele_pos_alt = fst (alleles.(mid))
          if      allele_pos_alt < pos_alt then bs (mid+1) hi
          else if allele_pos_alt > pos_alt then bs lo mid
          else
            match reference_bases with
              | Some dna when Set.mem dna (snd alleles.(mid)) -> `True
              | None -> `True
              | Some _ -> `False
      bs 0 (Array.length alleles)
    with Not_found -> `Null

(* server configuration *)
type config = {
  port : int;
  id : string;
  organization : string;
  description : string
}

(* beacon request *)
type request = {
  reference_bases : string;
  alternate_bases : string;
  chromosome : string;
  position : int;
  reference : string;
  dataset : string
}

(* parse beacon request from URI query string *)
let parse_request uri =
  let ms key = match Uri.get_query_param' uri key with
    | Some [str] when str <> "" -> str
    | _ -> invalid_arg ("invalid " ^ key)
  let os key = match Uri.get_query_param' uri key with
    | None -> ""
    | Some [str] -> str
    | _ -> invalid_arg ("invalid " ^ key)
  {
    reference_bases = String.uppercase (os "referenceBases");
    alternate_bases = String.uppercase (ms "alternateBases");
    chromosome = String.uppercase (ms "chromosome");
    position = (try int_of_string (ms "position") with _ -> invalid_arg "invalid position");
    reference = os "reference";
    dataset = os "dataset"
  }

(* serve /beacon/query. TODO rate limiting *)
let beacon_query data uri =
  try
    let beacon_req = parse_request uri
    let exists =
      Data.query data
        beacon_req.chromosome
        beacon_req.position
        beacon_req.alternate_bases
        ?reference_bases:(if beacon_req.reference_bases <> "" then Some beacon_req.reference_bases else None)
    let response =
      JSON.of_assoc [
        "exists", `String (match exists with `True -> "True" |
                                             `False -> "False" |
                                             `Null -> "Null" |
                                             `Overlap -> "Overlap")
      ]
    Lwt.return (`OK, response)
  with
    | Invalid_argument msg ->
        let body = JSON.of_assoc [
          "err", JSON.of_assoc [
            "name", `String "bad_request";
            "description", `String msg
          ]
        ]
        Lwt.return (`Bad_request, body)

(* serve /beacon/info *)
let beacon_info cfg =
  let response = JSON.of_assoc [
    "id", `String cfg.id;
    "organization", `String cfg.organization;
    "description", `String cfg.description;
    "api", `String "v0.2"
  ]
  Lwt.return (`OK, response)

(* route dispatcher *)
let dispatch cfg data conn req body =
  if Request.meth req <> `GET then
    Lwt.return (`Method_not_allowed, JSON.empty)
  else
    let uri = Request.uri req
    (* TODO ensure request has no body, kill connection o/w... *)
    match Uri.path uri with
      | "/beacon/query" -> beacon_query data uri
      | "/beacon/info" -> beacon_info cfg
      | _ -> Lwt.return (`Not_found, JSON.empty)

let pid = Unix.getpid ()
let host = Unix.gethostname ()
let timestamp () = int_of_float (Unix.gettimeofday() *. 1000.)
let client_ip (flow,_) = match flow with
  (* ref: https://github.com/mirage/ocaml-cohttp/commit/e015f79c89818f8bd598794f087b6c1df1fecc7e *)
  | Conduit_lwt_unix.TCP { fd } ->
      match Lwt_unix.getpeername fd with
        | Lwt_unix.ADDR_INET (ia,_) -> Ipaddr.to_string (Ipaddr_unix.of_inet_addr ia)
        | _ -> "unknown"
  | _ -> "unknown"

(* cohttp server. *)
let server cfg data =
  let req_counter = ref 0
  let callback conn req body =    
    (* log request *)
    let t0 = timestamp ()
    let req_id = !req_counter
    incr req_counter
    let reqlog = JSON.of_assoc [
      "time", `Int t0;
      "host", `String host;
      "pid", `Int pid;
      "rid", `Int req_id;
      "client", `String (client_ip conn);
      "method", `String (Code.string_of_method (Request.meth req));
      "path", `String (Uri.path_and_query (Request.uri req))
    ]
    (*printf "%s\n" (JSON.to_string reqlog)*)
    let backtrace = ref None

    (* dispatch request, catch and record any exceptions *)
    let%lwt status, body =
      try%lwt dispatch cfg data conn req body
      with exn ->
        backtrace := Some (Printexc.get_backtrace ())
        let body = JSON.of_assoc [
          "err", JSON.of_assoc [
            "name", `String "internal_server_error";
            "description", `String "internal server error"
          ]
        ]
        Lwt.return (`Internal_server_error, body)

    (* log response *)
    let reslog = List.fold_left ($+) reqlog [
      "dur",`Int (timestamp() - t0);
      "code", `Int (Code.code_of_status status);
      "response", body
    ]
    let reslog = if !backtrace = None then reslog else reslog $+ ("backtrace",`String (Option.get !backtrace))
    printf "%s\n" (JSON.to_string reslog)

    Server.respond_string ~status ~body:(JSON.to_string body) ()
  Server.create ~mode:(`TCP (`Port cfg.port)) (Server.make ~callback ())
