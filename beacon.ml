open Batteries
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Printf

Printexc.record_backtrace true

module Data = struct
  (* allele on a chromosome: (position,(alternateBases=*>referenceBases)) *)
  type allele = (int*((string,string) MultiPMap.t))
  (* map chromosome to sorted allele array. sorting by position facilitates overlap lookups *)
  type t = (string,allele array) Map.t

  let query ?reference_bases data chromosome position alternate_bases =
    try
      let alleles = Map.find chromosome data
      (* binary search the allele array *)
      let rec bs lo hi =
        if lo >= hi then `False
        else
          let mid = lo + (hi-lo)/2
          let allele_pos = fst alleles.(mid)
          if      allele_pos < position then bs (mid+1) hi
          else if allele_pos > position then bs lo mid
          else
            (* we found an allele entry at this position, so the answer will be either
               Overlap or True. (Currently we produce Overlap only for such exact
               position matches.) It's True if there's a matching alternateBases and
               (query referenceBases is unspecified, or query referenceBases is 
                specified and matching an entry in data). *)
            let allele_reference_bases = MultiPMap.find alternate_bases (snd alleles.(mid))
            if Set.PSet.is_empty allele_reference_bases then `Overlap
            else
              match reference_bases with
                | None -> `True
                | Some dna when Set.PSet.mem dna allele_reference_bases -> `True
                | _ -> `Overlap
      bs 0 (Array.length alleles)
    with Not_found -> `Null

  (* Load from a VCF stream. (only the first 5 columns are needed!) *)
  let load input =
    let data =
      IO.lines_of input |> fold
        fun d line ->
          try
            if String.length line = 0 || line.[0] = '#' then d
            else
              match String.nsplit line ~by:"\t" with
                | chromosome :: position :: _ :: reference_bases :: alternates :: _ ->
                    assert (chromosome <> "")
                    let position = int_of_string position - 1
                    try 
                      let _ = String.index reference_bases ','
                      failwith "multiple reference alleles!?"
                    with Not_found -> ()
                    let chrom_entry = try Map.find chromosome d with Not_found -> Map.empty

                    let pos_entry =
                      String.nsplit alternates ~by:"," |> List.fold_left
                        fun pos_entry alternate_bases ->
                          MultiPMap.add
                            String.uppercase alternate_bases
                            String.uppercase reference_bases
                            pos_entry
                        try Map.find position chrom_entry with Not_found -> MultiPMap.empty

                    Map.add chromosome (Map.add position pos_entry chrom_entry) d
                | _ -> failwith "invalid VCF line"
          with exn ->
            eprintf "%s\n" line
            raise exn
        Map.empty
    ((Map.map (Array.of_enum % Map.enum) data):t)

(* server configuration *)
type config = {
  port : int;
  id : string;
  organization : string;
  description : string;
  catchall : string option;
  qps : float;
  backlog : int
}

(* beacon query *)
type query = {
  reference_bases : string;
  alternate_bases : string;
  chromosome : string;
  position : int;
  reference : string;
  dataset : string
}

(* parse beacon query from URI query string *)
let parse_query uri =
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

let query_json q =
  let ans = JSON.of_assoc [
    "alternateBases", `String q.alternate_bases;
    "chromosome", `String q.chromosome;
    "position", `Int q.position
  ]
  let o j (key,str) = if str = "" then j else j $+ (key,`String str)
  List.fold_left o ans [
    ("referenceBases",q.reference_bases);
    ("reference",q.reference);
    ("dataset",q.dataset)
  ]

let query_rate_limiter = ref None

(* serve /beacon/query. *)
let beacon_query cfg data uri =
  try%lwt
    match !query_rate_limiter with
      | None when cfg.qps > 0. -> query_rate_limiter := Some (RateLimiter.create cfg.qps cfg.backlog)
      | _ -> ()
    let%lwt _ = match !query_rate_limiter with
      | Some rl -> RateLimiter.enter rl (* TODO: log the delay *)
      | None -> return 0.
    let beacon_req = parse_query uri
    let exists =
      Data.query data
        beacon_req.chromosome
        beacon_req.position
        beacon_req.alternate_bases
        ?reference_bases:(if beacon_req.reference_bases <> "" then Some beacon_req.reference_bases else None)
    let response = JSON.of_assoc [
      "beacon", `String cfg.id;
      "query", query_json beacon_req;
      "response", JSON.of_assoc [
        "exists", `String (match exists with `True -> "True" |
                                             `False -> "False" |
                                             `Null -> "Null" |
                                             `Overlap -> "Overlap")
      ]
    ]
    return (`OK, None, response)
  with
    | Invalid_argument msg ->
        let body = JSON.of_assoc [
          "beacon", `String cfg.id;
          "response", JSON.of_assoc [
            "err", JSON.of_assoc [
              "name", `String "bad_request";
              "description", `String msg
            ]
          ]
        ]
        return (`Bad_request, None, body)
    | RateLimiter.Maxed ->
        let body = JSON.of_assoc [
          "beacon", `String cfg.id;
          "response", JSON.of_assoc [
            "err", JSON.of_assoc [
              "name", `String "too_many_requests";
              "description", `String "too many requests"
            ]
          ]
        ]
        return (`Too_many_requests, None, body)

(* serve /beacon/info *)
let beacon_info_json cfg = JSON.of_assoc [
  "id", `String cfg.id;
  "organization", `String cfg.organization;
  "description", `String cfg.description;
  "api", `String "v0.2"
]
let beacon_info cfg =
  return (`OK, None, beacon_info_json cfg)

(* route dispatcher *)
let dispatch cfg data conn req body =
  if Request.meth req <> `GET then
    return (`Method_not_allowed, None, JSON.empty)
  else
    let uri = Request.uri req
    (* TODO ensure request has no body, kill connection o/w... *)
    match Uri.path uri with
      | "/beacon/query" -> beacon_query cfg data uri
      | "/beacon/info" -> beacon_info cfg
      | _ ->
          let status, headers = 
            if cfg.catchall = None then `Not_found, None
            else `See_other, Some (Header.init_with "location" (Option.get cfg.catchall))
          return
            status, headers, JSON.of_assoc [
              "beacon", `String cfg.id;
              "response", JSON.of_assoc [
              "err", JSON.of_assoc [
                "name", `String "not_found";
                "description", `String "not found"
              ]
            ]]

let pid = Unix.getpid ()
let host = Unix.gethostname ()
let timestamp () = int_of_float (Unix.gettimeofday() *. 1000.)
let client_ip (flow,_) = match flow with
  (* ref: https://github.com/mirage/ocaml-cohttp/commit/e015f79c89818f8bd598794f087b6c1df1fecc7e *)
  | Conduit_lwt_unix.TCP { Conduit_lwt_unix.fd } ->
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
    let%lwt status, headers, body =
      try%lwt dispatch cfg data conn req body
      with exn ->
        backtrace := Some (sprintf "%s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ()))
        let body = JSON.of_assoc [
          "beacon", `String cfg.id;
          "response", JSON.of_assoc [
            "err", JSON.of_assoc [
              "name", `String "internal_server_error";
              "description", `String "internal server error"
            ]
          ]
        ]
        return (`Internal_server_error, None, body)

    (* log response *)
    let reslog = List.fold_left ($+) reqlog [
      "dur",`Int (timestamp() - t0);
      "code", `Int (Code.code_of_status status);
      "response", body
    ]
    let reslog = if !backtrace = None then reslog else reslog $+ ("backtrace",`String (Option.get !backtrace))
    printf "%s\n" (JSON.to_string reslog); flush stdout

    Server.respond_string ?headers ~status ~body:(JSON.to_string body) ()

  let total_variants = Map.fold (fun ar c -> c + Array.length ar) data 0
  let startup_msg = JSON.of_assoc [
    "time", `Int (timestamp ());
    "host", `String host;
    "pid", `Int pid;
    "qps", `Float cfg.qps;
    "backlog", `Int cfg.backlog;
    "info", beacon_info_json cfg;
    "variant_counts", (`Object (Map.map (fun ar -> `Int (Array.length ar)) data)) $+ ("*",`Int total_variants)
  ]
  printf "%s\n" (JSON.to_string startup_msg); flush stdout
  Server.create ~mode:(`TCP (`Port cfg.port)) (Server.make ~callback ())
