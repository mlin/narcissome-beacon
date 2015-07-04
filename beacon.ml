open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Batteries

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

(* cohttp server. TODO: request-response logging *)
let server cfg data =
  let callback conn req body =    
    let%lwt status, body =
      try%lwt dispatch cfg data conn req body
      with exn ->
        let body = JSON.of_assoc [
          "err", JSON.of_assoc [
            "name", `String "internal_server_error";
            "description", `String "internal server error"
          ]
        ]
        Lwt.return (`Internal_server_error, body)
    Server.respond_string ~status ~body:(JSON.to_string body) ()
  Server.create ~mode:(`TCP (`Port cfg.port)) (Server.make ~callback ())
