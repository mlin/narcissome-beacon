open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Batteries

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
        if lo >= hi then `Null
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
  port : int
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

(* serve /beacon/query *)
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
    Server.respond_string ~status:`OK ~body:(JSON.to_string response) ()
  with
    | Invalid_argument msg ->
        let body = JSON.of_assoc [
          "err", JSON.of_assoc [
            "name", `String "bad_request";
            "description", `String msg
          ]
        ]
        Server.respond_string ~status:`Bad_request ~body:(JSON.to_string body) ()

(* cohttp server *)
let server { port } data =
  let callback conn req body =
    let uri = Request.uri req
    if Request.meth req <> `GET then
      Server.respond_string ~status:`Method_not_allowed ~body:"" ()
    (* TODO check request body size, kill connection if too large... *)
    else
      try
        match Uri.path uri with
          | "/beacon/query" -> beacon_query data uri
          (* TODO /beacon/info *)
          | _ -> Server.respond_string ~status:`Not_found ~body:"" ()
      with exn ->
        (* TODO log exn *)
        let body = JSON.of_assoc [
          "err", JSON.of_assoc [
            "name", `String "internal_server_error";
            "description", `String "internal server error"
          ]
        ]
        Server.respond_string ~status:`Internal_server_error ~body:(JSON.to_string body) ()
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

(*
    let headers = req |> Request.headers |> Header.to_string
    let%lwt body = Cohttp_lwt_body.to_string body
*)
