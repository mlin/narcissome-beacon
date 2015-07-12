open Batteries
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Printf

Printexc.record_backtrace true

module Data = struct
  (* allele on a chromosome: (position,alternateBases,referenceBases) *)
  type allele = int*string*string
  let allele_pos (pos,_,_) = pos

  (* map chromosome to buckets of alleles. each bucket has [lo,hi) coordinates,
     the number of alleles in the bucket, and a Snappy-compressed, marshalled,
     sorted (allele array). The compression scheme reduces this sucker's memory
     usage, so that I can run it on my micro instance. *)
  type t = (string,(int*int*int*string) array) Map.t

  let pickle x = Snappy.compress (Marshal.to_bytes x [])
  let unpickle buf = Marshal.from_bytes (Snappy.uncompress buf) 0

  (* find & unpickle the bucket containing the given position *)
  let get_bucket data chromosome position =
    let alleles = Map.find chromosome data
    (* TODO: alleles is a sorted array - do a binary search *)
    let (_,_,n_alleles,buf) = alleles |> Array.find (fun (lo,hi,_,_) -> position >= lo && position < hi)
    let ans = (unpickle buf:allele array)
    assert (Array.length ans = n_alleles)
    ans

  (* serve a query, producing True, Overlap, False, or Null *)
  let query ?reference_bases data chromosome position alternate_bases =
    if not (Map.mem chromosome data) then `Null
    else
      try
        (* get the alleles array from the pertinent bucket *)
        let alleles = get_bucket data chromosome position

        (* binary search for the lowest index with the desired position *)
        let rec bs lo hi =
          if lo >= hi then raise Not_found
          else
            let mid = lo + (hi-lo)/2
            let mid_pos = allele_pos alleles.(mid)
            if      mid_pos < position then bs (mid+1) hi
            else if mid_pos > position || (mid>0 && mid_pos = allele_pos alleles.(mid-1)) then bs lo mid
            else mid

        (* we found an allele entry at this position, so the answer will be either
           Overlap or True. (Currently we produce Overlap only for such exact
           position matches.) It's True if there's a matching alternateBases and
           (query referenceBases is unspecified, or query referenceBases is 
            specified and matching an entry in data). *)

        (* linear search for a matching allele starting from the aforementioned index *)
        let rec ls i =
          if i >= Array.length alleles || allele_pos alleles.(i) <> position then `Overlap
          else
            let _, allele_alternate_bases, allele_reference_bases = alleles.(i)
            if (alternate_bases = allele_alternate_bases &&
                (reference_bases = None || Option.get reference_bases = allele_reference_bases)) then
              `True
            else ls (i+1)

        ls (bs 0 (Array.length alleles))
      with Not_found -> `False

  let stride_enum n en =
    Enum.from
      fun () ->
        let ans = Enum.take n en
        if Enum.is_empty ans then raise Enum.No_more_elements else ans

  (* Load data from a VCF stream. (only the first 5 columns are needed!) *)
  let load variants_per_bucket input =
    (* parse vcf *)
    let vcf =
      (IO.lines_of input
        |> Enum.filter (fun line -> String.length line > 0 && line.[0] <> '#')
        |> Enum.map
            (fun line ->
              try
                (match String.nsplit line ~by:"\t" with
                  | chromosome :: position :: _ :: reference_bases :: alternates :: _ ->
                      chromosome, (int_of_string position - 1), reference_bases, alternates
                  | _ -> failwith "")
              with _ -> failwith ("Invalid VCF line: " ^ line)))

    (* create one serialized bucket from an enumeration of sorted VCF lines from the same chromosome *)
    let bucketize vcf_lines =
      let last_pos = ref (-1)
      let alleles =
        vcf_lines |> Enum.map
          fun (_,pos,reference_bases,alternates) ->
            if (pos < !last_pos) then
              failwith "VCF not sorted properly!"
            last_pos := pos
            List.enum
              String.nsplit alternates ~by:"," |> List.map
                fun alternate_bases -> (pos,String.uppercase alternate_bases,String.uppercase reference_bases)
      let alleles = Enum.flatten alleles |> Array.of_enum
      let n_alleles = Array.length alleles
      let bucket_lo = match alleles.(0) with (pos,_,_) -> pos
      let bucket_hi = match alleles.(n_alleles - 1) with (pos,_,_) -> pos+1
      bucket_lo, bucket_hi, n_alleles, pickle alleles

    (* make buckets from all the sorted VCF lines on one chromosome *)
    let chromosome_buckets vcf_lines =
      (vcf_lines
        |> Enum.group (fun (_,pos,_,_) -> pos)
        |> stride_enum variants_per_bucket
        |> Enum.map Enum.flatten
        |> fun en -> let chrom,_,_,_ = Option.(get (Enum.peek (get (Enum.peek en)))) in chrom, Array.of_enum (Enum.map bucketize en))

    (* group the VCF lines by chromosome, and make the buckets for each group *)
    (* TODO: ensure all lines for each chromosome are contiguous... *)
    (Enum.group_by (fun (chrom1,_,_,_) (chrom2,_,_,_) -> chrom1 = chrom2) vcf
      |> Enum.map chromosome_buckets
      |> Map.of_enum)

(* server configuration *)
type config = {
  localhost : bool;
  port : int;
  id : string;
  organization : string;
  description : string;
  reference : string Set.t;
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
    if beacon_req.dataset <> "" then invalid_arg "This beacon serves only the default dataset."
    if beacon_req.reference <> "" && not (Set.mem beacon_req.reference cfg.reference) then
      invalid_arg "unknown reference"
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
let server cfg (data:Data.t) =
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
    let headers = Header.add (Option.default (Header.init ()) headers) "content-type" "application/json"

    (* log response *)
    let reslog = List.fold_left ($+) reqlog [
      "dur",`Int (timestamp() - t0);
      "code", `Int (Code.code_of_status status)
    ]
    let reslog = if !backtrace = None then reslog else reslog $+ ("backtrace",`String (Option.get !backtrace))
    printf "%s\n" (JSON.to_string reslog); flush stdout

    Server.respond_string ~headers ~status ~body:(JSON.to_string body ^ "\n") ()

  let num_variants_by_chrom = data |> Map.map (Array.fold_left (fun c (_,_,len,_) -> c + len) 0)
  let total_variants = Map.fold (+) num_variants_by_chrom 0
  let db_compressed_bytes = data |> Map.map (Array.fold_left (fun c (_,_,_,str) -> c + String.length str) 0) |> (fun mp -> Map.fold (+) mp 0)
  let startup_msg = JSON.of_assoc [
    "time", `Int (timestamp ());
    "host", `String host;
    "pid", `Int pid;
    "qps", `Float cfg.qps;
    "backlog", `Int cfg.backlog;
    "info", beacon_info_json cfg;
    "variant_counts", (`Object (Map.map (fun n -> `Int n) num_variants_by_chrom)) $+ ("*",`Int total_variants);
    "db_compressed_bytes", `Int db_compressed_bytes
  ]
  printf "%s\n" (JSON.to_string startup_msg); flush stdout

  let%lwt ctx =
    if cfg.localhost then
      let%lwt ctx' = Conduit_lwt_unix.init ~src:"127.0.0.1" ()
      Lwt.return (Some (Cohttp_lwt_unix_net.init ~ctx:ctx' ()))
    else
      Lwt.return None

  Server.create
    ~timeout:(int_of_float (ceil (1.5 *. float cfg.backlog /. cfg.qps)))
    ~mode:(`TCP (`Port cfg.port))
    ?ctx
    Server.make ~callback ()
