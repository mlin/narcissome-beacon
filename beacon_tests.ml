open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Should
open Batteries

let (@) = JSON.Operators.($)

let test_config = {
  Beacon.port = 8222;
  id = "test";
  organization = "test";
  description = "test"
}

let test_data = [
  "12", [| 112241765, MultiPMap.of_enum (List.enum ["A","G"]) |];
  "1", [| 123456789, MultiPMap.of_enum (List.enum ["G","A";"G","AA";"GG","A";"GG","AA"]) |];
]

let get path = Client.get (Uri.of_string (sprintf "http://localhost:%d%s" test_config.port path))
let getbody path =
  let%lwt (response,body) = get path
  let%lwt body = Cohttp_lwt_body.to_string body
  Lwt.return (response,JSON.from_string body)

let ok path exists =
  let%lwt (response,body) = getbody path
  Response.status response $hould # equal `OK
  ((body@"response")@"exists") $hould # equal (`String exists)
  Lwt.return ()

let tests =
  (* start the server *)
  Lwt.async (fun _ -> Beacon.server test_config (Map.of_enum (List.enum test_data)))
  let%lwt _ = Lwt_unix.sleep 0.1

  let%lwt (response,body) = get "/beacon"
  Response.status response $hould # equal `Not_found

  let%lwt (response,body) = getbody "/beacon/info"
  Response.status response $hould # equal `OK
  printf "%s\n" (JSON.to_string body)

  let%lwt (response,body) = get "/beacon/query"
  Response.status response $hould # equal `Bad_request

  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A" "True"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=G" "True"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=T" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=112241765&alternateBases=T" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=12&position=12345678&alternateBases=A" "False"
  let%lwt _ = ok "/beacon/query?chromosome=10&position=112241765&alternateBases=A" "Null"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=123456789&alternateBases=G" "True"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=123456789&alternateBases=C" "Overlap"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=123456789&alternateBases=G&referenceBases=AA" "True"
  let%lwt _ = ok "/beacon/query?chromosome=1&position=123456789&alternateBases=G&referenceBases=T" "Overlap"

  Lwt.return ()

Lwt_main.run tests
