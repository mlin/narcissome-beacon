open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix
open JSON.Operators
open Should
open Batteries

Printexc.record_backtrace true

let (@) = JSON.Operators.($)

let test_config = {
  Beacon.port = 8222
}

let test_data = [
  "12", [| (112241765,"A"), Set.of_enum (List.enum ["G"]) |]
]

let get path = Client.get (Uri.of_string (sprintf "http://localhost:%d%s" test_config.port path))
let getbody path =
  let%lwt (response,body) = get path
  let%lwt body = Cohttp_lwt_body.to_string body
  Lwt.return (response,JSON.from_string body)

let tests =
  (* start the server *)
  Lwt.async (fun _ -> Beacon.server test_config (Map.of_enum (List.enum test_data)))
  let%lwt _ = Lwt_unix.sleep 0.1

  let%lwt (response,body) = get "/beacon"
  Response.status response $hould # equal `Not_found

  let%lwt (response,body) = get "/beacon/query"
  Response.status response $hould # equal `Bad_request

  let%lwt (response,body) = getbody "/beacon/query?chromosome=12&position=112241765&alternateBases=A"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "True")

  let%lwt (response,body) = getbody "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=G"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "True")

  let%lwt (response,body) = getbody "/beacon/query?chromosome=12&position=112241765&alternateBases=A&referenceBases=T"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "False")

  let%lwt (response,body) = getbody "/beacon/query?chromosome=12&position=112241765&alternateBases=T"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "Null")

  let%lwt (response,body) = getbody "/beacon/query?chromosome=12&position=12345678&alternateBases=A"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "Null")

  let%lwt (response,body) = getbody "/beacon/query?chromosome=10&position=112241765&alternateBases=A"
  Response.status response $hould # equal `OK
  (body@"exists") $hould # equal (`String "Null")

  Lwt.return ()

Lwt_main.run tests
