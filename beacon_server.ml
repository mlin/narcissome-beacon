open Printf
open Lwt
open Batteries

let cfg = {
	Beacon.port = 8000;
	id = "test";
	organization = "test";
	description = "test"
}

ignore (Lwt_main.run (Beacon.server cfg Map.empty))
