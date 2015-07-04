open Printf
open Lwt
open Batteries

ignore (Lwt_main.run (Beacon.server { Beacon.port = 8000 } Map.empty))
