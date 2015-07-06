open Lwt

type t = {
  qps : float;
  max_backlog : int;
  mutable last : float;
  mutable backlog : int
}

let create qps max_backlog = {
  qps; max_backlog;
  last = 0.0; backlog = 0
}

exception Maxed
let enter rl =
  let t = Unix.gettimeofday ()
  if t -. rl.last > (1.0 /. rl.qps) then
    (* idle for long enough to release the thread immediately *)
    rl.last <- t
    return 0.
  else if rl.backlog < rl.max_backlog then
    (* delay the thread for a period of time proportional to the backlog size *)
    rl.backlog <- rl.backlog + 1
    let%lwt _ = Lwt_unix.sleep (float rl.backlog /. rl.qps)
    rl.backlog <- rl.backlog - 1
    let t' = Unix.gettimeofday ()
    rl.last <- t'
    return (t' -. t)
  else
    (* backlog is maxed out *)
    Lwt.fail Maxed

