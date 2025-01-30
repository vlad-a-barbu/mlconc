open Effect
open Effect.Deep

type _ Effect.t +=
  | Fork : (unit -> unit) -> unit Effect.t
  | Yield : unit Effect.t

let yield () = perform Yield
let fork fn = perform @@ Fork fn

let run main =
  let q = Queue.create () in
  let enqueue k = Queue.push (fun () -> continue k ()) q in
  let dequeue () = if Queue.is_empty q then () else Queue.pop q () in
  let rec step fn =
    match fn () with
    | () -> dequeue ()
    | exception exn ->
        Printexc.to_string exn |> print_endline;
        dequeue ()
    | effect Yield, k ->
        enqueue k;
        dequeue ()
    | effect Fork fn, k ->
        enqueue k;
        step fn
  in
  step main

let countdown n =
  let rec go n =
    if n = 0 then ()
    else (
      Printf.printf "%d\n%!" n;
      Unix.sleepf 0.5;
      yield ();
      go (n - 1))
  in
  fun () -> go n

let main () =
  fork @@ countdown 3;
  fork @@ countdown 3

let () = run main
