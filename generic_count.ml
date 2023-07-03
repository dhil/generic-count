(* Generic search, or rather generic count, signature. *)
module type GENERIC_SEARCH = sig
  type point = int -> bool
  type predicate = point -> bool

  val count : predicate -> int
end

(* An implementation based on shift/reset. *)
module Shift_Search: GENERIC_SEARCH = struct
  type point = int -> bool
  type predicate = point -> bool

  let count pred =
    let open Delimcc_of_fxhandler in
    let branch p _ =
      shift p (fun k ->
          k true + k false)
    in
    reset (fun p ->
         if pred (branch p) then 1 else 0)

end

(* An implementation based on effect handlers. *)
module Handler_Search: GENERIC_SEARCH = struct
  type point = int -> bool
  type predicate = point -> bool

  type _ Effect.t += Branch : int -> bool Effect.t

  let branch i = Effect.perform (Branch i)

  let hcount : (bool, int) Effect.Deep.handler =
    let open Effect.Deep in
    { retc = (fun ans ->
        if ans then 1 else 0)
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Branch _ ->
         Some (fun (k : (a, int) continuation) ->
             let open Multicont.Deep in
             let r = promote k in
             let x = resume r true in
             let y = resume r false in
             x + y)
      | _ -> None) }

  let count pred =
    Effect.Deep.match_with pred branch hcount
end

(* An implementation of callcc in terms of effect handlers. *)
module Callcc = struct
    open Effect.Deep

    type _ Effect.t += Callcc : (('a -> 'b) -> 'a) -> 'a Effect.t

    let rec hcallcc =
      { retc = (fun x -> x)
      ; exnc = raise
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Callcc f ->
           Some (fun (k : (a, _) continuation) ->
               let open Multicont.Deep in
               let r = promote k in
               let exception Throw of a in
               let exception Done of a in
               let rec handle_throw r f =
                 try f () with
                 | Throw x -> handle_throw r (fun () -> resume r x)
                 | Done x -> resume r x
               in
               handle_throw r (fun () ->
                   toplevel (fun () ->
                       let ans = f (fun x -> raise (Throw x)) in
                       raise (Done ans))))
        | _ -> None) }
    and toplevel f = (* to be inserted at the beginning of the program *)
      Effect.Deep.match_with f () hcallcc

    let callcc : 'a 'b. (('a -> 'b) -> 'a) -> 'a
      = fun f -> Effect.perform (Callcc f)
end

(* An implementation based on callcc + state *)
module Callcc_Search : GENERIC_SEARCH = struct
  type point = int -> bool
  type predicate = point -> bool

  let count pred =
    let open Callcc in
    (* NOTE: The Sys.opaque_identity is a trick to prevent the
       compiler from performing the heap-to-stack conversion
       optimisation, which is invalid in the presence of multi-shot
       continuations. *)
    let result = Sys.opaque_identity (ref 0) in
    let inner = Sys.opaque_identity (ref (fun _x -> false)) in
    let pop k = inner := k in
    let push k =
      (* Memoise the previous continuation function. *)
      let prev = !inner in
      (* Override the continuation function to be a wrapper around the
         current continuation `k`. *)
      inner := (fun x -> (* the following trick realises the "back up"
                            behaviour of multi-shot delimited
                            continuations. It restores the previous
                            continuation function such the rest of the
                            program sees it after the exploration of
                            the false branch using the current
                            continuation. *)
        if x then k x else (pop prev; k x))
    in
    let () =
      if pred (fun _i ->
          callcc (fun k ->
              push k;
              !inner true))
      then incr result else ()
    in
    ignore (!inner false);
    !result
end

(* Examples *)
let tt0 _ = true

let tt1 q =
  let _ = q 1 in
  let _ = q 0 in
  true

let tt2 q =
  let _ = q 0 in
  let _ = q 0 in
  true

let i0 q = q 0

let i1 q = if q 0 then true else false

let i2 q =
  if q 0
  then q 0
  else false

let odd n q =
  let xor x y =
    match (x, y) with
    | (true, false) | (false, true) -> true
    | _ -> false
  in
  List.fold_left xor false (List.map q (List.init n (fun i -> i)))

(* Test runner *)
let test_all () =
  let tests : ((int -> bool) -> bool) list =
    [ tt0; tt1; tt2; i0; i1; i2; odd 2; odd 4 ]
  in
  let searchers : (module GENERIC_SEARCH) list =
    [ (module Shift_Search); (module Handler_Search); (module Callcc_Search) ]
  in
  let expected =
    [ 1; 4; 4; 1; 1; 1; 2; 8 ]
  in
  let actuals =
    List.map (fun (module M : GENERIC_SEARCH) ->
        Callcc.toplevel (* this prompt is necessary to delimit the effects of the Callcc implementation. *)
          (fun () -> List.map M.count tests))
      searchers
  in
  let string_of_int_list xs =
    (List.fold_left
       (fun acc x ->
         if acc = "["
         then Printf.sprintf "%s%d" acc x
         else Printf.sprintf "%s; %d" acc x)
       "[" xs) ^ "]"
  in
  List.iteri (fun n actual ->
      try
        assert (actual = expected)
      with Assert_failure _ ->
        Printf.fprintf stderr "Test %d failed!\n" n;
        Printf.fprintf stderr "Expected: %s\n" (string_of_int_list expected);
        Printf.fprintf stderr "Actual  : %s\n%!" (string_of_int_list actual);
    ) actuals

let _ = test_all ()

