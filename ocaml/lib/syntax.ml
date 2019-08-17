open Abs

let var =
  let i = ref 0 in
  fun () ->
    let () = incr i in
    Printf.sprintf ("x%d") !i

(* for accessing affine variable *)
let (!) v = VarA v

(* let binding *)
let (let* ) e k =
  let v = var () in
  Let(v, e, k @@ Var v)

(* affine let binding *)
let (let*! ) e k =
  let v = var () in
  LetA(v, e, k v)

(* lambda *)
let lam f =
  let v = var () in
  Lam(v, f @@ Var v)

(* affine lambda *)
let lama f =
  let v = var () in
  LamA(v, f v)

(* app *)
let ( @ ) f x = App(f, x)

(* affine app *)
let ( @! ) f x = AppA(f, x)

let ( |@> ) = Fun.flip ( @ )

let atom a = Atom a

