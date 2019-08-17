type atom = int
type variable = string

type t =
  | Atom of atom
  | Var  of variable
  | VarA of variable
  | Lam  of variable * t
  | LamA of variable * t
  | Let  of variable * t * t
  | LetA of variable * t * t
  | App  of t * t
  | AppA of t * t

type w =
  | AtomW of t
  | Clos  of variable * t * env
  | ClosA of variable * t * env * affenv
and comp = E of t | W of w
and env = (variable * w) list
and affenv = (variable * w) list

type f =
  (* ([] e, E) *)
  | AppCF of t * env
  (* ([] @ e, E) *)
  | AppACF of t * env * affenv
  (* w [] *)
  | AppF of w
  (* w @ [] *)
  | AppAF of w
  | LetF of variable * t * env
  | LetAF of variable * t * env * affenv

let show_w : w -> string = function
  | AtomW t ->
    begin match t with
      | Atom a -> string_of_int a
      | _ -> assert false
    end
  | Clos (x, _e, _) ->
    Printf.sprintf "fun %s -> abstr" x
  | ClosA (x, _e, _, _) ->
    Printf.sprintf "!fun %s -> abstr" x
