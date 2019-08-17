open Abs

type model = {
  comp : comp;
  env : env;
  affenv : affenv;
  k : f list
}

let mod_model =
  let rec go ({ comp; env; affenv; k } as model) = function
    | [] -> model
    | (`Comp comp) :: mods -> go { comp; env; affenv; k }  mods
    | (`Env env) :: mods -> go { comp; env; affenv; k } mods
    | `Affenv affenv  :: mods -> go { comp; env; affenv; k } mods
    | `K k :: mods -> go { comp; env; affenv; k } mods
  in
  Fun.flip go

let lookup = List.assoc

let drop key env =
  let rec go acc = function
    | [] -> acc
    | (key', _) :: lst when key = key' -> lst
    | e :: lst -> go (e :: acc) lst
  in go [] env |> List.rev

let step ({comp; env; affenv; k} as model) =
  model |> match (comp, k) with
  | E(Var x), _ ->
    mod_model [`Comp (W (lookup x env))]
  | E(VarA x), _ ->
    mod_model [`Comp (W (lookup x affenv)); `Affenv (drop x affenv)]
  | E(Let(x, bindee, body)), _ ->
    mod_model [`Comp (E bindee); `K (LetF(x, body, env) :: k)]
  | E(LetA(x, bindee, body)), _ ->
    mod_model [`Comp (E bindee); `K (LetAF(x, body, env, affenv) :: k)]
  | W w, LetF(x, e', env') :: k' ->
    mod_model [`Comp (E e'); `Env ((x, w) :: env'); `K k']
  | W w, LetAF(x, e', env', affenv') :: k' ->
    mod_model [`Comp (E e'); `Env env'; `Affenv ((x, w) :: affenv'); `K k']
  | E(Lam(x, e)), _ ->
    mod_model [`Comp (W (Clos(x, e, env)))]
  | E(LamA(x, e)), _ ->
    mod_model [`Comp (W (ClosA(x, e, env, affenv)))]
  | E(App(e, e')), _ ->
    mod_model [`Comp (E e); `K (AppCF(e', env) :: k)]
  | E(AppA(e, e')), _ ->
    mod_model [`Comp (E e); `K (AppACF(e', env, affenv) :: k)]
  | W w, AppCF(e', env') :: k ->
    mod_model [`Comp (E e'); `Env env'; `K (AppF w ::k)]
  | W w, AppACF(e', env', affenv') :: k ->
    mod_model [`Comp (E e'); `Env env'; `Affenv affenv'; `K (AppF w ::k)]
  | W w, AppF(Clos(x, e, env)) :: k' ->
    mod_model [`Comp (E e); `Env ((x, w) :: env); `K k']
  | W w, AppAF(ClosA(x, e, env', affenv')) :: k' ->
    mod_model [`Comp (E e); `Env ((x, w) :: env'); `Affenv affenv'; `K k']
  | E((Atom _) as a), _ ->
    mod_model [`Comp ( W(AtomW a))]
  | W _, _ -> Fun.id

let eval e =
  let rec go m =
    let m' = step m in
    (* 🤔 *)
    if m <> m' then go m'
    else m
  in
  let model = { comp = E e; env = []; affenv = []; k = [] } in
  let { comp; env = _; affenv = _; k = _ } =  go model in
  match comp with
  | E _ -> assert false
  | W w -> w

