open Syntax
exception Reduce

let rec evaluated = function
  | Fun _ -> true
  | c -> partial_apply 0 c
and partial_apply n = function
  | Const c -> (c.constr || c.arity > n)
  | App (f, v) -> evaluated v && partial_apply (n + 1) f
  | _ -> false

let union f g a = try g a with Reduce -> f a

let delta =
  let plus = Const { name = Name "+"; arity = 2; constr = false }, ( + ) in
  let times = Const { name = Name "*"; arity = 2; constr = false }, ( * ) in
  let int n = Const { name = Int n; arity = 0; constr = true } in
  let binop (op, opfunc) = function
    | App (App (Const { name = Name _; arity = 2; _ } as c,
                Const { name = Int u; _}),
           Const { name = Int v; _}) when c == op -> int (opfunc u v)
    | _ -> raise Reduce in
  let rules = List.map binop [plus; times] in
  List.fold_right union rules (fun _ -> raise Reduce)

let rec subst x v e = (* subst e with v *)
  assert (evaluated e);
  match e with
  | Var y -> if y = x then v else e
  | Const _ as c -> c
  | Fun (y, e1) -> if x = y then e else Fun (y, subst y v e1)
  | App (e1, e2) -> App (subst x v e1,  subst x v e2)
  | Let (y, e1, e2) -> if x = y then Let (y, subst y v e1, e2)
                                else Let (y, subst y v e1, subst y v e2)

let beta = function
  | App (Fun (x, e), v) when evaluated v -> subst x v e
  | Let (x, v, e) when evaluated v -> subst x v e
  | _ -> raise Reduce

let reduce = List.fold_right union [delta; beta] (fun _ -> raise Reduce)
