open Syntax
type env = (string * value) list
and value =
  | Closure of var * expr * env
  | Constant of constant * value list
type result = Error | Value of value

module type BigStepEval = sig val eval : env -> expr -> result end

module BigStep : BigStepEval = struct
  let val_int u =
    let const = { name = Int u; constr = true; arity = 0 }
    in Value (Constant (const, []))

  (* define primitive functions *)
  let delta c l =
    match c.name, l with
    | Name "+", [ Constant ({ name = Int u; _ }, []);
                  Constant ({ name = Int v; _ }, [])
                ] -> val_int (u + v)

    | Name "*", [ Constant ({ name = Int u; _ }, []);
                  Constant ({ name = Int v; _ }, [])
                ] -> val_int (u * v)
    | _ -> Error

  let get x env = try Value (List.assoc x env) with Not_found -> Error

  let rec eval env = function
    | Var x -> get x env
    | Const c -> Value (Constant (c, []))
    | Fun (x, e) -> Value (Closure (x, e, env))
    | Let (x, e1, e2) ->
        begin match eval env e1 with
        | Value v -> eval ((x, v)::env) e2
        | Error -> Error
        end
    | App (e1, e2) ->
        begin match eval env e1, eval env e2 with
        | Value (Closure (x, e, env)), Value v -> eval ((x, v)::env) e
        | Value (Constant (c, l)), Value v ->
            let k = List.length l + 1 in
            if c.arity < k then Error
            else if c.arity > k || c.constr then Value (Constant (c, v::l))
            else delta c (v::l)
        | _, _ -> Error
        end
end
