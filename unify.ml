type type_symbol = Tarrow | Tint
type texpr = { mutable texpr : node; mutable mark : int }
and node = Desc of desc | Link of texpr
and desc = Tvar of int | Tcon of type_symbol * texpr list

(* for building type representation *)
module TBuilder = struct

  module S = struct
    let count = ref 0
    let var () = begin
      incr count;
      ref (Desc (Tvar !count))
    end
    let last_mark = ref 0
    let marker () = incr last_mark; !last_mark
  end

  let expr d = { texpr = Desc d; mark = 0; }
  let int = expr (Tcon (Tint, []))
  let arrow t1 t2 = expr (Tcon (Tarrow, [t1; t2]))

  let rec repr t =
    match t.texpr with
    | Link u -> let v = repr u in t.texpr <- Link v; v
    | Desc _ -> t

  let desc t =
    match (repr t).texpr with
    | Link _ -> assert false
    | Desc d -> d
end

exception Unify of texpr * texpr
exception Arity of texpr * texpr

open TBuilder

let link t1 t2 = (repr t1).texpr <- Link t2

let rec unify t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 == t2 then () else
  match desc t1, desc t2 with
  | Tvar _, _ -> link t1 t2
  | _, Tvar _ -> link t2 t1
  | Tcon (g1, l1), Tcon (g2, l2) when g1 = g2 ->
      begin
        link t1 t2;
        List.iter2 unify l1 l2
      end
  | _ , _  -> raise (Unify (t1, t2))

exception Cycle of texpr list
let acyclic t =
  let visiting = S.marker () and visited = S.marker () in
  let cycles = ref [] in
  let rec visit t =
    let t = repr t in
      if t.mark > visiting then ()
      else if t.mark = visiting then cycles := t :: !cycles
      else begin
        t.mark <- visiting;
        begin match desc t with
        | Tvar _ -> ()
        | Tcon (_, l) -> List.iter visit l
        end;
        t.mark <- visited;
      end in
  visit t;
  if !cycles <> [] then raise (Cycle !cycles)

let funify t1 t2 = unify t1 t2; acyclic t1
