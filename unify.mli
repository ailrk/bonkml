type type_symbol = Tarrow | Tint
type texpr = { mutable texpr : node; mutable mark : int }
and node = Desc of desc | Link of texpr
and desc = Tvar of int | Tcon of type_symbol * texpr list

module TBuilder : sig
  val expr : desc -> texpr
  val int : texpr
  val arrow : texpr -> texpr -> texpr
  val repr : texpr -> texpr
  val desc : texpr -> desc
end

exception Unify of texpr * texpr
exception Arity of texpr * texpr

val unify : texpr -> texpr -> unit
