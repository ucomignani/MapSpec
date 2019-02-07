open Core.Std
       
module RelationSymbol : sig
  type t
  val fromString : string -> t
  val toString : t -> string
  val print : t -> unit
  val compare: t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module Variable : sig
  type t
  val print : t -> unit
  val fromString : string -> t
  val toString : t -> string
  val compare: t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module VariableS : Set.S with type Elt.t = Variable.t
