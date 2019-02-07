open Core.Std

open GeneralStruct
       
module Atom : sig
  type t
  val getRelationSymbol : t -> RelationSymbol.t
  val getVarsList : t -> Variable.t list
  val create : RelationSymbol.t -> Variable.t list -> t
  val rename : t -> ?suffix:string -> t
  val toString : t -> string
  val print : t -> unit
  val vars : t -> VariableS.t
  val varsNumber : t -> int
  val varOccurenceNumber : t -> Variable.t -> int
  val hasVar : t -> Variable.t -> bool
  val renameVar : t -> Variable.t -> Variable.t -> t
  val randomlyReplaceAVariableByAnother : t -> Variable.t -> t
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module AtomS : Set.S with type Elt.t = Atom.t
                
module AtomConjunction : sig
  type t
  val toString : t -> string
  val print : t -> unit
  val println : t -> unit
  val vars : t -> VariableS.t
  val varOccurenceNumber : t -> Variable.t -> int
  val chooseRandomAtom : t -> Atom.t
  val add : t -> Atom.t -> t
  val remove : t -> Atom.t -> t
  val renameAtoms : t -> ?suffix:string -> t
  val empty : t
  val isEmpty : t -> bool
  val containVars : t -> VariableS.t -> bool
  val renameVar : t -> Variable.t -> Variable.t -> t
  val dropAtom : t -> Atom.t -> t
  val length : t -> int
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val symmetric_difference : t -> t -> t
  val isASubsetOf : t -> t -> bool
  val map : t -> f:(Atom.t -> Atom.t) -> t
  val fold : t ->  init:'accum -> f:('accum -> Atom.t ->'accum) -> 'accum
  val iter : t -> f:(Atom.t -> unit) -> unit
  val filter : t -> f:(Atom.t -> bool) -> t
  val exists : t -> f:(Atom.t -> bool) -> bool
  val for_all : t -> f:(Atom.t -> bool) -> bool
  val choose_exn : t -> Atom.t
  val of_list : Atom.t list -> t
  val to_list : t -> Atom.t list
end

module AtomConjunctionS : Set.S with type Elt.t = AtomConjunction.t
                           
module Tgd : sig
  type t
  val create : AtomConjunction.t -> AtomConjunction.t -> t
  val print : t -> unit
  val printAsInstancesPair : t -> unit
  val toString : t -> string
  val toStringAsInstancesPair : t -> string
  val extractExistentiallyQuantifiedVariables : t ->  VariableS.t
  val lhs : t -> AtomConjunction.t
  val rhs : t -> AtomConjunction.t
  val invertSourceAndTarget : t -> t
  val compare: t -> t -> int
  val vars : t -> GeneralStruct.VariableS.t
  val varOccurenceNumber : t -> Variable.t -> int
  val renameVar : t -> Variable.t -> Variable.t -> t
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module TgdS : Set.S with type Elt.t = Tgd.t
               
module Mapping : sig
  type t
  val empty : t
  val length : t -> int
  val add : t -> Tgd.t -> t
  val remove : t -> Tgd.t -> t
  val chooseRandomTgd : t -> Tgd.t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val print : t -> unit
  val toString : t -> string
  val toStringAsPairsOfInstances : t -> string
  val toOutputString : t -> string
  val unifyTwoVariables : t -> GeneralStruct.Variable.t -> GeneralStruct.Variable.t -> t
  val extractExistentiallyQuantifiedVariables : t ->  VariableS.t
  val invertTgds : t -> t
  val vars : t -> GeneralStruct.VariableS.t
  val from_list : Tgd.t list -> t
  val map : t -> f:(Tgd.t -> Tgd.t) -> t
  val fold : t ->  init:'accum -> f:('accum -> Tgd.t ->'accum) -> 'accum
  val iter : t -> f:(Tgd.t -> unit) -> unit
  val filter : t -> f:(Tgd.t -> bool) -> t
  val for_all : t -> f:(Tgd.t -> bool) -> bool                                                         
  val exists : t -> f:(Tgd.t -> bool) -> bool
  val is_empty : t -> bool
  val choose : t -> Tgd.t option
  val choose_exn : t -> Tgd.t
end

module MappingPair : sig
  type t
  val empty : t

  val addCanonicalTgd : t -> Tgd.t -> t
  val addCanonicalMapping : t -> Mapping.t -> t
  val addExpectedTgd : t -> Tgd.t -> t
  val addExpectedMapping : t -> Mapping.t -> t

  val getCanonicalMapping : t -> Mapping.t
  val getExpectedMapping : t -> Mapping.t 
end

