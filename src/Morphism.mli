open Core.Std

open GeneralStruct

module Morphism : sig
  type t

  val empty : t
  val create : Variable.t -> Variable.t -> t
  val add : t -> key:Variable.t -> data:Variable.t -> t
  val find : t -> Variable.t -> Variable.t option
  val find_exn : t -> Variable.t -> Variable.t
  val getDomain : t -> VariableS.t
  val getCodomain : t -> VariableS.t
  val length : t -> int
  val print : t -> unit
end
                    
module MorphismS : Set.S with type Elt.t = Morphism.t

(* return a tuple (bool, morphismS) with bool equal to true if the input morphism has been modified *)
val expandMorphismToAtomsPair : Morphism.t -> Mapping.Atom.t -> Mapping.Atom.t -> Morphism.t list

val expandMorphismToAtomConjunctionsPair : Morphism.t -> Mapping.AtomConjunction.t -> Mapping.AtomConjunction.t -> MorphismS.t
                                                                               
val printMorphismS : MorphismS.t -> unit                        
