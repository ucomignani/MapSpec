open Core.Std

open GeneralStruct
open Mapping

val generateCandidatesConjunctions : AtomConjunctionS.t -> AtomConjunctionS.t -> AtomConjunctionS.t

(* Generate the upper semi lattice of valids atoms conjunctions given the tgd in input *)
val generateBaseCandidatesConjunctions : Tgd.t -> AtomConjunctionS.t
       
(* Refinement of atom conjunctions in the left-hand part of a tgd given in input *)
val tgd_atomConjunctionRefinement: bool -> Mapping.t -> Mapping.t option -> string option -> Tgd.t -> Mapping.t

(* Refinement of atom conjunctions in the left-hand part of tgds in the input mapping *)
val atomConjunctionRefinement: bool -> ?expectedResult:Mapping.t -> ?strategy:string -> Mapping.t -> (Mapping.t * int)
