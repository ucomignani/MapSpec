open Core.Std
  
val chaseAtomConjunction : Mapping.Mapping.t -> Mapping.AtomConjunction.t -> Mapping.AtomConjunction.t
val chaseAtomConjunctionWithTgd : Mapping.Tgd.t -> Mapping.AtomConjunction.t -> Mapping.AtomConjunction.t


val subsummedTgds : Mapping.Tgd.t -> Mapping.Tgd.t -> bool
val equivConjunctions : Mapping.AtomConjunction.t -> Mapping.AtomConjunction.t -> bool
val equivTgds : Mapping.Tgd.t -> Mapping.Tgd.t -> bool
val equivMappings : Mapping.Mapping.t -> Mapping.Mapping.t -> bool
