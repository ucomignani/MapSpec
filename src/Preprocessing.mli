open Core.Std

open GeneralStruct
open Mapping

val splitReduction : Variable.t list -> Mapping.t -> Mapping.t
val sigmaRedundancySuppression : Mapping.t -> Mapping.t
val normalizeMapping : Variable.t list -> Mapping.t -> Mapping.t
