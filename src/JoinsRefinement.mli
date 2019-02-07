open Mapping

val renameVariableOccurences : Tgd.t -> GeneralStruct.Variable.t -> GeneralStruct.VariableS.t * Tgd.t
val joinsRefinement : bool -> ?expectedResult:Mapping.t -> ?strategy:string -> Mapping.t -> (Mapping.t * int)

