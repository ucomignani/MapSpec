open Core.Std

open GeneralStruct
open Mapping

module PartitionBlock : Set.S with type Elt.t = GeneralStruct.Variable.t
module JoinsPartition : Set.S with type Elt.t = PartitionBlock.t
module PartitionS : Set.S with type Elt.t = JoinsPartition.t
    
val printPartitionBlock : PartitionBlock.t -> unit
            
val printPartition : JoinsPartition.t -> unit

val partitionToString : JoinsPartition.t -> string
                                           
val printPartitionS : PartitionS.t -> unit

val generateUpperPartitions : JoinsPartition.t -> PartitionS.t
                            
val subpartition : JoinsPartition.t -> JoinsPartition.t -> bool
