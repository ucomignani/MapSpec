open Core.Std

module PartitionBlock = GeneralStruct.VariableS
module JoinsPartition = Set.Make(PartitionBlock)
module PartitionS = Set.Make(JoinsPartition)

let printPartitionBlock block =
  Printf.printf "{";
  PartitionBlock.iter block GeneralStruct.Variable.print;
  Printf.printf "}"

let partitionBlockToString block = 
  "{"
  ^ (block
     |> PartitionBlock.to_list
     |> List.map ~f:GeneralStruct.Variable.toString
     |> String.concat ~sep:","
    )
  ^ "}"
                
let printPartition partition =
  Printf.printf "{";
  JoinsPartition.iter partition printPartitionBlock;
  Printf.printf "} "

let partitionToString partition =
  "{"
  ^ (partition
     |> JoinsPartition.to_list
     |> List.map ~f:partitionBlockToString
     |> String.concat ~sep:"; "
    )
  ^ "} "
                
let printPartitionS partitionS =
  PartitionS.iter partitionS printPartition
                
let generateUpperPartitions partition =
  (* For each possible pair of block in a partition, make union of them to generate a new partition *)
  partition
  |> JoinsPartition.fold ~f:(fun accum block1 -> let subPartition1 =
                                                   JoinsPartition.remove partition block1
                                                 in
                                                 subPartition1
                                                 |> JoinsPartition.fold ~f:(fun accum block2 -> let subPartition2 =
                                                                                                  JoinsPartition.remove subPartition1 block2
                                                                                                in
                                                                                                PartitionBlock.union block1 block2
                                                                                                |> JoinsPartition.add subPartition2
                                                                                                |> PartitionS.add accum
                                                                           )
                                                                        ~init:PartitionS.empty
                                                 |> PartitionS.union accum
                            )
                         ~init:PartitionS.empty

let subpartition evaluatedPartition referencePartition =
  JoinsPartition.for_all ~f:(fun evaluatedBlock -> JoinsPartition.exists ~f:(fun refBlock -> PartitionBlock.subset evaluatedBlock refBlock)
                                                                         referencePartition
                            )
                         evaluatedPartition
