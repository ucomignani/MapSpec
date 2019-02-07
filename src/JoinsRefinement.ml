open Core.Std
   
open GeneralStruct
open Mapping
open JoinsPartition

let countJoinsQuestionNumber = ref 0
let invalidTgds = ref Mapping.empty

                                              
(* for this version: generate one block for each variable (all the lattice is explored) *)
let generateAPrioriBaseCandidatesPartitions variables =
  variables
  |> VariableS.fold ~f:(fun accum variable -> PartitionBlock.add PartitionBlock.empty variable
                                              |> JoinsPartition.add accum
                       )
                    ~init:JoinsPartition.empty
  |> PartitionS.add PartitionS.empty

let extractVariablesToRefine tgd =
  let lhs =
    Tgd.lhs tgd
  in
  lhs
  |> AtomConjunction.vars
  |> VariableS.filter ~f:(fun var -> (AtomConjunction.varOccurenceNumber lhs var) > 1)

let sub_renameVariablesOccurences prefix variable conjunction =
  (* mutable structure to list the new variables (used to create the partition) *)
  let renamedVariables =
    ref VariableS.empty
  in
  (* create a conjunction with all variable's occurences renamed *)
  let renamedConjunction =
    conjunction
    |> AtomConjunction.to_list
    |> List.mapi ~f:(fun iAt atom -> let newAtom =
                                       Atom.getVarsList atom
                                       |> List.mapi ~f:(fun iVar var -> if var = variable
                                                                        then let newVar =
                                                                               Variable.fromString (
                                                                                   (Variable.toString var)
                                                                                   ^"_" ^ prefix
                                                                                   ^"_"
                                                                                   ^(Int.to_string iAt)
                                                                                   ^"_"
                                                                                   ^(Int.to_string iVar))
                                                                             in
                                                                             renamedVariables := VariableS.add !renamedVariables newVar;
                                                                             newVar
                                                                        else var
                                                       )
                                       |> Atom.create (Atom.getRelationSymbol atom)
                                     in
                                     newAtom
                    )
    |> AtomConjunction.of_list
  in
  (!renamedVariables,renamedConjunction)
  
let renameVariableOccurences tgd variable =
  (* rename left-hand side's variables *)
  let (renamedLhsVars,newLhs) =
    Tgd.lhs tgd
    |> sub_renameVariablesOccurences "lhs" variable
  in
  (* rename right-hand side's variables *)
  let (renamedRhsVars,newRhs) =
    Tgd.rhs tgd
    |> sub_renameVariablesOccurences "rhs" variable
  in
  (* create the new tgd, the list of variables and return results *)
  let renamedVariables =
    VariableS.union renamedLhsVars renamedRhsVars
  in
  let newTgd =
    Tgd.create newLhs newRhs
  in
  (renamedVariables, newTgd)
  
let generateTgdWithPartition tgdBase partition =
  partition
  |> JoinsPartition.fold ~f:(fun tgdTmp block -> let refVarInBlock =
                                                   match PartitionBlock.choose block with
                                                   | None -> assert false
                                                   | Some var -> var
                                                 in
                                                 block
                                                 |> PartitionBlock.fold
                                                      ~f:(fun tgdTmp2 var -> Tgd.renameVar tgdTmp2 var refVarInBlock)
                                                      ~init:tgdTmp
                            )
                         ~init:tgdBase

let generateQuestionWithPartition tgd partition =
  let newTgd =
    generateTgdWithPartition tgd partition
  in
  let sourceExample =
    Tgd.lhs newTgd
  in
  let targetExample =
    Chase.chaseAtomConjunctionWithTgd newTgd sourceExample
  in
  let exampleBase =
    Tgd.create sourceExample targetExample
  in
  let example =
    exampleBase
    |> Mapping.add Mapping.empty
    |> Preprocessing.splitReduction
         (GeneralStruct.VariableS.to_list
            (Tgd.extractExistentiallyQuantifiedVariables exampleBase)
         )
    |> Preprocessing.sigmaRedundancySuppression
    |> Mapping.choose_exn
  in
  Printf.printf "\nIs the example\n";
  Tgd.printAsInstancesPair example;
  ;
    Printf.printf "valid?(Y/N)\n"

let generateQuestionWithPartitionAndWithLogger tgd partition =
  let newTgd =
    generateTgdWithPartition tgd partition
  in
  let sourceExample =
    Tgd.lhs newTgd
  in
  let targetExample =
    Chase.chaseAtomConjunctionWithTgd newTgd sourceExample
  in
  let exampleBase =
   Tgd.create sourceExample targetExample
  in
    let example =
    exampleBase
    |> Mapping.add Mapping.empty
    |> Preprocessing.splitReduction
         (GeneralStruct.VariableS.to_list
            (Tgd.extractExistentiallyQuantifiedVariables exampleBase)
         )
    |> Preprocessing.sigmaRedundancySuppression
    |> Mapping.choose_exn
  in
  (*  Printf.printf "%s" ("Is the example\n"
                      ^ DataExample.DataExample.toString example
                      ^ "valid?(Y/N)");*)
  Bolt.Logger.log "log" Bolt.Level.TRACE ("Is the example\n"
                                          ^   Tgd.toStringAsInstancesPair example
                                          ^ "valid?(Y/N)")
  
let generateMappingWithPartitions tgd partitionS =
  partitionS
  |> PartitionS.fold ~f:(fun accum partition -> let newTgd =
                                                  generateTgdWithPartition tgd partition
                                                in                                                
                                                newTgd
                                                |> Mapping.add accum
                        )
                     ~init:Mapping.empty

let redundantCandidateWithPreviousExploration validatedTgds newPartition baseTgd =
  let newTgd =
    (generateTgdWithPartition baseTgd newPartition)
  in
  validatedTgds
  |> Mapping.exists ~f:(fun validTgd -> Chase.subsummedTgds newTgd validTgd )

let redundantCandidateWithPreviousInvalidExploration newPartition baseTgd =
  let newTgd =
    (generateTgdWithPartition baseTgd newPartition)
  in
  !invalidTgds
  |> Mapping.exists ~f:(fun invalidTgd -> Chase.subsummedTgds invalidTgd newTgd)
  
let rec askPartitionValidity tgd partition =
  generateQuestionWithPartition tgd partition
  ;
    let validity = read_line ()
    in
    match validity with
    | "Y" | "y" -> true
    | "N" | "n" -> let newTgd =
                     (generateTgdWithPartition tgd partition)
                   in
                   invalidTgds := Mapping.add !invalidTgds newTgd;
                   false
    | _ -> Printf.printf "Wrong entry.\n ";
           askPartitionValidity tgd partition

let generateCandidatesPartitionsAPriori valids invalids =
  invalids
  |> PartitionS.fold ~f:(fun accum invalidPartition -> invalidPartition (* Generate partitions of the upper level *)
                                                       |> generateUpperPartitions
                                                       |> PartitionS.union accum )
                     ~init:PartitionS.empty
  |> PartitionS.filter ~f:(fun evaluatedCandidate -> valids  (* filter partition with an already validated partition as 'sub-partition' *)
                                                     |> PartitionS.for_all ~f:(fun partition -> not (subpartition partition evaluatedCandidate))
                          )

let generateCandidatesPartitionsNaive candidates =
  candidates
  |> PartitionS.fold ~f:(fun accum candidatePartition -> candidatePartition (* Generate partitions of the upper level *)
                                                         |> generateUpperPartitions
                                                         |> PartitionS.union accum )
                     ~init:PartitionS.empty

let existAVariableAloneInABlock partition variableS =
  let blockVarRef =
    VariableS.to_list variableS
    |> PartitionBlock.of_list
  in
  partition
  |> JoinsPartition.exists ~f:(fun block -> PartitionBlock.diff block blockVarRef
                                            |> PartitionBlock.is_empty
                              )

let askCandidatesValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd candidates =
  let candidatesWithoutEquiv =
    candidates
    |> PartitionS.fold ~f:(fun (accPart,accMap) cand -> let newTgd =
                                                          generateTgdWithPartition tgd cand
                                                        in
                                                        let existEquivPartition =
                                                          accMap
                                                          |> Mapping.exists ~f:(fun tgd -> Chase.equivTgds tgd newTgd)
                                                        in
                                                        if existEquivPartition
                                                        then (accPart,accMap)
                                                        else ((PartitionS.add accPart cand),
                                                              (Mapping.add accMap newTgd)
                                                             )
                                                      
                          )
                       ~init:(PartitionS.empty,Mapping.empty)
  |> fst
  in
  candidatesWithoutEquiv
  |> PartitionS.partition_tf
       ~f:(fun partition -> if existAVariableAloneInABlock partition lhsRenamedVariables
                               ||
                                 (quasiLatticeOptimisation
                                  &&
                                    (redundantCandidateWithPreviousInvalidExploration partition tgd)
                                 )
                            then false
                            else (
                              if (quasiLatticeOptimisation
                                  &&
                                    (redundantCandidateWithPreviousExploration validatedTgds partition tgd)
                                 )
                              then true
                              else
                                (
                                  countJoinsQuestionNumber := !countJoinsQuestionNumber +1;
                                  match expectedResult with
                                  | None -> askPartitionValidity tgd partition
                                  | Some expectedMapping -> let newTgd =
                                                              (generateTgdWithPartition tgd partition)
                                                            in
                                                            let result =
                                                              expectedMapping
                                                              |>Mapping.exists
                                                                  ~f:(fun expectedTgd -> expectedTgd
                                                                                         |> Chase.subsummedTgds newTgd)
                                                            in
                                                            generateQuestionWithPartitionAndWithLogger tgd partition;
                                                            (match result with
                                                             | true -> Bolt.Logger.log "log" Bolt.Level.TRACE "-> T\n"
                                                             | false -> invalidTgds := Mapping.add !invalidTgds newTgd;
                                                                        Bolt.Logger.log "log" Bolt.Level.TRACE "-> F\n"
                                                            );
                                                            result
                                )
                            )
          )
  
let askSingleCandidateValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd candidate =
  candidate
  |> (fun partition -> if existAVariableAloneInABlock partition lhsRenamedVariables
                          ||
                            (quasiLatticeOptimisation
                             &&
                               (redundantCandidateWithPreviousInvalidExploration partition tgd)
                            )
                       then false
                       else (
                         if (quasiLatticeOptimisation
                             &&
                                    (redundantCandidateWithPreviousExploration validatedTgds partition tgd)
                            )
                         then true
                         else
                           ( countJoinsQuestionNumber := !countJoinsQuestionNumber +1;
                             match expectedResult with
                             | None -> askPartitionValidity tgd partition
                             | Some expectedMapping -> let newTgd =
                                                         (generateTgdWithPartition tgd partition)
                                                       in
                                                       let result =
                                                         expectedMapping
                                                         |>Mapping.exists
                                                             ~f:(fun expectedTgd -> expectedTgd
                                                                                    |> Chase.subsummedTgds newTgd)
                                                       in
                                                       generateQuestionWithPartitionAndWithLogger tgd partition;
                                                       (match result with
                                                        | true ->  Bolt.Logger.log "log" Bolt.Level.TRACE "-> T\n"
                                                        | false -> invalidTgds := Mapping.add !invalidTgds newTgd;
                                                                   Bolt.Logger.log "log" Bolt.Level.TRACE "-> F\n"
                                                       );
                                                       result
                           )
                       )
     )
  
let rec explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd validsPartitions candidates =
  if Int.equal (PartitionS.length candidates) 1 (* only one candidate *)
     && PartitionS.for_all ~f:(fun partition -> JoinsPartition.length partition = 1) candidates (* the candidate is the supremum *)
  then candidates
  else let candidatesValidity = askCandidatesValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd candidates
       in
       match candidatesValidity with
       | (valids,invalids) when PartitionS.is_empty invalids -> valids
       | (valids,invalids) -> let valids2 =
                                invalids
                                |> generateCandidatesPartitionsAPriori (PartitionS.union valids validsPartitions) (* give all the valids partitions in input *)
                                |> explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd valids
                              in
                              PartitionS.union valids valids2      


let rec explorationTopDownBreadth quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd candidates =
  if Int.equal (PartitionS.length candidates) 1 (* only one candidate *)
     && PartitionS.for_all ~f:(fun partition -> JoinsPartition.length partition = 1) candidates (* the candidate is the supremum *)
  then (candidates,PartitionS.empty)
  else
    (* explore upper levels *)
    let (upperValidCandidates,upperInvalidCandidates) = 
      candidates
      |> generateCandidatesPartitionsNaive
      |> explorationTopDownBreadth quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd
    in
    let (actualValidsPartitions,actualInvalidsPartitions) =
      (* eliminate candidates from actual level n which are not sub-partitions of valids partitions in level n+1 *)
      candidates
      |> PartitionS.filter ~f:(fun candidatePartition ->
                             upperValidCandidates
                             |> PartitionS.exists ~f:(fun partitionToCompare -> (subpartition candidatePartition partitionToCompare)
                                                     )
                           )
      |> PartitionS.filter ~f:(fun candidate -> (*check if candidate is not subset of an eliminated partition *)
                             upperInvalidCandidates
                             |> PartitionS.for_all ~f:(fun setToCompare -> not (subpartition candidate setToCompare)
                                                      )
                           )
      (* ask validity of remaining candidates *)
      |> askCandidatesValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd
    in
    let prunedUpperValidsPartitions =
      (* eliminate partitions of upper levels which are supersets of partitions validated at current level *)
      upperValidCandidates
      |> PartitionS.filter ~f:(fun candidatePartition ->
                             actualValidsPartitions
                             |> PartitionS.for_all ~f:(fun partitionToCompare -> not (subpartition partitionToCompare candidatePartition)
                                                      )
                           )
    in
    (PartitionS.union actualValidsPartitions prunedUpperValidsPartitions,PartitionS.union actualInvalidsPartitions upperInvalidCandidates)

let rec explorationBottomUpDepth
          quasiLatticeOptimisation
          validatedTgds
          expectedResult   
          lhsRenamedVariables
          tgd
          actualSubPartition
          exploredPartitions
          validsPartitions
          candidates =
  if Int.equal (PartitionS.length candidates) 1 (* only one candidate *)
     && PartitionS.for_all ~f:(fun partition -> JoinsPartition.length partition = 1) candidates (* the candidate is the supremum *)
  then (candidates,(PartitionS.union exploredPartitions candidates))
  else let prunedCandidates =
         candidates
         |> PartitionS.filter ~f:(fun partition -> (subpartition actualSubPartition partition)
                                                   && not (exploredPartitions |> PartitionS.exists ~f:(fun partitionToCompare -> JoinsPartition.equal partitionToCompare partition)
                                                          )
                                 )
       in
       prunedCandidates
       |> PartitionS.fold ~f:(fun (validsAccum,exploredAccum) partition -> let newExploredAccum = PartitionS.add exploredAccum partition
                                                                           in
                                                                           if askSingleCandidateValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd partition
                                                                           then
                                                                             let newValidsAccum = PartitionS.add validsAccum partition
                                                                             in
                                                                             (newValidsAccum,newExploredAccum)
                                                                           else
                                                                             let (resValidsAccum,resExploredAccum) =
                                                                               candidates
                                                                               |> generateCandidatesPartitionsNaive
                                                                               |> explorationBottomUpDepth quasiLatticeOptimisation
                                                                                                           validatedTgds
                                                                                                           expectedResult  
                                                                                                           lhsRenamedVariables
                                                                                                           tgd
                                                                                                           partition
                                                                                                           newExploredAccum
                                                                                                           validsAccum
                                                                             in
                                                                             (PartitionS.union resValidsAccum validsAccum, PartitionS.union resExploredAccum newExploredAccum)
                             )
                          ~init:(validsPartitions,exploredPartitions)

let explorationTopDownDepth
          quasiLatticeOptimisation
          validatedTgds
          expectedResult      
          lhsRenamedVariables
          tgd
          actualSubPartition
          exploredPartitions
          validsPartitions
          candidates =
  if Int.equal (PartitionS.length candidates) 1 (* only one candidate *)
     && PartitionS.for_all ~f:(fun partition -> JoinsPartition.length partition = 1) candidates (* the candidate is the supremum *)
  then (candidates,candidates)
  else let prunedCandidates =
         candidates
         |> PartitionS.filter ~f:(fun partition -> (subpartition actualSubPartition partition)
                                                           && not (exploredPartitions |> PartitionS.exists ~f:(fun partitionToCompare -> JoinsPartition.equal partitionToCompare partition)
                                                          )
                                 )
       in
       prunedCandidates
       |> PartitionS.fold ~f:(fun (validsAccum,exploredAccum) partition -> let (resValidsAccum,resExploredAccum) =
                                                                               candidates
                                                                               |> generateCandidatesPartitionsNaive
                                                                               |> explorationBottomUpDepth quasiLatticeOptimisation
                                                                                                           validatedTgds
                                                                                                           expectedResult
                                                                                                           lhsRenamedVariables
                                                                                                           tgd
                                                                                                           partition
                                                                                                           exploredAccum
                                                                                                           validsAccum
                                                                           in
                                                                           let newExploredAccum = PartitionS.add exploredAccum partition
                                                                           in
                                                                           (* ask only if super-partitions are validated *)
                                                                           if (resExploredAccum
                                                                               |> PartitionS.for_all
                                                                                    ~f:(fun partitionToCompare -> not (subpartition partition partitionToCompare)
                                                                                                                  ||
                                                                                                                    (resValidsAccum
                                                                                                                     |> PartitionS.exists
                                                                                                                          ~f:(fun validPartition -> JoinsPartition.equal validPartition partitionToCompare)
                                                                                                                    )
                                                                                       )
                                                                              )
                                                                           then
                                                                             if askSingleCandidateValidity quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables tgd partition
                                                                             then
                                                                               let newValidsAccum = PartitionS.add validsAccum partition
                                                                               in
                                                                               (newValidsAccum,newExploredAccum)
                                                                             else
                                                                               (PartitionS.union resValidsAccum validsAccum, PartitionS.union resExploredAccum newExploredAccum)
                                                                           else
                                                                             (PartitionS.union resValidsAccum validsAccum, PartitionS.union resExploredAccum newExploredAccum)
                                                                            
                             )
                          ~init:(validsPartitions,exploredPartitions)

let variable_joinsRefinement quasiLatticeOptimisation validatedTgds expectedResult strategy tgd variable =
  let (renamedVariables,newTgd) =
    renameVariableOccurences tgd variable
  in
  let lhsRenamedVariables =
    AtomConjunction.vars (Tgd.rhs newTgd)
    |> VariableS.inter renamedVariables
  in
  match strategy with
  | Some "AP" ->  generateAPrioriBaseCandidatesPartitions renamedVariables
                  |> explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables newTgd PartitionS.empty
                  |> generateMappingWithPartitions newTgd
  | Some "TDB" -> generateAPrioriBaseCandidatesPartitions renamedVariables
                  |> explorationTopDownBreadth quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables newTgd
                  |> fst
                  |> generateMappingWithPartitions newTgd
  | Some "BUD" -> generateAPrioriBaseCandidatesPartitions renamedVariables
                  |> explorationBottomUpDepth quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables newTgd JoinsPartition.empty PartitionS.empty PartitionS.empty
                  |> fst
                  |> generateMappingWithPartitions newTgd
                  |> Preprocessing.sigmaRedundancySuppression
  | Some "TDD" -> generateAPrioriBaseCandidatesPartitions renamedVariables
                  |> explorationTopDownDepth quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables newTgd JoinsPartition.empty PartitionS.empty PartitionS.empty
                  |> fst
                  |> generateMappingWithPartitions newTgd
                  |> Preprocessing.sigmaRedundancySuppression
  | None | Some _ -> generateAPrioriBaseCandidatesPartitions renamedVariables
                     |> explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult lhsRenamedVariables newTgd PartitionS.empty
                     |> generateMappingWithPartitions newTgd
                     
let tgd_joinsRefinement quasiLatticeOptimisation validatedTgds expectedResult strategy tgd =
  Bolt.Logger.log "log" Bolt.Level.TRACE (" ***** Joins refinement: "
                                          ^ Tgd.toString tgd);
  let mapping =
    Mapping.add Mapping.empty tgd
  in
  let result =
    tgd
    |> extractVariablesToRefine
    |> VariableS.fold
         ~f:(fun accum variable -> accum
                                   |> Mapping.fold
                                        ~f:(fun accum tgdTmp -> variable_joinsRefinement quasiLatticeOptimisation validatedTgds expectedResult strategy tgdTmp variable
                                                                |> Mapping.union accum)
                                        ~init:Mapping.empty)
         ~init:mapping
  in
    result
  
let joinsRefinement quasiLatticeOptimisation ?expectedResult ?strategy inputMapping =
  countJoinsQuestionNumber := 0;
  let res =
    inputMapping
    |> Mapping.fold ~f:(fun accum tgd -> tgd
                                         |> tgd_joinsRefinement quasiLatticeOptimisation accum expectedResult strategy
                                         |> Mapping.union accum 
                                         |> Preprocessing.sigmaRedundancySuppression
                       )
                    ~init:Mapping.empty
  in
  (* print the number of questions only if automatic answering is used *)
  (
    match expectedResult with
    | None -> ()
    | Some _ ->
        Bolt.Logger.log "log" Bolt.Level.TRACE ("Number of questions: "
                                                ^ ( Int.to_string !countJoinsQuestionNumber)
                                                ^ "\n\n"
                                               )
  )
  ;
    (res,!countJoinsQuestionNumber)
