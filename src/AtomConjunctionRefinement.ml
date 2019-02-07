open Core.Std

open GeneralStruct
open Mapping

let countAtomQuestionNumber = ref 0
let exploredConjunctions = ref AtomConjunctionS.empty
let invalidTgds = ref Mapping.empty

                         
let redundantCandidateWithPreviousExploration validatedTgds newConjunction baseTgd =
  let newTgd =
    Tgd.create newConjunction (Tgd.rhs baseTgd)
  in
  Mapping.exists ~f:(fun validTgd ->
                     Chase.subsummedTgds newTgd validTgd)
                         validatedTgds

let redundantCandidateWithPreviousInvalidExploration newConjunction baseTgd =
  let newTgd =
    Tgd.create newConjunction (Tgd.rhs baseTgd)
  in
  !invalidTgds
  |> Mapping.exists ~f:(fun invalidTgd -> Chase.subsummedTgds invalidTgd newTgd)
  
let generateBaseCandidatesConjunctions tgd =
  Tgd.lhs tgd
  |> AtomConjunction.fold ~f:(fun atomConjSet atom -> AtomConjunctionS.add atomConjSet (AtomConjunction.add AtomConjunction.empty atom))
                          ~init:AtomConjunctionS.empty
  
let extractMinimalSetOfVariables tgd =
  let lhsVars =
    tgd
    |> Tgd.lhs
    |> AtomConjunction.vars
  in
  let rhsVars =
    tgd
    |> Tgd.rhs
    |> AtomConjunction.vars
  in
  VariableS.inter rhsVars lhsVars
  
let askQuestionConjunctionValidity example =
  Printf.printf "\nIs the example\n";
  Tgd.printAsInstancesPair example;
  Printf.printf "valid?(Y/N)\n"

let askQuestionConjunctionValidityWithLogInFile example =
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\nIs the example\n"
                                          ^ (Tgd.toStringAsInstancesPair example)
                                          ^ "valid?(Y/N)\n")

let rec askConjunctionValidity tgd conjunction =
  let newTgd =
    Tgd.create conjunction (Tgd.rhs tgd)
  in
  let sourceInstance =
    conjunction                                  
  in
  let targetInstance =
    Chase.chaseAtomConjunctionWithTgd newTgd sourceInstance
  in
  let tgdTest =
    Tgd.create sourceInstance targetInstance
  in
  let tgdTestSimpl =
    tgdTest
    |> Mapping.add Mapping.empty
    |> Preprocessing.splitReduction
         (GeneralStruct.VariableS.to_list
            (Tgd.extractExistentiallyQuantifiedVariables tgdTest)
         )
    |> Preprocessing.sigmaRedundancySuppression
    |> Mapping.choose_exn
  in
  let validity = askQuestionConjunctionValidity tgdTestSimpl;
                 read_line ()
  in
  match validity with
  | "Y" | "y" -> true
  | "N" | "n" -> let newTgd =
                   Tgd.rhs tgd
                   |> Tgd.create conjunction
                 in
                 invalidTgds := Mapping.add !invalidTgds newTgd;
                 false
  | _ -> Printf.printf "Wrong entry.\n ";
         askConjunctionValidity tgd conjunction

let validForExploration conjunction1 conjunction2 =
  (Int.equal (AtomConjunction.length (AtomConjunction.diff conjunction1 conjunction2)) 1)
  &&
    (Int.equal (AtomConjunction.length (AtomConjunction.diff conjunction2 conjunction1)) 1)    

let generateCandidatesConjunctions valids invalids =
  let searchValidsConj conj =
    invalids
    |> AtomConjunctionS.filter ~f:(fun conjToTest -> validForExploration conj conjToTest)
    |> AtomConjunctionS.map ~f:(fun validConj -> AtomConjunction.union validConj conj)
  in
  invalids
  |> AtomConjunctionS.fold ~f:(fun accum toTest -> AtomConjunctionS.union accum (searchValidsConj toTest))
                           ~init:AtomConjunctionS.empty
  |> AtomConjunctionS.filter
       ~f:(fun conjunction -> AtomConjunctionS.for_all
                                ~f:(fun validConjunction -> conjunction
                                                            |> AtomConjunction.isASubsetOf validConjunction
                                                            |> not
                                   )
                                valids
          )

let generateCandidatesConjunctionsNaiveTopDown candidates =
  let searchValidsConj conj =
    candidates
    |> AtomConjunctionS.filter ~f:(fun conjToTest -> validForExploration conj conjToTest)
    |> AtomConjunctionS.map ~f:(fun validConj -> AtomConjunction.union validConj conj)
  in
  candidates
  |> AtomConjunctionS.fold ~f:(fun accum toTest -> AtomConjunctionS.union accum (searchValidsConj toTest))
                           ~init:AtomConjunctionS.empty
let askSingleCandidateValidity  quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd candidate =
  let generateNewTgd baseTgd lhs =
    Tgd.create lhs (Tgd.rhs baseTgd)
  in
  candidate
  |> (fun conjunction ->
    if AtomConjunction.containVars conjunction neededVariables
       &&
         (not quasiLatticeOptimisation
          ||
            (
              not (redundantCandidateWithPreviousExploration validatedTgds conjunction tgd)
              &&
                not (redundantCandidateWithPreviousInvalidExploration conjunction tgd)
            )
         )
      then      
        (countAtomQuestionNumber := !countAtomQuestionNumber + 1;
         exploredConjunctions := AtomConjunctionS.add !exploredConjunctions conjunction;
         match expectedResult with
         | None -> askConjunctionValidity tgd conjunction
         | Some expectedMapping -> let newTgd =
                                     generateNewTgd tgd conjunction
                                   in
                                   let sourceInstance =
                                     conjunction                                  
                                   in
                                   let targetInstance =
                                     Chase.chaseAtomConjunctionWithTgd newTgd sourceInstance
                                   in
                                   let tgdTest =
                                     Tgd.create sourceInstance targetInstance
                                   in
                                   let tgdTestSimpl =
                                     tgdTest
                                     |> Mapping.add Mapping.empty
                                     |> Preprocessing.splitReduction
                                          (GeneralStruct.VariableS.to_list
                                             (Tgd.extractExistentiallyQuantifiedVariables tgdTest)
                                          )
                                     |> Preprocessing.sigmaRedundancySuppression
                                     |> Mapping.choose_exn
                                   in
                                   let result =
                                     expectedMapping
                                     |>Mapping.exists
                                         ~f:(fun expectedTgd -> expectedTgd
                                                                |> Chase.subsummedTgds newTgd)
                                   in
                                   askQuestionConjunctionValidityWithLogInFile tgdTestSimpl;
                                   (match result with
                                    | true ->  Bolt.Logger.log "log" Bolt.Level.TRACE "-> T\n"
                                    | false -> invalidTgds := Mapping.add !invalidTgds newTgd;
                                               Bolt.Logger.log "log" Bolt.Level.TRACE "-> F\n"
                                   );
                                   result                      
        )
    else false
  )
  
let askCandidatesValidity  quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd candidates =
  let generateNewTgd baseTgd lhs =
    Tgd.create lhs (Tgd.rhs baseTgd)
  in
  candidates
  |> AtomConjunctionS.partition_tf
       ~f:(fun conjunction ->
         if AtomConjunction.containVars conjunction neededVariables
            &&
         (not quasiLatticeOptimisation
          ||
            (
              not (redundantCandidateWithPreviousExploration validatedTgds conjunction tgd)
              &&
                not (redundantCandidateWithPreviousInvalidExploration conjunction tgd)
            )
         )
         then 
               (countAtomQuestionNumber := !countAtomQuestionNumber + 1;
              exploredConjunctions := AtomConjunctionS.add !exploredConjunctions conjunction;
              match expectedResult with
              | None -> askConjunctionValidity tgd conjunction
              | Some expectedMapping ->  let newTgd =
                                           generateNewTgd tgd conjunction
                                         in
                                         let sourceInstance =
                                           conjunction                                  
                                         in
                                         let targetInstance =
                                           Chase.chaseAtomConjunctionWithTgd newTgd sourceInstance
                                         in
                                         let tgdTest =
                                           Tgd.create sourceInstance targetInstance
                                         in
                                         let tgdTestSimpl =
                                           tgdTest
                                           |> Mapping.add Mapping.empty
                                           |> Preprocessing.splitReduction
                                                (GeneralStruct.VariableS.to_list
                                                   (Tgd.extractExistentiallyQuantifiedVariables tgdTest)
                                                )
                                           |> Preprocessing.sigmaRedundancySuppression
                                           |> Mapping.choose_exn
                                         in
                                         let result =
                                           expectedMapping
                                          |>Mapping.exists
                                              ~f:(fun expectedTgd -> expectedTgd
                                                                     |> Chase.subsummedTgds newTgd)
                                        in
                                        askQuestionConjunctionValidityWithLogInFile tgdTestSimpl;
                                        (match result with
                                         | true ->   Bolt.Logger.log "log" Bolt.Level.TRACE "-> T\n"
                                         | false ->  invalidTgds := Mapping.add !invalidTgds newTgd;
                                                     Bolt.Logger.log "log" Bolt.Level.TRACE "-> F\n"
                                        );
                                        result
                                        
             )
         else false
       )


let rec explorationAPriori
          quasiLatticeOptimisation
          validatedTgds
          expectedResult
          neededVariables
          tgd
          validsConjunctions
          candidates =
  if AtomConjunctionS.is_empty candidates (* no candidate *)
     || (AtomConjunctionS.is_empty  validsConjunctions (* no valid conjunction and the actual candidate is the supremum *)
         && AtomConjunctionS.for_all ~f:(fun conjunction -> let lhsSize =
                                                              tgd
                                                              |> Tgd.lhs
                                                              |> AtomConjunction.length
                                                            in
                                                            (AtomConjunction.length conjunction) = lhsSize
                                        ) candidates
        )
  then candidates
  else let candidatesValidity = askCandidatesValidity quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd candidates
       in
       match candidatesValidity with
       | (valids,invalids) when AtomConjunctionS.is_empty invalids -> valids
       | (valids,invalids) -> let valids2 =
                                generateCandidatesConjunctions (AtomConjunctionS.union valids validsConjunctions) invalids
                                |> explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd valids
                              in
                              AtomConjunctionS.union valids valids2

let rec explorationTopDownBreadth
          quasiLatticeOptimisation
          validatedTgds
          expectedResult
          neededVariables
          tgd
          candidates =
  if AtomConjunctionS.is_empty candidates (* no candidate *)
     || (AtomConjunctionS.for_all ~f:(fun conjunction -> let lhsSize = (* the actual candidate is the supremum *)
                                                           tgd
                                                           |> Tgd.lhs
                                                           |> AtomConjunction.length
                                                         in
                                                         (AtomConjunction.length conjunction) = lhsSize
                                     ) candidates
        )
  then (candidates,AtomConjunctionS.empty)
  else
    (* explore upper levels *)
    let (upperValidCandidates,upperInvalidCandidates) = 
      candidates
      |> generateCandidatesConjunctionsNaiveTopDown
      |> explorationTopDownBreadth quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
    in
    let (actualValidsConj,actualInvalidsConj) =
      (* eliminate candidates from actual level n which are not sub-partitions of valids partitions in level n+1 *)
      candidates
      |> AtomConjunctionS.filter ~f:(fun candidate ->
                                   upperValidCandidates
                                   |> AtomConjunctionS.exists ~f:(fun setToCompare -> (AtomConjunction.isASubsetOf candidate setToCompare)
                                                                 )
                                 )
      |> AtomConjunctionS.filter ~f:(fun candidate -> (*check if candidate is not subset of an eliminated conjunction *)
                                   upperInvalidCandidates
                                   |> AtomConjunctionS.for_all ~f:(fun setToCompare -> not (AtomConjunction.isASubsetOf candidate setToCompare)
                                                                 )
                                 )
      (* ask validity of remaining candidates *)
      |> askCandidatesValidity quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
    in
    let prunedUpperValidsConj =
      (* eliminate partitions of upper levels which are supersets of partitions validated at current level *)
      upperValidCandidates
      |> AtomConjunctionS.filter ~f:(fun candidate ->
                                   actualValidsConj
                                   |> AtomConjunctionS.for_all ~f:(fun setToCompare -> not (AtomConjunction.isASubsetOf setToCompare candidate)
                                                                  )
                                 )
    in
    ( (AtomConjunctionS.union actualValidsConj prunedUpperValidsConj), (AtomConjunctionS.union actualInvalidsConj upperInvalidCandidates) )

let rec explorationBottomUpDepth
          quasiLatticeOptimisation
          validatedTgds
          expectedResult
          neededVariables
          tgd
          actualSubConjunction
          exploredConjunctions
          validsConjunctions
          candidates =
  if AtomConjunctionS.is_empty candidates (* no candidate *)
     || (*  the actual candidate is the supremum *)
       (AtomConjunctionS.for_all ~f:(fun conjunction -> let lhsSize =
                                                              tgd
                                                              |> Tgd.lhs
                                                              |> AtomConjunction.length
                                                            in
                                                            (AtomConjunction.length conjunction) = lhsSize
                                        ) candidates
        )
  then (candidates,(AtomConjunctionS.union exploredConjunctions candidates))
  else let prunedCandidates =
         candidates
         |> AtomConjunctionS.filter ~f:(fun conjunction -> (AtomConjunction.isASubsetOf actualSubConjunction conjunction)
                                                           && not (exploredConjunctions |> AtomConjunctionS.exists ~f:(fun conjunctionToCompare -> AtomConjunction.isASubsetOf conjunction conjunctionToCompare
                                                                                                                                                   && AtomConjunction.isASubsetOf conjunctionToCompare conjunction)
                                                                  )
                                       )
       in
       prunedCandidates
       |> AtomConjunctionS.fold ~f:(fun (validsAccum,exploredAccum) conjunction -> let newExploredAccum = AtomConjunctionS.add exploredAccum conjunction
                                                                                   in
                                                                                   if askSingleCandidateValidity quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd conjunction
                                                                                   then
                                                                                     let newValidsAccum = AtomConjunctionS.add validsAccum conjunction
                                                                                     in
                                                                                     (newValidsAccum,newExploredAccum)
                                                                                   else
                                                                                     let (resValidsAccum,resExploredAccum) =
                                                                                       prunedCandidates
                                                                                       |> generateCandidatesConjunctionsNaiveTopDown
                                                                                       |> explorationBottomUpDepth quasiLatticeOptimisation
                                                                                                                   validatedTgds
                                                                                                                   expectedResult
                                                                                                                   neededVariables
                                                                                                                   tgd
                                                                                                                   conjunction
                                                                                                                   newExploredAccum
                                                                                                                   validsAccum
                                                                                     in
                                                                                     (AtomConjunctionS.union resValidsAccum validsAccum, AtomConjunctionS.union resExploredAccum newExploredAccum)
                                                                                                                 
                                                                                     
                             )
                             ~init:(validsConjunctions,exploredConjunctions)

       
let explorationTopDownDepth
          quasiLatticeOptimisation
          validatedTgds
          expectedResult
          neededVariables
          tgd
          actualSubConjunction
          exploredConjunctions
          validsConjunctions
          candidates =
  if AtomConjunctionS.is_empty candidates (* no candidate *)
     || (*  the actual candidate is the supremum *)
       (AtomConjunctionS.for_all ~f:(fun conjunction -> let lhsSize =
                                                          tgd
                                                          |> Tgd.lhs
                                                          |> AtomConjunction.length
                                                        in
                                                        (AtomConjunction.length conjunction) = lhsSize
                                    ) candidates
       )
  then (candidates,candidates)
  else let prunedCandidates =
         candidates
         |> AtomConjunctionS.filter ~f:(fun conjunction -> (AtomConjunction.isASubsetOf actualSubConjunction conjunction)
                                                           && not (exploredConjunctions |> AtomConjunctionS.exists ~f:(fun conjunctionToCompare -> AtomConjunction.isASubsetOf conjunction conjunctionToCompare
                                                                                                                             && AtomConjunction.isASubsetOf conjunctionToCompare conjunction)
                                                          )
                                 )
       in
       prunedCandidates
       |> AtomConjunctionS.fold ~f:(fun (validsAccum,exploredAccum) conjunction -> let (resValidsAccum,resExploredAccum) =
                                                                               candidates
                                                                               |> generateCandidatesConjunctionsNaiveTopDown
                                                                               |> explorationBottomUpDepth quasiLatticeOptimisation
                                                                                                           validatedTgds
                                                                                                           expectedResult
                                                                                                           neededVariables
                                                                                                           tgd
                                                                                                           conjunction
                                                                                                           exploredAccum
                                                                                                           validsAccum
                                                                           in
                                                                           let newExploredAccum = AtomConjunctionS.add exploredAccum conjunction
                                                                           in
                                                                           (* ask only if super-conjunctions are validated *)
                                                                           if (resExploredAccum
                                                                               |> AtomConjunctionS.for_all
                                                                                    ~f:(fun conjunctionToCompare -> not (AtomConjunction.isASubsetOf conjunction conjunctionToCompare)
                                                                                                                  ||
                                                                                                                    (resValidsAccum
                                                                                                                     |> AtomConjunctionS.exists
                                                                                                                          ~f:(fun validConjunction -> AtomConjunction.isASubsetOf validConjunction conjunctionToCompare
                                                                                                                             && AtomConjunction.isASubsetOf conjunctionToCompare validConjunction)
                                                                                                                    )
                                                                                       )
                                                                              )
                                                                           then
                                                                             if askSingleCandidateValidity quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd conjunction
                                                                             then
                                                                               let newValidsAccum = AtomConjunctionS.add validsAccum conjunction
                                                                               in
                                                                               (newValidsAccum,newExploredAccum)
                                                                             else
                                                                               (AtomConjunctionS.union resValidsAccum validsAccum, AtomConjunctionS.union resExploredAccum newExploredAccum)
                                                                           else
                                                                             (AtomConjunctionS.union resValidsAccum validsAccum, AtomConjunctionS.union resExploredAccum newExploredAccum)
                                                                            
                             )
                          ~init:(validsConjunctions,exploredConjunctions)
       
let generateTgds tgd conjunctionS =
  let rhs =
    Tgd.rhs tgd
  in
  conjunctionS
  |> AtomConjunctionS.fold ~f:(fun mapping lhs -> let tgdToAdd =
                                                    Tgd.create lhs rhs
                                                  in
                                                  Mapping.add mapping tgdToAdd
                              )
                           ~init:Mapping.empty

let executeAPriori quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd =
  generateBaseCandidatesConjunctions tgd
  |> explorationAPriori quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd AtomConjunctionS.empty (* empty set correspond to valids conjunctions*)
  |> generateTgds tgd

let executeTDB quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd =
  generateBaseCandidatesConjunctions tgd
  |> explorationTopDownBreadth quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
  |> fst
  |> generateTgds tgd

let executeBUD quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd =
  generateBaseCandidatesConjunctions tgd
  |> explorationBottomUpDepth quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd AtomConjunction.empty AtomConjunctionS.empty AtomConjunctionS.empty
  |> fst
  |> generateTgds tgd
  |> Preprocessing.sigmaRedundancySuppression

let executeTDD quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd =
  generateBaseCandidatesConjunctions tgd
  |> explorationTopDownDepth quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd AtomConjunction.empty AtomConjunctionS.empty AtomConjunctionS.empty
  |> fst
  |> generateTgds tgd
  |> Preprocessing.sigmaRedundancySuppression
    
let tgd_atomConjunctionRefinement quasiLatticeOptimisation validatedTgds expectedResult strategy tgd =
  Bolt.Logger.log "log" Bolt.Level.TRACE (" ***** Atom refinement: "
                                          ^ Tgd.toString tgd);
  
  exploredConjunctions := AtomConjunctionS.empty;
  
  let neededVariables =
    extractMinimalSetOfVariables tgd
  in
  let result =
    match strategy with
    | Some "AP" ->  executeAPriori quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
                    
    | Some "TDB" -> executeTDB quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd

    | Some "BUD" -> executeBUD quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd

    | Some "TDD" -> executeTDD quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
                    
    | None | Some _ -> executeAPriori quasiLatticeOptimisation validatedTgds expectedResult neededVariables tgd
  in
  Bolt.Logger.log "log" Bolt.Level.TRACE ("Explored conjunctions:\n { {"
                                          ^ ( !exploredConjunctions
                                              |> AtomConjunctionS.to_list
                                              |> List.map ~f:AtomConjunction.toString
                                              |> String.concat ~sep:" };\n { "
                                            )
                                          ^ " } }\n"
                                         )
  ;
    result

let atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult ?strategy mapping =
  countAtomQuestionNumber := 0;

  let res =
    mapping
    |> Mapping.fold ~f:(fun accum tgd -> tgd
                                         |> tgd_atomConjunctionRefinement quasiLatticeOptimisation accum expectedResult strategy
                                         |> Mapping.union accum
                                         |> Preprocessing.sigmaRedundancySuppression
                       )
                    ~init:Mapping.empty
  in
  (* print the number of questions only if automatic answering is used *)
  (match expectedResult with
   | None -> ()
   | Some _ ->
       Bolt.Logger.log "log" Bolt.Level.TRACE ( "Number of questions: "
                                                ^ (Int.to_string !countAtomQuestionNumber)
                                                ^ "\n\n"
                                              )
  )
  ;
    (res,!countAtomQuestionNumber)
