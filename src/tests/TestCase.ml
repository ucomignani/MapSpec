open OUnit2

open GeneralStruct
open Mapping
open Preprocessing

let quasiLatticeOptimisation =
  true
  
         
let testCase =
  let examplePath =
    "testFiles/example1.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** AP  "^examplePath^"*****\n");
  
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in  
  let existentialVariables = canonicalGLAVmapping
                             |> Mapping.extractExistentiallyQuantifiedVariables
                             |> GeneralStruct.VariableS.to_list
  in
  canonicalGLAVmapping
  |> Mapping.iter  ~f:(fun dataExample -> "\nDataEx:\n" ^ (Tgd.toStringAsInstancesPair dataExample)
                                          |> Bolt.Logger.log "log" Bolt.Level.TRACE)
  ;

    
    let fligthRS = RelationSymbol.fromString "Flight" in
    let fromVar = Variable.fromString "from" in
    let toVar = Variable.fromString "to" in
    let airlineVar = Variable.fromString "idAirline" in
    
    let airlineRS = RelationSymbol.fromString "Airline" in
    let airlineNameVar = Variable.fromString "name" in
    let locationVar = Variable.fromString "location" in
    
    let departureRS = RelationSymbol.fromString "Departure" in

    let arrivalRS = RelationSymbol.fromString "Arrival" in

    let companyRS = RelationSymbol.fromString "Company" in
    let companyVar = Variable.fromString "idCompany" in

    let flightAtom = Atom.create fligthRS (fromVar::toVar::airlineVar::[]) in
    let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

    let departureAtom = Atom.create departureRS (fromVar::companyVar::[]) in
    let arrivalAtom = Atom.create arrivalRS (toVar::companyVar::[]) in
    let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in

    
    let sourceConjunction1 =
      AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
    in
    let targetConjunction1 =
      AtomConjunction.add (
          AtomConjunction.add (
              AtomConjunction.add AtomConjunction.empty departureAtom)
                              arrivalAtom)
                          companyAtom
    in

    let tgd1 = Tgd.create sourceConjunction1 targetConjunction1 in

    let mappingRef =
      Mapping.add Mapping.empty tgd1
    in
    "\nExpected mapping:\n" ^ (Mapping.toString mappingRef)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE;

    "\n\n cGsm:\n" ^ (Mapping.toString canonicalGLAVmapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;
    
    let splitRedCanGLAVsm =
      Preprocessing.splitReduction existentialVariables canonicalGLAVmapping
    in
    "\n\n split reduced cGsm:\n" ^ (Mapping.toString splitRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let sigmaRedCanGLAVsm =
      Preprocessing.sigmaRedundancySuppression splitRedCanGLAVsm
    in
    "\n\n result of sigma redundancy suppression:\n" ^ (Mapping.toString sigmaRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (atomRefinedMapping,countAtomQuestions) =
      AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) sigmaRedCanGLAVsm
    in
    "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (joinRefinedMapping,countJoinQuestions) =
      JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) atomRefinedMapping
    in
    "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    "Parsing tests" >:::
      [
        "test source1 length" >::
          (fun _ -> assert_equal 4 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 6  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
        "test cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
        "test split red cGsm length" >::
          (fun _ -> assert_equal 2 (Mapping.length splitRedCanGLAVsm));
        "test sigma red cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length sigmaRedCanGLAVsm));
        "test atom refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length atomRefinedMapping));
        "test join refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length joinRefinedMapping));
        "test final mapping equivalent to expected" >::
          (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
      ]


let testCaseTDB =
  let examplePath =
    "testFiles/example1.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** TDB "^examplePath^"*****\n");

  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in
  canonicalGLAVmapping
  |> Mapping.iter  ~f:(fun dataExample -> "\nDataEx:\n" ^ (Tgd.toStringAsInstancesPair dataExample)
                                               |> Bolt.Logger.log "log" Bolt.Level.TRACE)
  ;

    
    let fligthRS = RelationSymbol.fromString "Flight" in
    let fromVar = Variable.fromString "from" in
    let toVar = Variable.fromString "to" in
    let airlineVar = Variable.fromString "idAirline" in
    
    let airlineRS = RelationSymbol.fromString "Airline" in
    let airlineNameVar = Variable.fromString "name" in
    let locationVar = Variable.fromString "location" in
    
    let departureRS = RelationSymbol.fromString "Departure" in

    let arrivalRS = RelationSymbol.fromString "Arrival" in

    let companyRS = RelationSymbol.fromString "Company" in
    let companyVar = Variable.fromString "idCompany" in

    let flightAtom = Atom.create fligthRS (fromVar::toVar::airlineVar::[]) in
    let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

    let departureAtom = Atom.create departureRS (fromVar::companyVar::[]) in
    let arrivalAtom = Atom.create arrivalRS (toVar::companyVar::[]) in
    let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in

    
    let sourceConjunction1 =
      AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
    in
    let targetConjunction1 =
      AtomConjunction.add (
          AtomConjunction.add (
              AtomConjunction.add AtomConjunction.empty departureAtom)
                              arrivalAtom)
                          companyAtom
    in

    let tgd1 = Tgd.create sourceConjunction1 targetConjunction1 in

    let mappingRef =
      Mapping.add Mapping.empty tgd1
    in
    "\nExpected mapping:\n" ^ (Mapping.toString mappingRef)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE;

    
     let existentialVariables = canonicalGLAVmapping
                               |> Mapping.extractExistentiallyQuantifiedVariables
                               |> GeneralStruct.VariableS.to_list
     in
    "\n\n cGsm:\n" ^ (Mapping.toString canonicalGLAVmapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let splitRedCanGLAVsm =
      Preprocessing.splitReduction existentialVariables canonicalGLAVmapping
    in
    "\n\n split reduced cGsm:\n" ^ (Mapping.toString splitRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let sigmaRedCanGLAVsm =
      Preprocessing.sigmaRedundancySuppression splitRedCanGLAVsm
    in
    "\n\n result of sigma redundancy suppression:\n" ^ (Mapping.toString sigmaRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (atomRefinedMapping,countAtomQuestions) =
      AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDB") sigmaRedCanGLAVsm
    in
    "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (joinRefinedMapping,countJoinQuestions) =
      JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDB") atomRefinedMapping
    in
    "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    "Parsing tests" >:::
      [
        "test source1 length" >::
          (fun _ -> assert_equal 4 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 6  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
        "test cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
        "test split red cGsm length" >::
          (fun _ -> assert_equal 2 (Mapping.length splitRedCanGLAVsm));
        "test sigma red cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length sigmaRedCanGLAVsm));
        "test atom refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length atomRefinedMapping));
        "test join refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length joinRefinedMapping));
        "test final mapping equivalent to expected" >::
          (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
      ]

let testCaseBUD =
  let examplePath =
    "testFiles/example1.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** BUD "^examplePath^"*****\n");
                           
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |>  MappingPair.getCanonicalMapping
  in
  canonicalGLAVmapping
  |> Mapping.iter  ~f:(fun dataExample -> "\nDataEx:\n" ^ (Tgd.toStringAsInstancesPair dataExample)
                                               |> Bolt.Logger.log "log" Bolt.Level.TRACE)
  ;

    
    let fligthRS = RelationSymbol.fromString "Flight" in
    let fromVar = Variable.fromString "from" in
    let toVar = Variable.fromString "to" in
    let airlineVar = Variable.fromString "idAirline" in
    
    let airlineRS = RelationSymbol.fromString "Airline" in
    let airlineNameVar = Variable.fromString "name" in
    let locationVar = Variable.fromString "location" in
    
    let departureRS = RelationSymbol.fromString "Departure" in

    let arrivalRS = RelationSymbol.fromString "Arrival" in

    let companyRS = RelationSymbol.fromString "Company" in
    let companyVar = Variable.fromString "idCompany" in

    let flightAtom = Atom.create fligthRS (fromVar::toVar::airlineVar::[]) in
    let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

    let departureAtom = Atom.create departureRS (fromVar::companyVar::[]) in
    let arrivalAtom = Atom.create arrivalRS (toVar::companyVar::[]) in
    let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in

    
    let sourceConjunction1 =
      AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
    in
    let targetConjunction1 =
      AtomConjunction.add (
          AtomConjunction.add (
              AtomConjunction.add AtomConjunction.empty departureAtom)
                              arrivalAtom)
                          companyAtom
    in

    let tgd1 = Tgd.create sourceConjunction1 targetConjunction1 in

    let mappingRef =
      Mapping.add Mapping.empty tgd1
    in
    "\nExpected mapping:\n" ^ (Mapping.toString mappingRef)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE;

     
     let existentialVariables = canonicalGLAVmapping
                               |> Mapping.extractExistentiallyQuantifiedVariables
                               |> GeneralStruct.VariableS.to_list
     in
    "\n\n cGsm:\n" ^ (Mapping.toString canonicalGLAVmapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let splitRedCanGLAVsm =
      Preprocessing.splitReduction existentialVariables canonicalGLAVmapping
    in
    "\n\n split reduced cGsm:\n" ^ (Mapping.toString splitRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let sigmaRedCanGLAVsm =
      Preprocessing.sigmaRedundancySuppression splitRedCanGLAVsm
    in
    "\n\n result of sigma redundancy suppression:\n" ^ (Mapping.toString sigmaRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (atomRefinedMapping,countAtomQuestions) =
      AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "BUD") sigmaRedCanGLAVsm
    in
    "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (joinRefinedMapping,countJoinQuestions) =
      JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "BUD") atomRefinedMapping
    in
    "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    "Parsing tests" >:::
      [
        "test source1 length" >::
          (fun _ -> assert_equal 4 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 6  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
        "test cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
        "test split red cGsm length" >::
          (fun _ -> assert_equal 2 (Mapping.length splitRedCanGLAVsm));
        "test sigma red cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length sigmaRedCanGLAVsm));
        "test atom refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length atomRefinedMapping));
        "test join refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length joinRefinedMapping));
        "test final mapping equivalent to expected" >::
          (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
      ]

let testCaseTDD =
  let examplePath =
    "testFiles/example1.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** TDD "^examplePath^"*****\n");
                            
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |>  MappingPair.getCanonicalMapping
  in
  canonicalGLAVmapping
  |> Mapping.iter  ~f:(fun dataExample -> "\nDataEx:\n" ^ (Tgd.toStringAsInstancesPair dataExample)
                                               |> Bolt.Logger.log "log" Bolt.Level.TRACE)
  ;

    
    let fligthRS = RelationSymbol.fromString "Flight" in
    let fromVar = Variable.fromString "from" in
    let toVar = Variable.fromString "to" in
    let airlineVar = Variable.fromString "idAirline" in
    
    let airlineRS = RelationSymbol.fromString "Airline" in
    let airlineNameVar = Variable.fromString "name" in
    let locationVar = Variable.fromString "location" in
    
    let departureRS = RelationSymbol.fromString "Departure" in

    let arrivalRS = RelationSymbol.fromString "Arrival" in

    let companyRS = RelationSymbol.fromString "Company" in
    let companyVar = Variable.fromString "idCompany" in

    let flightAtom = Atom.create fligthRS (fromVar::toVar::airlineVar::[]) in
    let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

    let departureAtom = Atom.create departureRS (fromVar::companyVar::[]) in
    let arrivalAtom = Atom.create arrivalRS (toVar::companyVar::[]) in
    let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in

    
    let sourceConjunction1 =
      AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
    in
    let targetConjunction1 =
      AtomConjunction.add (
          AtomConjunction.add (
              AtomConjunction.add AtomConjunction.empty departureAtom)
                              arrivalAtom)
                          companyAtom
    in

    let tgd1 = Tgd.create sourceConjunction1 targetConjunction1 in

    let mappingRef =
      Mapping.add Mapping.empty tgd1
    in
    "\nExpected mapping:\n" ^ (Mapping.toString mappingRef)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE;

    "\n\n cGsm:\n" ^ (Mapping.toString canonicalGLAVmapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    let existentialVariables = canonicalGLAVmapping
                               |> Mapping.extractExistentiallyQuantifiedVariables
                               |> GeneralStruct.VariableS.to_list
    in
    
    let splitRedCanGLAVsm =
      Preprocessing.splitReduction existentialVariables canonicalGLAVmapping
    in
    "\n\n split reduced cGsm:\n" ^ (Mapping.toString splitRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let sigmaRedCanGLAVsm =
      Preprocessing.sigmaRedundancySuppression splitRedCanGLAVsm
    in
    "\n\n result of sigma redundancy suppression:\n" ^ (Mapping.toString sigmaRedCanGLAVsm)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (atomRefinedMapping,countAtomQuestions) =
      AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDD") sigmaRedCanGLAVsm
    in
    "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
    let (joinRefinedMapping,countJoinQuestions) =
      JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDD") atomRefinedMapping
    in
    "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
    |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    "Parsing tests" >:::
      [
        "test source1 length" >::
          (fun _ -> assert_equal 4 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 6  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
        "test cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
        "test split red cGsm length" >::
          (fun _ -> assert_equal 2 (Mapping.length splitRedCanGLAVsm));
        "test sigma red cGsm length" >::
          (fun _ -> assert_equal 1 (Mapping.length sigmaRedCanGLAVsm));
        "test atom refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length atomRefinedMapping));
        "test join refined length" >::
          (fun _ -> assert_equal 1 (Mapping.length joinRefinedMapping));
        "test final mapping equivalent to expected" >::
          (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
      ]
