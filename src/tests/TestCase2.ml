open OUnit2

open GeneralStruct
open Mapping
open Preprocessing

let quasiLatticeOptimisation =
  true

let testCase =
  let examplePath =
    "testFiles/example2.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** "^examplePath^"*****\n");

    
                             
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in  

  (* relations symbols *)
  let travelAgencyRS = RelationSymbol.fromString "TravelAgency" in
  let airlineRS = RelationSymbol.fromString "Airline" in
  let fligthRS = RelationSymbol.fromString "Flight" in

  let departureRS = RelationSymbol.fromString "Departure" in
  let arrivalRS = RelationSymbol.fromString "Arrival" in
  let companyRS = RelationSymbol.fromString "Company" in

  (* variables *)
  let travelAgencyVar = Variable.fromString "idAgency" in
  let agencyNameVar = Variable.fromString "agencyName" in
  let agencyLocationVar = Variable.fromString "agencyLocation" in

  let idFlightVar = Variable.fromString "idFlight" in
  let fromVar = Variable.fromString "from" in
  let toVar = Variable.fromString "to" in
  
  let airlineVar = Variable.fromString "idAirline" in
  let airlineNameVar = Variable.fromString "name" in
  let locationVar = Variable.fromString "location" in
    
  let companyVar = Variable.fromString "idCompany" in

  (* Atoms *)
  let agencyAtom = Atom.create travelAgencyRS (travelAgencyVar::agencyNameVar::agencyLocationVar::[]) in
  let companyTravelAtom = Atom.create companyRS (companyVar::agencyNameVar::agencyLocationVar::[]) in

  
  let flightAtom = Atom.create fligthRS (idFlightVar::fromVar::toVar::airlineVar::[]) in
  let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

  
  let departureAtom = Atom.create departureRS (fromVar::idFlightVar::companyVar::[]) in
  let arrivalAtom = Atom.create arrivalRS (toVar::idFlightVar::companyVar::[]) in
  let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in


  (* conjunctions *)
  let sourceConjunctionTravelAgency =
    AtomConjunction.add AtomConjunction.empty agencyAtom
  in
  let sourceConjunctionFlight =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
  in

  let targetConjunctionTravelAgency =
            AtomConjunction.add AtomConjunction.empty companyTravelAtom
  in
  
  let targetConjunctionDeparture =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty departureAtom) companyAtom
  in
  let targetConjunctionArrival =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty arrivalAtom) companyAtom
  in

  (* tgds *)
  let tgdTravel = Tgd.create  sourceConjunctionTravelAgency targetConjunctionTravelAgency in
  let tgdDeparture = Tgd.create sourceConjunctionFlight targetConjunctionDeparture in
  let tgdArrival = Tgd.create sourceConjunctionFlight targetConjunctionArrival in

  (* mappings *)
  let mappingRef =
    Mapping.add ( Mapping.add (Mapping.add Mapping.empty tgdTravel) tgdDeparture) tgdArrival
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
    AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef)  sigmaRedCanGLAVsm
  in
  "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
  let (joinRefinedMapping,countJoinQuestions) =
    JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef)  atomRefinedMapping
  in
  "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

  "Framework tests 1" >:::
    [
              "test source1 length" >::
          (fun _ -> assert_equal 5 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 9  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
        
      "test cGsm length" >::
        (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
      "test split red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length splitRedCanGLAVsm));
      "test sigma red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length sigmaRedCanGLAVsm));
      "test atom refined length" >::
        (fun _ -> assert_equal 5 (Mapping.length atomRefinedMapping));
      "test join refined length" >::
        (fun _ -> assert_equal 3 (Mapping.length joinRefinedMapping));
      "test final mapping equivalent to expected" >::
        (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
    ]

let testCaseTDB =
  let examplePath =
    "testFiles/example2.txt";
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

  (* relations symbols *)
  let travelAgencyRS = RelationSymbol.fromString "TravelAgency" in
  let airlineRS = RelationSymbol.fromString "Airline" in
  let fligthRS = RelationSymbol.fromString "Flight" in

  let departureRS = RelationSymbol.fromString "Departure" in
  let arrivalRS = RelationSymbol.fromString "Arrival" in
  let companyRS = RelationSymbol.fromString "Company" in

  (* variables *)
  let travelAgencyVar = Variable.fromString "idAgency" in
  let agencyNameVar = Variable.fromString "agencyName" in
  let agencyLocationVar = Variable.fromString "agencyLocation" in

  let idFlightVar = Variable.fromString "idFlight" in
  let fromVar = Variable.fromString "from" in
  let toVar = Variable.fromString "to" in
  
  let airlineVar = Variable.fromString "idAirline" in
  let airlineNameVar = Variable.fromString "name" in
  let locationVar = Variable.fromString "location" in
    
  let companyVar = Variable.fromString "idCompany" in

  (* Atoms *)
  let agencyAtom = Atom.create travelAgencyRS (travelAgencyVar::agencyNameVar::agencyLocationVar::[]) in
  let companyTravelAtom = Atom.create companyRS (companyVar::agencyNameVar::agencyLocationVar::[]) in

  
  let flightAtom = Atom.create fligthRS (idFlightVar::fromVar::toVar::airlineVar::[]) in
  let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

  
  let departureAtom = Atom.create departureRS (fromVar::idFlightVar::companyVar::[]) in
  let arrivalAtom = Atom.create arrivalRS (toVar::idFlightVar::companyVar::[]) in
  let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in


  (* conjunctions *)
  let sourceConjunctionTravelAgency =
    AtomConjunction.add AtomConjunction.empty agencyAtom
  in
  let sourceConjunctionFlight =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
  in

  let targetConjunctionTravelAgency =
            AtomConjunction.add AtomConjunction.empty companyTravelAtom
  in
  
  let targetConjunctionDeparture =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty departureAtom) companyAtom
  in
  let targetConjunctionArrival =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty arrivalAtom) companyAtom
  in

  (* tgds *)
  let tgdTravel = Tgd.create  sourceConjunctionTravelAgency targetConjunctionTravelAgency in
  let tgdDeparture = Tgd.create sourceConjunctionFlight targetConjunctionDeparture in
  let tgdArrival = Tgd.create sourceConjunctionFlight targetConjunctionArrival in

  (* mappings *)
  let mappingRef =
    Mapping.add ( Mapping.add (Mapping.add Mapping.empty tgdTravel) tgdDeparture) tgdArrival
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
    AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDB")  sigmaRedCanGLAVsm
  in
  "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
  let (joinRefinedMapping,countJoinQuestions) =
    JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDB")  atomRefinedMapping
  in
  "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

  "Framework tests 1" >:::
    [
              "test source1 length" >::
          (fun _ -> assert_equal 5 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 9  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
      "test cGsm length" >::
        (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
      "test split red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length splitRedCanGLAVsm));
      "test sigma red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length sigmaRedCanGLAVsm));
      "test atom refined length" >::
        (fun _ -> assert_equal 5 (Mapping.length atomRefinedMapping));
      "test join refined length" >::
        (fun _ -> assert_equal 3 (Mapping.length joinRefinedMapping));
      "test final mapping equivalent to expected" >::
        (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
    ]


let testCaseBUD =
  let examplePath =
    "testFiles/example2.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** "^examplePath^"*****\n");

    
                             
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in  

  (* relations symbols *)
  let travelAgencyRS = RelationSymbol.fromString "TravelAgency" in
  let airlineRS = RelationSymbol.fromString "Airline" in
  let fligthRS = RelationSymbol.fromString "Flight" in

  let departureRS = RelationSymbol.fromString "Departure" in
  let arrivalRS = RelationSymbol.fromString "Arrival" in
  let companyRS = RelationSymbol.fromString "Company" in

  (* variables *)
  let travelAgencyVar = Variable.fromString "idAgency" in
  let agencyNameVar = Variable.fromString "agencyName" in
  let agencyLocationVar = Variable.fromString "agencyLocation" in

  let idFlightVar = Variable.fromString "idFlight" in
  let fromVar = Variable.fromString "from" in
  let toVar = Variable.fromString "to" in
  
  let airlineVar = Variable.fromString "idAirline" in
  let airlineNameVar = Variable.fromString "name" in
  let locationVar = Variable.fromString "location" in
    
  let companyVar = Variable.fromString "idCompany" in

  (* Atoms *)
  let agencyAtom = Atom.create travelAgencyRS (travelAgencyVar::agencyNameVar::agencyLocationVar::[]) in
  let companyTravelAtom = Atom.create companyRS (companyVar::agencyNameVar::agencyLocationVar::[]) in

  
  let flightAtom = Atom.create fligthRS (idFlightVar::fromVar::toVar::airlineVar::[]) in
  let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

  
  let departureAtom = Atom.create departureRS (fromVar::idFlightVar::companyVar::[]) in
  let arrivalAtom = Atom.create arrivalRS (toVar::idFlightVar::companyVar::[]) in
  let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in


  (* conjunctions *)
  let sourceConjunctionTravelAgency =
    AtomConjunction.add AtomConjunction.empty agencyAtom
  in
  let sourceConjunctionFlight =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
  in

  let targetConjunctionTravelAgency =
            AtomConjunction.add AtomConjunction.empty companyTravelAtom
  in
  
  let targetConjunctionDeparture =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty departureAtom) companyAtom
  in
  let targetConjunctionArrival =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty arrivalAtom) companyAtom
  in

  (* tgds *)
  let tgdTravel = Tgd.create  sourceConjunctionTravelAgency targetConjunctionTravelAgency in
  let tgdDeparture = Tgd.create sourceConjunctionFlight targetConjunctionDeparture in
  let tgdArrival = Tgd.create sourceConjunctionFlight targetConjunctionArrival in

  (* mappings *)
  let mappingRef =
    Mapping.add ( Mapping.add (Mapping.add Mapping.empty tgdTravel) tgdDeparture) tgdArrival
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
    AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "BUD")  sigmaRedCanGLAVsm
  in
  "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
  let (joinRefinedMapping,countJoinQuestions) =
    JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "BUD")  atomRefinedMapping
  in
  "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

  "Framework tests 1" >:::
    [
              "test source1 length" >::
          (fun _ -> assert_equal 5 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 9  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
      "test cGsm length" >::
        (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
      "test split red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length splitRedCanGLAVsm));
      "test sigma red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length sigmaRedCanGLAVsm));
      "test atom refined length" >::
        (fun _ -> assert_equal 5 (Mapping.length atomRefinedMapping));
      "test join refined length" >::
        (fun _ -> assert_equal 3 (Mapping.length joinRefinedMapping));
      "test final mapping equivalent to expected" >::
        (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
    ]

let testCaseTDD =
  let examplePath =
    "testFiles/example2.txt";
  in
  
  Printf.printf "\n***** %s *****\n" examplePath;
  Printf.printf "\t Cf. Log file\n";
  Bolt.Logger.log "log" Bolt.Level.TRACE ("\n***** "^examplePath^"*****\n");

    
                             
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let canonicalGLAVmapping =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in  

  (* relations symbols *)
  let travelAgencyRS = RelationSymbol.fromString "TravelAgency" in
  let airlineRS = RelationSymbol.fromString "Airline" in
  let fligthRS = RelationSymbol.fromString "Flight" in

  let departureRS = RelationSymbol.fromString "Departure" in
  let arrivalRS = RelationSymbol.fromString "Arrival" in
  let companyRS = RelationSymbol.fromString "Company" in

  (* variables *)
  let travelAgencyVar = Variable.fromString "idAgency" in
  let agencyNameVar = Variable.fromString "agencyName" in
  let agencyLocationVar = Variable.fromString "agencyLocation" in

  let idFlightVar = Variable.fromString "idFlight" in
  let fromVar = Variable.fromString "from" in
  let toVar = Variable.fromString "to" in
  
  let airlineVar = Variable.fromString "idAirline" in
  let airlineNameVar = Variable.fromString "name" in
  let locationVar = Variable.fromString "location" in
    
  let companyVar = Variable.fromString "idCompany" in

  (* Atoms *)
  let agencyAtom = Atom.create travelAgencyRS (travelAgencyVar::agencyNameVar::agencyLocationVar::[]) in
  let companyTravelAtom = Atom.create companyRS (companyVar::agencyNameVar::agencyLocationVar::[]) in

  
  let flightAtom = Atom.create fligthRS (idFlightVar::fromVar::toVar::airlineVar::[]) in
  let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in

  
  let departureAtom = Atom.create departureRS (fromVar::idFlightVar::companyVar::[]) in
  let arrivalAtom = Atom.create arrivalRS (toVar::idFlightVar::companyVar::[]) in
  let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in


  (* conjunctions *)
  let sourceConjunctionTravelAgency =
    AtomConjunction.add AtomConjunction.empty agencyAtom
  in
  let sourceConjunctionFlight =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty flightAtom) airlineAtom
  in

  let targetConjunctionTravelAgency =
            AtomConjunction.add AtomConjunction.empty companyTravelAtom
  in
  
  let targetConjunctionDeparture =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty departureAtom) companyAtom
  in
  let targetConjunctionArrival =
        AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty arrivalAtom) companyAtom
  in

  (* tgds *)
  let tgdTravel = Tgd.create  sourceConjunctionTravelAgency targetConjunctionTravelAgency in
  let tgdDeparture = Tgd.create sourceConjunctionFlight targetConjunctionDeparture in
  let tgdArrival = Tgd.create sourceConjunctionFlight targetConjunctionArrival in

  (* mappings *)
  let mappingRef =
    Mapping.add ( Mapping.add (Mapping.add Mapping.empty tgdTravel) tgdDeparture) tgdArrival
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
    AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDD")  sigmaRedCanGLAVsm
  in
  "\n\n atom refined mapping:\n" ^ (Mapping.toString atomRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

    
  let (joinRefinedMapping,countJoinQuestions) =
    JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:(Some mappingRef) ?strategy:(Some "TDD")  atomRefinedMapping
  in
  "\n\n join refined mapping:\n" ^ (Mapping.toString joinRefinedMapping)
  |> Bolt.Logger.log "log" Bolt.Level.TRACE ;

  "Framework tests 1" >:::
    [
              "test source1 length" >::
          (fun _ -> assert_equal 5 (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.lhs)
                                   )
          );
        "test target1 length" >::
          (fun _ -> assert_equal 9  (AtomConjunction.length (Mapping.choose_exn canonicalGLAVmapping
                                                            |> Tgd.rhs)
                                   )
          );
      "test cGsm length" >::
        (fun _ -> assert_equal 1 (Mapping.length canonicalGLAVmapping));
      "test split red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length splitRedCanGLAVsm));
      "test sigma red cGsm length" >::
        (fun _ -> assert_equal 5 (Mapping.length sigmaRedCanGLAVsm));
      "test atom refined length" >::
        (fun _ -> assert_equal 5 (Mapping.length atomRefinedMapping));
      "test join refined length" >::
        (fun _ -> assert_equal 3 (Mapping.length joinRefinedMapping));
      "test final mapping equivalent to expected" >::
        (fun _ -> assert_equal true (Chase.equivMappings mappingRef joinRefinedMapping));
    ]
