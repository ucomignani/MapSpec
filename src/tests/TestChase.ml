open Core.Std
open OUnit2

open GeneralStruct
open Mapping

open Chase

let sub_testChase mapping source =
  let chaseResult =
    chaseAtomConjunction mapping source
    in
    
  Printf.printf "\nResult of chasing: \n";
  AtomConjunction.print source;
  Printf.printf "\nwith : \n";
  Mapping.print mapping;
  Printf.printf "\n";
  
  chaseResult
  |> AtomConjunction.print ;
  Printf.printf "\n";

  chaseResult

let testChase examplePath =

  Printf.printf "\n*********************************************** Tests chase ***********************************************";
  
  let dataExSMappingPair =
    open_in examplePath
    |> Lexing.from_channel
    |> Parser.main Lexer.token
  in
  let dataExampleS =
    dataExSMappingPair
    |> MappingPair.getCanonicalMapping
  in
  let source =
    Mapping.choose_exn dataExampleS
    |> Tgd.lhs
  in    

  let fligthRS = RelationSymbol.fromString "Flight" in
  let fromVar = Variable.fromString "from" in
  let toVar = Variable.fromString "to" in
  let airlineVar = Variable.fromString "idAirline" in
  
  let airlineRS = RelationSymbol.fromString "Airline" in
  let airlineNameVar = Variable.fromString "name" in
  let locationVar = Variable.fromString "location" in

  let travelAgencyRS = RelationSymbol.fromString "TravelAgency" in
  let travelAgencyVar = Variable.fromString "idAgency" in
  let agencyNameVar = Variable.fromString "agencyName" in
  let agencyLocationVar = Variable.fromString "agencyLocation" in
    
  let departureRS = RelationSymbol.fromString "Departure" in

  let arrivalRS = RelationSymbol.fromString "Arrival" in

  let companyRS = RelationSymbol.fromString "Company" in
  let companyVar = Variable.fromString "idCompany" in

  let flightAtom = Atom.create fligthRS (fromVar::toVar::airlineVar::[]) in
  let airlineAtom = Atom.create airlineRS (airlineVar::airlineNameVar::locationVar::[]) in
  let agencyAtom = Atom.create travelAgencyRS (travelAgencyVar::agencyNameVar::agencyLocationVar::[]) in

  let departureAtom = Atom.create departureRS (fromVar::companyVar::[]) in
  let arrivalAtom = Atom.create arrivalRS (toVar::companyVar::[]) in
  let companyAtom = Atom.create companyRS (companyVar::airlineNameVar::locationVar::[]) in
  let company2Atom = Atom.create companyRS (companyVar::agencyNameVar::agencyLocationVar::[]) in

  
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

  let sourceConjunction2 =
    AtomConjunction.add AtomConjunction.empty agencyAtom
  in
  let targetConjunction2 =
    AtomConjunction.add AtomConjunction.empty company2Atom
  in

  let tgd1 = Tgd.create sourceConjunction1 targetConjunction1 in
  let tgd2 = Tgd.create sourceConjunction2 targetConjunction2 in

  let mapping1 =
    Mapping.add (Mapping.add Mapping.empty tgd1) tgd2
  in
  let chaseResult1 =
    sub_testChase mapping1 source
  in

  
  
  "Chase tests" >:::
    [
      "test Chase: " >::
        (fun _ -> assert_equal (AtomConjunction.length chaseResult1) 7 );
    
    ]
