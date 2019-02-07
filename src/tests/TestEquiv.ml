open Core.Std
open OUnit2

open GeneralStruct
open Mapping

open Chase

let testEquiv =

  Printf.printf "\n*********************************************** Tests equivs ***********************************************";
  
  let symbAtS = RelationSymbol.fromString "S" in
  let varS1 = Variable.fromString "vS1" in
  let varS2 = Variable.fromString "vS2" in
  let varS3 = Variable.fromString "vS3" in
  let varS4 = Variable.fromString "vS4" in
  
  let symbAtT = RelationSymbol.fromString "T" in
  let varT1 = Variable.fromString "vT1" in
  let varT2 = Variable.fromString "vT2" in
  let varT3 = Variable.fromString "vT3" in
  let varT4 = Variable.fromString "vT4" in

  let varExist1 = Variable.fromString "vE1" in
  let varExist2 = Variable.fromString "vE2" in
  

  let at1 = Atom.create symbAtT (varT1::varT2::[]) in
  let at2 = Atom.create symbAtT (varT3::varT4::[]) in
  let at3 = Atom.create symbAtS (varS1::varS2::[]) in
  let at4 = Atom.create symbAtS (varS3::varS4::[]) in
  let at5 = Atom.create symbAtS (varS3::varS3::[]) in 
  let at6 = Atom.create symbAtS (varExist1::varT1::varS1::[]) in
  let at7 = Atom.create symbAtS (varExist2::varT3::varS3::[]) in
  let at8 = Atom.create symbAtS (varExist2::varT4::varS3::[]) in

  (* conjunctions *)
  let conjunctionRef =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty at1) at3
  in
  let conjunctionEquiv =
    AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty at2) at4
  in

  let conjunctionNotEquiv =
    AtomConjunction.add ( AtomConjunction.add AtomConjunction.empty at2) at5
  in

  let targetConjunctionRef =
    AtomConjunction.add AtomConjunction.empty at6
  in
  let targetConjunctionEquiv =
    AtomConjunction.add AtomConjunction.empty at7
  in
  let targetConjunctionNotEquiv =
    AtomConjunction.add AtomConjunction.empty at8
  in
  
  (* tgds *)
  let tgdRef = Tgd.create conjunctionRef targetConjunctionRef in
  let tgdEquiv = Tgd.create  conjunctionEquiv targetConjunctionEquiv in
  let tgdNotEquiv = Tgd.create  conjunctionEquiv targetConjunctionNotEquiv in

  (* mappings *)
  let equivConjunctionsTrue =
    Chase.equivConjunctions conjunctionRef conjunctionEquiv
  in
  let equivConjunctionsFalse =
    Chase.equivConjunctions conjunctionRef conjunctionNotEquiv
  in

  let equivTgdTrue =
    Chase.equivTgds tgdRef tgdEquiv
  in

  let equivTgdFalse =
    Chase.equivTgds tgdRef tgdNotEquiv
  in
  
  "Equiv tests" >:::
    [
      "test equiv conjunctions true: " >::
        (fun _ -> assert_equal equivConjunctionsTrue true );
      
      "test equiv conjunctions false: " >::
        (fun _ -> assert_equal equivConjunctionsFalse false );

      "test equiv tgd true: " >::
        (fun _ -> assert_equal equivTgdTrue true );
      
      "test equiv tgd false: " >::
        (fun _ -> assert_equal equivTgdFalse false );
    ]
