open Core.Std
open OUnit2

open GeneralStruct
open Mapping
open Morphism

let testMorphismSAtoms =
    Printf.printf "\n*********************************************** Tests morphisms atoms ***********************************************";

  let symbAtS = RelationSymbol.fromString "S" in
  let varS1 = Variable.fromString "vS1" in
  let varS2 = Variable.fromString "vS2" in
  let varS3 = Variable.fromString "vS3" in
  let varS4 = Variable.fromString "vS4" in
  
  let symbAtT = RelationSymbol.fromString "T" in
  let varT1 = Variable.fromString "vT1" in
  let varT2 = Variable.fromString "vT2" in

  let atomReference = Atom.create symbAtS (varS1::varS2::[]) in
  let coherentAtom = Atom.create symbAtS (varS3::varS4::[]) in

  let atomWithDifferentRelationSymbol = Atom.create symbAtT (varT1::varT2::[]) in
  let validAtom = Atom.create symbAtS (varT1::varT2::[]) in
  let atomToAddIncoherence = Atom.create symbAtS (varT2::varT1::[]) in

  (* Case with atoms with different relations symbols *)
  let morphismSatomWithDifferentRelationSymbol =
    expandMorphismToAtomsPair Morphism.empty atomReference atomWithDifferentRelationSymbol
  in

  (* Case with atoms with coherent mapping beetween their variables *)
  Printf.printf "\nValid : ";
  Atom.print atomReference; Printf.printf " ->"; Atom.print validAtom;
  Printf.printf " and ";
  Atom.print coherentAtom; Printf.printf " ->"; Atom.print validAtom;
  Printf.printf "\n";
  let morphismSValidAtom =
    let baseMorphism =
      List.hd_exn (expandMorphismToAtomsPair Morphism.empty atomReference validAtom)
    in
    List.hd_exn (expandMorphismToAtomsPair baseMorphism coherentAtom validAtom)
  in
  Morphism.print morphismSValidAtom;

  (* Case with  atoms without coherent mapping between their variables. 
The mapping is intended to be constructed with the fist pair of atoms, 
and second pair of atoms (which add incoherence) is intended to be ignored *)
  Printf.printf "\nWith incoherence: ";
  Atom.print atomReference; Printf.printf " ->"; Atom.print validAtom;
  Printf.printf " and ";
  Atom.print atomReference; Printf.printf " ->"; Atom.print atomToAddIncoherence;
  Printf.printf "\n(incoherences are supposed to be ignored so don't appears)\n";
  let baseMorphism =
    List.hd_exn (expandMorphismToAtomsPair Morphism.empty atomReference validAtom)
  in
  let morphismSWithIncoherentAtomExcluded =
    expandMorphismToAtomsPair baseMorphism atomReference atomToAddIncoherence
  in
  Morphism.print baseMorphism;
  
  "Morphism tests" >:::
    [
    "test morphism content with atomWithDifferentRelationSymbol" >::
      (fun _ -> assert_equal (List.is_empty morphismSatomWithDifferentRelationSymbol) true );
    

    "test morphism content with validAtom" >::
      (fun _ -> assert_equal (Morphism.length morphismSValidAtom) 4 );


    "test morphism content with incoherence" >::
      (fun _ -> assert_equal (List.is_empty morphismSWithIncoherentAtomExcluded) true );
    ]
