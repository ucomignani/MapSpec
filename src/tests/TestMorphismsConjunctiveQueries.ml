open Core.Std
open OUnit2

open GeneralStruct
open Mapping
open Morphism

let valid_testMorphismSConjunctiveQuery refSourceConjunction refTargetConjunction =
  Printf.printf "\nValid : ";
  AtomConjunction.print refSourceConjunction; Printf.printf " ->"; AtomConjunction.print refTargetConjunction;
  Printf.printf "\n";
  let morphismSValidAtom =
    expandMorphismToAtomConjunctionsPair Morphism.empty refSourceConjunction refTargetConjunction
  in
  printMorphismS morphismSValidAtom;
  morphismSValidAtom

let invalid_testMorphismSConjunctiveQuery refSourceConjunction refTargetConjunction =
  Printf.printf "\nInvalid : ";
  AtomConjunction.print refSourceConjunction; Printf.printf " ->"; AtomConjunction.print refTargetConjunction;
  Printf.printf "\n";
  let morphismSValidAtom =
    expandMorphismToAtomConjunctionsPair Morphism.empty refTargetConjunction refSourceConjunction
  in
  printMorphismS morphismSValidAtom;
  morphismSValidAtom

let testMorphismSConjunctiveQuery =
      Printf.printf "\n*********************************************** Tests morphisms conjunctions ***********************************************";
  let symbAtS = RelationSymbol.fromString "S" in
  let varS1 = Variable.fromString "vS1" in
  let varS2 = Variable.fromString "vS2" in
  let varS3 = Variable.fromString "vS3" in
  let varS4 = Variable.fromString "vS4" in
  
  let symbAtT = RelationSymbol.fromString "T" in
  let varT1 = Variable.fromString "vT1" in
  let varT2 = Variable.fromString "vT2" in

  let atomReference = Atom.create symbAtS (varS1::varS2::[]) in
  let coherentAtom = Atom.create symbAtT (varS3::varS4::[]) in
  let incoherentAtom = Atom.create symbAtS (varS2::varS4::[]) in

  let atomWithDifferentRelationSymbol = Atom.create symbAtT (varT1::varT2::[]) in
  let validAtom = Atom.create symbAtT (varT1::varT2::[]) in
  let validAtom2 = Atom.create symbAtS (varT2::varT1::[]) in
  let validAtom3 = Atom.create symbAtS (varT1::varT2::[]) in
  let atomToAddIncoherence = Atom.create symbAtS (varT2::varT1::[]) in

  
  let refSourceConjunction = AtomConjunction.add AtomConjunction.empty atomReference in
  let refSourceConjunction2 =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty atomReference) coherentAtom
  in
  let incoherentSourceConjunction =
    AtomConjunction.add (AtomConjunction.add AtomConjunction.empty atomReference) incoherentAtom
  in
  
  let conjunctionWithDifferentRelationSymbol = AtomConjunction.add AtomConjunction.empty atomWithDifferentRelationSymbol in
  let validTargetConjunction =  AtomConjunction.add( AtomConjunction.add AtomConjunction.empty validAtom ) validAtom2 in
  let validTargetConjunction2 =  AtomConjunction.add( AtomConjunction.add AtomConjunction.empty validAtom2 ) validAtom3 in
  let invalidTargetConjunction =
     AtomConjunction.add (AtomConjunction.add (AtomConjunction.add AtomConjunction.empty validAtom) atomToAddIncoherence) coherentAtom
  in
  
  (* Case with atoms with coherent mapping beetween their variables *)
  let morphismSValidAtom =
    valid_testMorphismSConjunctiveQuery  refSourceConjunction validTargetConjunction
  in
  let morphismSValidAtom2 =
    valid_testMorphismSConjunctiveQuery incoherentSourceConjunction validTargetConjunction2
  in
  let morphismSValidAtom3 =
    valid_testMorphismSConjunctiveQuery refSourceConjunction2 validTargetConjunction
  in
  let morphismSValidAtom4 =
    valid_testMorphismSConjunctiveQuery refSourceConjunction invalidTargetConjunction
  in  
  
  (* Case with  atoms without coherent mapping between their variables. 
The mapping is intended to be constructed with the fist pair of atoms, 
and second pair of atoms (which add incoherence) is intended to be ignored *)

  let morphismSInvalidAtom =
    invalid_testMorphismSConjunctiveQuery incoherentSourceConjunction validTargetConjunction
  in

  let morphismSConjunctionsWithDifferentRelationSymbol =
    invalid_testMorphismSConjunctiveQuery refSourceConjunction conjunctionWithDifferentRelationSymbol
  in
  
  "Morphism tests" >:::
    [
      "test morphism content with validConjunction: size morphism set" >::
        (fun _ -> assert_equal (MorphismS.length morphismSValidAtom) 1 );
      "test morphism content with validConjunction: size morphism" >::
        (fun _ -> assert_equal (Morphism.length (MorphismS.choose_exn morphismSValidAtom) ) 2 );
      
      "test morphism content with validConjunction2: size morphism set" >::
        (fun _ -> assert_equal (MorphismS.length morphismSValidAtom2) 2 );
      "test morphism content with validConjunction2: size morphism" >::
        (fun _ -> assert_equal (Morphism.length (MorphismS.choose_exn morphismSValidAtom2) ) 3 );
      
      "test morphism content with validConjunction3: size morphism set" >::
        (fun _ -> assert_equal (MorphismS.length morphismSValidAtom3) 1 );
      "test morphism content with validConjunction3: size morphism" >::
        (fun _ -> assert_equal (Morphism.length (MorphismS.choose_exn morphismSValidAtom3) ) 4 );

      "test morphism content with validConjunction4: size morphism set" >::
        (fun _ -> assert_equal (MorphismS.length morphismSValidAtom4) 1 );
      "test morphism content with validConjunction4: size morphism" >::
        (fun _ -> assert_equal (Morphism.length (MorphismS.choose_exn morphismSValidAtom4) ) 2 );
      
      "test morphism content with incoherence" >::
        (fun _ -> assert_equal (MorphismS.is_empty morphismSInvalidAtom) true );
      
      "test morphism content with conjunctionWithDifferentRelationSymbol" >::
        (fun _ -> assert_equal (MorphismS.is_empty morphismSConjunctionsWithDifferentRelationSymbol) true );
    
    ]
