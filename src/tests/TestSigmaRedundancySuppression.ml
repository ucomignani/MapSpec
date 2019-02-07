open Core.Std
open OUnit2

open GeneralStruct
open Mapping
       
open Morphism
       
open Preprocessing

let sub_testSigmaRedundancySuppression mapping =
  Printf.printf "\nTest : ";
  Mapping.print mapping;
  Printf.printf "\n";
  let resMapping =
    Preprocessing.sigmaRedundancySuppression mapping
  in
  Mapping.print resMapping;
  resMapping

    
let testSigmaRedundancySuppression =
      Printf.printf "\n*********************************************** Tests sigma redundancy suppression ***********************************************";

  let symbAtS = RelationSymbol.fromString "S" in
  let symbAtT = RelationSymbol.fromString "T" in

  let var1 = Variable.fromString "v1" in
  let var2 = Variable.fromString "v2" in
  let var3 = Variable.fromString "v3" in
  let var4 = Variable.fromString "v4" in
  


  let atomS1 = Atom.create symbAtS (var1::var2::[]) in
  let atomT1 = Atom.create symbAtT (var1::var2::[]) in

  let atomS2 = Atom.create symbAtS (var3::var4::[]) in
  let atomT2 = Atom.create symbAtT (var3::var4::[]) in

  let atomT3 = Atom.create symbAtT (var3::var3::[]) in

  let refSourceConjunction = AtomConjunction.add (
                                 AtomConjunction.add AtomConjunction.empty atomS1
                               ) atomT1
  in

  let refSourceConjunction2 =
    AtomConjunction.add AtomConjunction.empty atomS1
  in

  let refTargetConjunction = AtomConjunction.add (
                                 AtomConjunction.add AtomConjunction.empty atomS2
                               ) atomT2
  in

  let refTargetConjunction2 = AtomConjunction.add (
                                  AtomConjunction.add (
                                      AtomConjunction.add AtomConjunction.empty atomS2
                                    ) atomT2
                                ) atomT3
  in

  let refTargetConjunction3 =
    AtomConjunction.add AtomConjunction.empty atomS2
  in

  let tgd1 =
    Tgd.create refSourceConjunction refTargetConjunction
  in
  let tgd2 =
    Tgd.create refTargetConjunction refSourceConjunction
  in

  let tgd3 =
    Tgd.create refSourceConjunction refTargetConjunction2
  in

  let tgd4 =
    Tgd.create refSourceConjunction2 refTargetConjunction
  in

  let tgd5 =
    Tgd.create refSourceConjunction2 refTargetConjunction3
  in

  let tgd6 =
    Tgd.create refSourceConjunction2 refTargetConjunction2
  in

  let mapping1 =
    Mapping.add (
        Mapping.add Mapping.empty tgd1
      ) tgd2
  in
  
  let mapping2 =
    Mapping.add (
        Mapping.add Mapping.empty tgd1
      ) tgd3
  in
  
  let mapping3 =
    Mapping.add (
        Mapping.add Mapping.empty tgd1
      ) tgd4
  in

  let mapping4 =
    Mapping.add (
        Mapping.add Mapping.empty tgd1
      ) tgd5
  in

  let mapping5 =
    Mapping.add (
        Mapping.add Mapping.empty tgd1
      ) tgd6
  in
  
  let sub1 =
    sub_testSigmaRedundancySuppression mapping1
  in

  let sub2 =
    sub_testSigmaRedundancySuppression mapping2
  in

  let sub3 =
    sub_testSigmaRedundancySuppression mapping3
  in
  
  let sub4 =
    sub_testSigmaRedundancySuppression mapping4
  in

  let sub5 =
    sub_testSigmaRedundancySuppression mapping5
  in
  
  "Sigma redundancy suppression tests" >:::
    [
      "Mapping 1 size test: " >::
        (fun _ -> assert_equal (Mapping.length sub1) 1 );
      "Mapping 2 size test: " >::
        (fun _ -> assert_equal (Mapping.length sub2) 1 );
      "Mapping 3 size test: " >::
        (fun _ -> assert_equal (Mapping.length sub3) 1 );
      "Mapping 4 size test: " >::
        (fun _ -> assert_equal (Mapping.length sub4) 2 );
      "Mapping 5 size test: " >::
        (fun _ -> assert_equal (Mapping.length sub5) 1 );
      
    ]
