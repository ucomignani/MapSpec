open Core.Std

let applyTgdToConjunction tgd atomConjunction =
  let lhs =
    Mapping.Tgd.lhs tgd
  in
  Morphism.expandMorphismToAtomConjunctionsPair Morphism.Morphism.empty lhs atomConjunction

let applyMorphismToAtom prefix morphism atom =
  Mapping.Atom.getVarsList atom
  |> List.map ~f:(fun var -> let varMorphism =
                               Morphism.Morphism.find morphism var
                             in
                             let newVar =
                               (* if the variable is in the morphism we do the substitution, else a fresh variable is generated*)
                               match varMorphism with
                               | None -> (GeneralStruct.Variable.toString var) ^ prefix
                                         |> GeneralStruct.Variable.fromString
                               | Some data -> data
                             in
                             newVar
                 )
  |> Mapping.Atom.create (Mapping.Atom.getRelationSymbol atom)
                                                                
let applyMorphismToConjunction prefix morphism conjunction =
  conjunction
  |> Mapping.AtomConjunction.map ~f:(fun atom -> applyMorphismToAtom prefix morphism atom)
                                                     
let chaseAtomConjunction mapping sourceConjunction =
  mapping
  |> Mapping.Mapping.fold
       ~f:(fun (counter,accum) tgd -> let (counterTmp,accumTmp) =
                                        applyTgdToConjunction tgd sourceConjunction
                                        |> Morphism.MorphismS.fold ~f:(fun (counter2,resConjunction) morphism ->
                                                                     let prefix =
                                                                       "_"
                                                                       ^ (Int.to_string counter)
                                                                       ^ "_"
                                                                       ^ (Int.to_string counter2)
                                                                     in
                                                                     let newConjunction =
                                                                       applyMorphismToConjunction prefix morphism (Mapping.Tgd.rhs tgd)
                                                                     in
                                                                     (counter2 + 1, Mapping.AtomConjunction.union resConjunction newConjunction)
                                                                   )
                                                                   ~init:(0,Mapping.AtomConjunction.empty)
                                      in
                                      (counterTmp + 1, Mapping.AtomConjunction.union accum accumTmp)
          )
       ~init: (0,Mapping.AtomConjunction.empty)
  |> snd

                                                            
let chaseAtomConjunctionWithTgd tgd sourceConjunction =
  applyTgdToConjunction tgd sourceConjunction
  |> Morphism.MorphismS.fold ~f:(fun (counter,resConjunction) morphism ->
                               let prefix =
                                 "_"
                                 ^ (Int.to_string counter)
                               in
                               let newConjunction =
                                 applyMorphismToConjunction prefix morphism (Mapping.Tgd.rhs tgd)
                               in
                               (counter + 1, Mapping.AtomConjunction.union resConjunction newConjunction)
                             )
                             ~init:(0,Mapping.AtomConjunction.empty)
  |> snd

                                                                
       

let equivConjunctions conjunction1 conjunction2 =                                                      
  not (Morphism.expandMorphismToAtomConjunctionsPair Morphism.Morphism.empty conjunction1 conjunction2
       |> Morphism.MorphismS.is_empty)
  &&
    not (Morphism.expandMorphismToAtomConjunctionsPair  Morphism.Morphism.empty conjunction2 conjunction1
         |> Morphism.MorphismS.is_empty)

let subsummedTgds tgd1 tgd2 =
  let lhs =
    Mapping.Tgd.lhs
  in
  let rhs =
    Mapping.Tgd.rhs
  in
  let tgdExistVars =
    Mapping.Tgd.extractExistentiallyQuantifiedVariables tgd1
  in
  let resChase =
    ref (chaseAtomConjunctionWithTgd tgd2 (lhs tgd1))
  in
  if  (Mapping.AtomConjunction.length (lhs tgd1)) < (Mapping.AtomConjunction.length (lhs tgd2)) (* not a necessary condition, just a small optimisation *)
  then false
  else
    Mapping.Tgd.rhs tgd1
    |> Mapping.AtomConjunction.for_all
         ~f:(fun atomRef -> !resChase
                            |> Mapping.AtomConjunction.exists
                                 ~f:(fun atomChase -> if not ((Mapping.Atom.getRelationSymbol atomChase) =  (Mapping.Atom.getRelationSymbol atomRef))
                                                         ||
                                                           ((Mapping.Atom.varsNumber atomChase) <> (Mapping.Atom.varsNumber atomRef))
                                                      then false
                                                      else
                                                        (
                                                          List.zip_exn
                                                            (Mapping.Atom.getVarsList atomChase)
                                                            (Mapping.Atom.getVarsList atomRef)
                                                          |> List.for_all ~f:(fun (varChase,varRef) -> let isExistantial =
                                                                                                         let resFind =
                                                                                                           GeneralStruct.VariableS.find
                                                                                                             ~f:(fun existVar -> varRef = existVar)
                                                                                                             tgdExistVars
                                                                                                         in
                                                                                                         match resFind with
                                                                                                         | None -> false
                                                                                                         | Some _ -> true
                                                                                                       in
                                                                                                       let res =
                                                                                                         isExistantial
                                                                                                         ||
                                                                                                           varChase = varRef
                                                                                                       in
                                                                                                       res
                                                                             )
                                                        )
                                    )
            )
        
let equivTgds tgd1 tgd2 =
  let res =
    (subsummedTgds tgd1 tgd2)
    &&
      (subsummedTgds tgd2 tgd1)
  in
  res

  
let equivMappings mapping1 mapping2 =
  mapping1
  |> Mapping.Mapping.for_all ~f:(fun tgd1 -> mapping2
                                             |> Mapping.Mapping.exists ~f:(fun tgd2 -> equivTgds tgd1 tgd2))
  &&
  mapping2
  |> Mapping.Mapping.for_all ~f:(fun tgd2 -> mapping1
                                             |> Mapping.Mapping.exists ~f:(fun tgd1 -> equivTgds tgd1 tgd2))

         
