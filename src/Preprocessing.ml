open Core.Std

open Mapping
open GeneralStruct

let rec insertAtoms atomS atomSetsList =
  match atomSetsList with
  | [] -> atomS::[]
  | hd::tl -> if (AtomConjunction.exists
                    ~f:(fun atomRes -> AtomConjunction.exists
                                         ~f:(fun atom -> atomRes = atom)
                                         hd
                       )
                    atomS
                 )
              then (AtomConjunction.union atomS hd
                   )::tl
              else hd::(insertAtoms atomS tl)

let rec splitReduction_generateSplitReducedRhsList varToRhsAtoms =
  match varToRhsAtoms with
  | [] -> []
  | hd::tl -> let (_,atomS) = hd in
              insertAtoms atomS (splitReduction_generateSplitReducedRhsList tl)         

let splitReduction existantialVarsList mapping =
  mapping
  |> Mapping.fold ~f:(fun accum tgd ->
                    let lhs = Tgd.lhs tgd in
                    let rhs = Tgd.rhs tgd in
                    let tgdsWithoutExistantialVariable =
                      rhs
                      |> AtomConjunction.filter ~f:(fun atom -> existantialVarsList
                                                                |> List.for_all ~f:(fun var -> not (Atom.hasVar atom var))
                                                   )
                      |> AtomConjunction.fold ~f:(fun accum2 atom -> let newConjunction =
                                                                      atom
                                                                      |> AtomConjunction.add AtomConjunction.empty
                                                                    in
                                                                    newConjunction
                                                                    |> Tgd.create lhs
                                                 |> Mapping.add accum2
                                              )
                                              ~init:Mapping.empty
                    in
                    
                    existantialVarsList
                    |> List.map ~f:(fun var -> let atomS =
                                                 AtomConjunction.filter ~f:(fun atom -> Atom.hasVar atom var)
                                                                        rhs
                                               in
                                               (var,atomS)
                                   )
                    |> splitReduction_generateSplitReducedRhsList
                    |> List.fold ~f:(fun accum splitReducedRhs -> Mapping.add accum (Tgd.create lhs splitReducedRhs))
                                 ~init:Mapping.empty
                    |> Mapping.union accum
                    |> Mapping.union tgdsWithoutExistantialVariable
                  )
                  ~init:Mapping.empty

let sigmaRedundancySuppression mapping =
  mapping
  |> Mapping.fold ~f:(fun accum tgd -> let mappingTmp =
                                         Mapping.remove accum tgd
                                       in
                                       let toSuppress =
                                         mappingTmp
                                         |> Mapping.exists ~f:(fun tgdComp ->
                                                             Chase.subsummedTgds tgd tgdComp
                                                           )
                                       in
                                       if toSuppress
                                       then mappingTmp
                                       else accum
                     )
                  ~init:mapping
    
let normalizeMapping existantialVarsList mapping =
  mapping
  |> splitReduction existantialVarsList
  |> sigmaRedundancySuppression                    
