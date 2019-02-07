open Core.Std

open GeneralStruct

       
module Morphism =
  struct
    module M = Map.Make(Variable)
    type t = Variable.t M.t

    let empty = M.empty
                  
    let create = M.singleton
                   
    let add = M.add

    let find = M.find

    let find_exn = M.find_exn
                     
    let keys = M.keys

    let data = M.data
                 
    let getDomain morphism =
      VariableS.of_list (keys morphism)
                        
    let getCodomain morphism =
      VariableS.of_list (data morphism)
                        
    let length = M.length
                        
    let for_all = M.for_all
                        
    let compare = M.compare_direct Variable.compare

    let print morphism =
      if M.is_empty morphism
      then
        Printf.printf "empty morphism\n"
      else
        M.to_alist morphism
        |> List.iter ~f:(fun pair -> let (key,data) =
                                       pair
                                     in 
                                     Variable.print key;
                                     Printf.printf " -> ";
                                     Variable.print data;
                                     Printf.printf "\n"
                      )
    let sexp_of_t morphism =
      (VariableS.fold ~f:(fun accum var ->(Variable.toString var) ^ accum)
                      ~init:""
                      (getDomain morphism) )
      ^(VariableS.fold ~f:(fun accum var ->(Variable.toString var) ^ accum)
                      ~init:""
                      (getCodomain morphism) )
      |> Sexp.of_string
      
    let t_of_sexp sexp =
        of_sexp_error "Variables morphism error. " sexp
  end

module MorphismS = Set.Make(Morphism)
                           
let printMorphismS morphismS =
  if MorphismS.is_empty morphismS
  then
    Printf.printf "empty set\n"
  else
    MorphismS.iter ~f:(fun morphism -> Morphism.print morphism; Printf.printf "\n")  morphismS
                   
let expandMorphismToAtomsPair morphism fromAtom toAtom =
    let varsSourceAtom =
      Mapping.Atom.getVarsList fromAtom
    in
    let varsTargetAtom =
      Mapping.Atom.getVarsList toAtom
    in

    (* are the two relations symbols and variables number the same*)
  if ( (Mapping.Atom.getRelationSymbol fromAtom)
       = (Mapping.Atom.getRelationSymbol toAtom) )
     &&
       ((List.length varsSourceAtom)
        = (List.length varsTargetAtom))
  then
    let pairSourceTargetVar =
      List.zip_exn varsSourceAtom varsTargetAtom
    in
    let checkCoherence actualMorphism key data =
      let findKey =
        Morphism.find actualMorphism key
      in
      match findKey with
      | None -> true
      | Some dataInMorphism -> if dataInMorphism = data
                               then true
                               else false
      in
    (* if two morphisms with the same source then there's an incoherence *)
    let containIncoherence =
      not (List.for_all ~f:(fun (key,data) -> checkCoherence morphism key data) pairSourceTargetVar
          )
    in
    if containIncoherence
    then
      []
        
    else
      let (coherence,newMorphism) =
        pairSourceTargetVar
        |> List.fold ~f:(fun (coherence,morphismTmp) varPair -> if not coherence
                                                                   ||
                                                                     not (checkCoherence morphismTmp (fst varPair) (snd varPair))
                                                                then (false,morphismTmp)
                                                                else (true,
                                                                      (Morphism.add morphismTmp ~key:(fst varPair) ~data:(snd varPair)) )
                        )
                     ~init:(true,morphism)
      in
      if coherence
      then newMorphism::[]
      else []          
  else
    []

let rec expandMorphismToAtomConjunctionsPair morphism fromCQ toCQ =
  if Mapping.AtomConjunction.isEmpty fromCQ
  then
    MorphismS.add MorphismS.empty morphism
  else
    sub_expandMorphismToAtomConjunctionsPair morphism fromCQ toCQ


and sub_expandMorphismToAtomConjunctionsPair morphism fromCQ toCQ =
  fromCQ
  |> Mapping.AtomConjunction.choose_exn
  |> fun atom -> toCQ (* find correspondings atom in toCQ *)
                 |> Mapping.AtomConjunction.fold ~f:(fun accum atom2 -> (* explore the possible valids morphism for each subtrees *)
                                                   let resultingMorphismList =
                                                     expandMorphismToAtomsPair morphism atom atom2
                                                   in
                                                   if List.is_empty resultingMorphismList
                                                   then accum (* no morphism *)
                                                   else let fromCQ2 =
                                                          Mapping.AtomConjunction.dropAtom fromCQ atom
                                                        in
                                                        let toCQ2 =
                                                          Mapping.AtomConjunction.dropAtom toCQ atom2
                                                        in                                                          
                                                        expandMorphismToAtomConjunctionsPair
                                                          (List.hd_exn resultingMorphismList)
                                                          fromCQ2 toCQ2
                                                        |> MorphismS.union accum )
                                                 ~init:MorphismS.empty
