open Core.Std

open GeneralStruct
       
module Atom =
  struct  
    type t = RelationSymbol.t * Variable.t list

    let getRelationSymbol (relationSymbol,_) =
      relationSymbol
                                           
    let getVarsList (_,varsList) =
      varsList
      
    let create relationSymbol variablesList =
      (relationSymbol,variablesList)

    let rename (relationSymbol,variablesList) ?(suffix="") =
      let newVariablesList =
        variablesList
        |> List.map ~f:(fun var -> let newVar =
                                            ((Variable.toString var)^suffix)
                                            |> Variable.fromString
                                          in
                                          newVar
                        )
      in
      (relationSymbol,
       newVariablesList)

    let toString atom =
      let (relationSymbol,variableList) = atom
      in
      (RelationSymbol.toString relationSymbol)
      ^ "("
      ^ (variableList
         |> List.map ~f:Variable.toString
         |> String.concat ~sep:",")
      ^ ")"
          
    let print atom =
      let (relationSymbol,variableList) = atom
      in
      RelationSymbol.print relationSymbol;
      Printf.printf "(";
      variableList
      |> List.map ~f:Variable.toString
      |> String.concat ~sep:","
      |> Printf.printf "%s";
      Printf.printf ")"

    let vars (_,vars) = VariableS.of_list vars

    let varsNumber (_,vars) = List.length vars
                                          
    let varOccurenceNumber atom variable =
      snd atom
      |> List.filter ~f:(fun x -> x=variable)
      |> List.length
              
    let hasVar (_,variableList) var =
      List.exists ~f:(fun x -> x=var) variableList

    let renameVar (relationSymb,vars) var newVar =
      let newVarList =
        vars
        |> List.map ~f:(fun varTmp -> if varTmp = var
                                      then newVar
                                      else varTmp
                       )
      in
      (relationSymb,newVarList)

    let rec randomlyReplaceAVariableByAnother (relationSymbol,variableList) newVariable =
      let (hdList,tlList) =
        Random.self_init();
        Random.int (List.length variableList) - 1
        |> List.split_n variableList
      in
      let updatedTlList =
        tlList
        |> List.tl_exn
        |> List.cons newVariable
      in
      (relationSymbol,(List.append hdList updatedTlList))
      
    let compare
	  (relationSymbol1,varList1)
	  (relationSymbol2,varList2) = 
      if relationSymbol1 = relationSymbol2
      then
        List.compare (Variable.compare) varList1 varList2 
      else
        RelationSymbol.compare relationSymbol1 relationSymbol2

    let sexp_of_t (relationSymbol,variableList) =
      (RelationSymbol.toString relationSymbol)^(List.to_string ~f:Variable.toString variableList)
      |> Sexp.of_string
      
    let t_of_sexp sexp =
        of_sexp_error "Atom error. " sexp

  end

module AtomS = Set.Make(Atom)
    
module AtomConjunction =  struct
  include AtomS

  let toString conjunction =
    conjunction
    |> to_list
    |> List.map ~f:Atom.toString
    |> String.concat ~sep:" /\\ "

  let toStringAsInstance conjunction =
    (conjunction
       |> to_list
       |> List.map ~f:Atom.toString
       |> String.concat ~sep:"; ")

  let toOutputString conjunction =
    conjunction
    |> to_list
    |> List.map ~f:Atom.toString
    |> String.concat ~sep:", "
                     
  let print conjunction = 
    conjunction
    |> to_list
    |> List.map ~f:Atom.toString
    |> String.concat ~sep:" /\\ "
    |> Printf.printf "%s"


  let println conjunction =
    print conjunction;
    Printf.printf "\n"
              
  let vars conjunction =
    fold ~f:(fun set atom -> VariableS.union (Atom.vars atom) set)
                   ~init:VariableS.empty
                   conjunction

  let varOccurenceNumber conjunction variable =
    fold ~f:(fun sum atom -> (+) (Atom.varOccurenceNumber atom variable) sum )
              ~init:0
              conjunction

  let chooseRandomAtom conjunction =
      let choosenAtomOption =
        Random.self_init();
        Random.int (length conjunction)
        |> find_index conjunction
      in
      match choosenAtomOption with
      | None -> assert false
      | Some atom -> atom
                      
  let add conjunction atom =
     add conjunction atom

  let remove conjunction atom =
    remove conjunction atom
        
  let renameAtoms conjunction ?(suffix="") =
    conjunction
    |> fold ~f:(fun accum atom -> Atom.rename atom ~suffix:suffix
                                  |> add accum
               )
            ~init:empty
    
  let containVars conjunction vars =
    vars
    |> VariableS.for_all ~f:(fun var -> exists ~f:(fun atom -> Atom.hasVar atom var) conjunction)

  let renameVar conjunction var newVar =
    conjunction
    |> map ~f:(fun atom -> Atom.renameVar atom var newVar)
        
  let dropAtom conjunction atomToDrop =
    remove conjunction atomToDrop
             
  let length conjunction =
    length conjunction

  let symmetric_difference conj1 conj2 =
    inter conj1 conj2
  |> diff (union conj1 conj2)
      
  let empty = empty

  let isEmpty conjunction =
    is_empty conjunction

  let isASubsetOf possibleSubset conjunction =
     conjunction
     |> subset possibleSubset
      
end
                            
module AtomConjunctionS = Set.Make(AtomConjunction)

module Tgd =
  struct
    type t = AtomConjunction.t * AtomConjunction.t

    let create lhs rhs =
      (lhs,rhs)
                                   
    let print tgd =
      let (lhs,rhs) =
        tgd
      in
      AtomConjunction.print lhs;
      Printf.printf "-> ";
      AtomConjunction.print rhs;
      Printf.printf "\n"

    let printAsInstancesPair tgd =
      let (lhs,rhs) =
        tgd
      in
      Printf.printf "%s"
        ("SOURCE\n "
         ^ AtomConjunction.toStringAsInstance lhs
         ^ "\nTARGET\n "
         ^ AtomConjunction.toStringAsInstance rhs
         ^ "\n")
                    
    let toString tgd =
      let (lhs,rhs) =
        tgd
      in
      AtomConjunction.toString lhs
      ^ "-> "
      ^ AtomConjunction.toString rhs
      ^ "\n"

    let toStringAsInstancesPair tgd =
      let (lhs,rhs) =
        tgd
      in
      "SOURCE\n "
      ^ AtomConjunction.toStringAsInstance lhs
      ^ "\nTARGET\n "
      ^ AtomConjunction.toStringAsInstance rhs
      ^ "\n"
      
    let toOutputString tgd =
      let (lhs,rhs) =
        tgd
      in
      AtomConjunction.toOutputString lhs
      ^ " -> "
      ^ AtomConjunction.toOutputString rhs
      ^ ";\n"
                    
    let extractExistentiallyQuantifiedVariables tgd =
      let (lhs,rhs) = tgd
      in
      let lhsVariables =
        AtomConjunction.vars lhs
      in
      let rhsVariables =
        AtomConjunction.vars rhs
      in
      VariableS.diff rhsVariables lhsVariables

    let lhs tgd =
      let (lhs,_) = tgd
      in
      lhs

    let rhs tgd =
      let (_,rhs) = tgd
      in
      rhs

    let invertSourceAndTarget (lhs,rhs) =
      (rhs,lhs)
      
    let renameVar tgd var newVar =
      let newLhs =
        AtomConjunction.renameVar (lhs tgd) var newVar
      in
      let newRhs =
        AtomConjunction.renameVar (rhs tgd) var newVar
      in
      create newLhs newRhs

    let vars (lhs,rhs) =
      VariableS.union (AtomConjunction.vars lhs) (AtomConjunction.vars rhs)

    let varOccurenceNumber tgd variable =
      let lhs =
        lhs tgd
      in
      let rhs =
        rhs tgd
      in
      (AtomConjunction.varOccurenceNumber lhs variable)
      +
      (AtomConjunction.varOccurenceNumber rhs variable)        
                      
    let compare tgd1 tgd2 =
      let compareLhs =
        AtomConjunction.compare (lhs tgd1) (lhs tgd2)
      in
      if compareLhs = 0
      then AtomConjunction.compare (rhs tgd1) (rhs tgd2)
      else compareLhs

    let sexp_of_t (lhs,rhs) =
      (AtomConjunction.toString lhs)^(AtomConjunction.toString rhs)
      |> Sexp.of_string
      
    let t_of_sexp sexp =
      of_sexp_error "Tgd error. " sexp
             
  end

module TgdS = Set.Make(Tgd)

module Mapping =
  struct    
    include TgdS
             
    let print mapping =
      iter ~f:Tgd.print mapping
      ;
        Printf.printf "\n"
           
    let toString mapping =
      mapping
      |> fold ~f:(fun accum tgd -> accum ^ (Tgd.toString tgd) )
              ~init:""

    let toStringAsPairsOfInstances mapping =
      mapping
      |> fold ~f:(fun accum tgd -> accum ^ (Tgd.toStringAsInstancesPair tgd) )
              ~init:""
            
    let toOutputString mapping =
      mapping
      |> fold ~f:(fun accum tgd -> accum ^ (Tgd.toOutputString tgd) ^ "\n" )
              ~init:"TGD\n"

    let vars mapping =
      mapping
      |> fold ~f:(fun accum tgd -> Tgd.vars tgd
                                             |> VariableS.union accum)
              ~init:VariableS.empty

    let from_list tgdList =
      tgdList
    |> List.fold ~f:(fun accum tgd -> tgd
                                    |> add accum
                  )
                ~init:empty
              
    let extractExistentiallyQuantifiedVariables mapping =
      mapping
      |> fold ~f:(fun accum tgd -> tgd
                                   |> Tgd.extractExistentiallyQuantifiedVariables
                                   |> VariableS.union accum
                 )
              ~init:VariableS.empty
              
    let unifyTwoVariables mapping var1 var2 =
      mapping
      |> fold ~f:(fun accum tgd -> Tgd.renameVar tgd var1 var2
                                                   |> add accum)
              ~init:empty

    let invertTgds mapping =
      mapping
      |> fold ~f:(fun accum tgd -> tgd
                                   |> Tgd.invertSourceAndTarget
                                   |> add accum
                 )
              ~init:empty

    let chooseRandomTgd mapping =
      let choosenTgdOption =
        Random.self_init();
        Random.int (length mapping)
        |> find_index mapping
      in
      match choosenTgdOption with
      | None -> assert false
      | Some tgd -> tgd
                                 
  end

module MappingPair = struct
  type t = Mapping.t * Mapping.t

  let empty =
    (Mapping.empty,Mapping.empty)

  let addCanonicalTgd (canonical,expected) tgd =
    let newMapping =
      Mapping.add canonical tgd
    in
    (newMapping,expected)

  let addCanonicalMapping (canonical,expected) mappingToAdd =
    let newMapping =
      Mapping.union canonical mappingToAdd
    in
    (newMapping,expected)
    
  let addExpectedTgd (canonical,expected) tgd =
    let newMapping =
      Mapping.add expected tgd
    in
    (canonical,newMapping)

  let addExpectedMapping (canonical,expected) mappingToAdd =
    let newMapping =
      Mapping.union expected mappingToAdd
    in
    (canonical,newMapping)

  let getCanonicalMapping (canonical,_) =
    canonical

  let getExpectedMapping (_,expected) =
    expected

end
