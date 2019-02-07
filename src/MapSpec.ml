open Core.Std
open Core_bench.Std
   
open Preprocessing

let parseInputFile filename =
  filename
  |> open_in
  |> Lexing.from_channel
  |> Parser.main Lexer.token

let getExpectedMapping dataExSMappingPair=
  let mapping =
    Mapping.MappingPair.getExpectedMapping dataExSMappingPair
  in
  if Mapping.Mapping.is_empty mapping
  then None
  else Some (
           mapping
           |> Mapping.Mapping.fold ~f:(fun accum tgd ->
                                     let existentialVariables =
                                       tgd
                                       |> Mapping.Tgd.extractExistentiallyQuantifiedVariables
                                       |> GeneralStruct.VariableS.to_list
                                     in
                                     tgd
                                     |> Mapping.Mapping.add Mapping.Mapping.empty
                                     |> Preprocessing.splitReduction existentialVariables
                                     |> Mapping.Mapping.union accum
                                   )
                                   ~init:Mapping.Mapping.empty
           |> Preprocessing.sigmaRedundancySuppression
         )
  
(* Main function *)                                                     
let main filename atomStrategy joinStrategy quasiLatticeOptimisation =
  let inputMappingPair = filename
                         |> parseInputFile
  in
  
  let expectedMapping = inputMappingPair
                        |> getExpectedMapping
  in

  let canonicalMapping = inputMappingPair
                         |> Mapping.MappingPair.getCanonicalMapping
  in

  let existentialVariables = canonicalMapping
                             |> Mapping.Mapping.extractExistentiallyQuantifiedVariables
                             |> GeneralStruct.VariableS.to_list
  in

  let (atomRefinedMapping,countAtomQuestionsNumber) =
    canonicalMapping
    |> Preprocessing.splitReduction existentialVariables
    |> Preprocessing.sigmaRedundancySuppression
    |> AtomConjunctionRefinement.atomConjunctionRefinement quasiLatticeOptimisation ?expectedResult:expectedMapping ?strategy:(Some atomStrategy)
  in
  let (outputMapping,countJoinQuestionsNumber) =
    atomRefinedMapping
    |> JoinsRefinement.joinsRefinement quasiLatticeOptimisation ?expectedResult:expectedMapping ?strategy:(Some joinStrategy)
  in

  (* output file creation *)
  let outputFile =
    let uuid =
      Uuid.create ()
      |> Uuid.to_string
    in
    open_out ("result_"^uuid^".txt")
  in
  Printf.fprintf outputFile "AtQuest %s\n" (( Int.to_string countAtomQuestionsNumber));
  Printf.fprintf outputFile "JoinQuest %s\n\n" (( Int.to_string countJoinQuestionsNumber));

  (match expectedMapping with
   | None -> Printf.printf ""
   | Some m -> (Printf.fprintf outputFile "\nExpected mapping :\n%s\n"
                               (m
                                |> Mapping.Mapping.toString)
               )
  );          

  Printf.fprintf outputFile "\nOutput mapping :\n%s\n"
                 (outputMapping
                  |> Mapping.Mapping.toString
                 );

  Printf.fprintf outputFile "\nInput examples :\n%s\n"
                 (canonicalMapping
                  |> Mapping.Mapping.toStringAsPairsOfInstances
                 );

  Out_channel.close outputFile;

  (* displayed output *)
  Printf.printf "\nExpected mapping :\n";
  (
    match expectedMapping with
    | None -> Printf.printf "None\n"
    | Some m -> m
                |> Mapping.Mapping.print
  );

  Printf.printf "\nOutput mapping :\n";
  outputMapping
  |> Mapping.Mapping.print

  

let spec =
  let open Command.Spec
  in
  empty
  +> anon ("filename" %: file)
  +> flag "--atStrat" (optional_with_default "TDB"  string)
          ~doc:"val Atom exploration strategy: AP = a priori; TDB (default)  = top-down breadth; BUD = bottom-up depth first; TDD = top-down depth first"
  +> flag "--joinStrat" (optional_with_default "TDB" string)
          ~doc:"val Atom exploration strategy: AP = a priori; TDB (default)  = top-down breadth; BUD = bottom-up depth first; TDD = top-down depth first"
  +> flag "--quasiLatticeOpti" (optional_with_default true bool)
          ~doc:"bool Use (or not) quasi-lattices to avoid redundant questiosn."
  
(* arguments to choose the strategy: AP = a priori; TDB = top-down breadth; BUD = bottom-up depth first; TDD = top-down depth first*)

  
let command =
  Command.basic
    ~summary:"Execute the interactive mapping specification on the input file"
    ~readme:(fun () -> "Execute the interactive mapping specification on the input file.\n
                        If the file contain an expected mapping, then this mapping is used as an oracle to automatically answer questions (for benchmarking only).")
    spec
    (fun filename atomStrategy joinStrategy quasiLatticeOptimisation () -> main filename atomStrategy joinStrategy quasiLatticeOptimisation)

  
let () =
  Command.run
    ~version:"1.0"
    ~build_info:"_"
    command
