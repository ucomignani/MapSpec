open Core.Std
       
module RelationSymbol = struct
  include String
  let fromString s = s
  let toString relSymb = relSymb
  let print relationSymbol = (Printf.printf " %s") relationSymbol                          
end


module Variable = struct
  include String
  let fromString s = s
  let toString var = var
  let print variable = (Printf.printf " %s") variable
end

module VariableS = Set.Make(String)                    
