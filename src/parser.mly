/* Grammar for Source and Target Instances*/
%{
(* OCaml Header *)
%}
/* Declarations */
%token LPAREN RPAREN
%token AND COMMA SEMICOLUMN LINE_END
%token TGD
%token SOURCE TARGET TO
%token EOF
%token <string> NAME

%start  main
%type <Mapping.MappingPair.t> main

%%
  /* Rules */
main:
| TGD tgds main
  {Mapping.MappingPair.addExpectedMapping $3 $2 }
| dataExample main
  { Mapping.MappingPair.addCanonicalTgd $2 $1 }
| EOF
  { Mapping.MappingPair.empty }
;

tgds:
| tgd tgds
{ Mapping.Mapping.add $2 $1 } 
| tgd
{ Mapping.Mapping.add Mapping.Mapping.empty $1 }
;
  
tgd:
| conjunction TO conjunction SEMICOLUMN
  { Mapping.Tgd.create $1 $3 }
| conjunction TO conjunction
  { Mapping.Tgd.create $1 $3 } 
;

conjunction:
| atom AND conjunction
    { Mapping.AtomConjunction.add $3 $1 }       
| atom COMMA conjunction
    { Mapping.AtomConjunction.add $3 $1 }
| atom
    { Mapping.AtomConjunction.add Mapping.AtomConjunction.empty $1 }
;

atom:
| NAME LPAREN variables RPAREN
       { Mapping.Atom.create (GeneralStruct.RelationSymbol.fromString $1) $3 }
;

variables:
| name COMMA variables
  { $1::$3 }
| name
  { [$1] }
;
  
name:
| NAME LPAREN skolemVariables RPAREN 
  { GeneralStruct.Variable.fromString ($1^ "_" ^ $3) }
| NAME
  { GeneralStruct.Variable.fromString $1 }

skolemVariables:
| NAME COMMA skolemVariables
  { $1 ^ "_" ^ $3 }
| NAME
  { $1 }
;
  
dataExample:
| SOURCE instance TARGET instance
  { Mapping.Tgd.create $2 $4 }
;

instance: 
| fact instance
    { Mapping.AtomConjunction.add $2 $1 }       
| fact
    { Mapping.AtomConjunction.add Mapping.AtomConjunction.empty $1 }
;

fact:
| NAME LPAREN constants RPAREN SEMICOLUMN
  { Mapping.Atom.create (GeneralStruct.RelationSymbol.fromString $1) $3 }
| NAME LPAREN constants RPAREN
  { Mapping.Atom.create (GeneralStruct.RelationSymbol.fromString $1) $3 }
;

constants:
| NAME COMMA constants
  { (GeneralStruct.Variable.fromString $1)::$3 }
| NAME
  { [ GeneralStruct.Variable.fromString $1] }
; 

%%
(* Note: name level is used to handle skolem functions in mappings of iBench scenarios*)
