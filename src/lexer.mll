{
  open Parser

  exception Error of string

}

rule token = parse
(* skip spaces and comments *)
    | ([' ' '\t' '\r' '\n'] | '#' [^'\n']* '\n' | "Name:"  [^'\n']* '\n')   { token lexbuf }
    | ("->"|"TO") { TO }
    | ("SOURCE"|"Source:") { SOURCE }
    | ("TARGET"|"Target:") { TARGET }
    | ("TGD"|"Tgd:") { TGD }
    | (['a'-'z' 'A'-'Z' '_'](['a'-'z' 'A'-'Z' '_' '0'-'9']*)) as n { NAME n }
    | '&' { AND }
    | ','    { COMMA }
    | ';' { SEMICOLUMN }
    | '('    { LPAREN }
    | ')'    { RPAREN }
    | (eof)   { EOF }
    | _
      { raise (Error (Printf.sprintf "At offset %d: unexpected character '%s'." (Lexing.lexeme_start lexbuf) (Lexing.lexeme lexbuf))) }

