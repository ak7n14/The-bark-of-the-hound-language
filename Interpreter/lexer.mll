(* File lexer.mll *)
{
open Parser        (* Token list in parser.mli *)
exception Eof
}
let allChars = ['a'-'z''A'-'Z''_''0'-'9']
let allStrings = ['"'](allChars*[' ']*)*['"']
rule token = parse

    | [' ''\t']                         { token lexbuf }        (* skip blanks *)
    | ['\n']                            { token lexbuf}      
    | "/*" [^'|''*']* "*/"              { token lexbuf}
    | "begin"                           { BEGIN }               (* marking start and end of program *)
    | "end"                             { END }
    | "$input"['0'-'9']+ as input       { INPUT input  }        (* input stream and output stream *)
    | "add"                             { ADD }                 (* add to an output list *)
    | "remove"                          { REMOVE }
    | "if"                              { IF }                  (* if then else fi block *)
    | "then"                            { THEN }    
    | "else"                            { ELSE }
    | "fi"                              { FI }
    | "for"                             { FOR }                 (* for in do done block *)
    | "in"                              { IN }
    | "do"                              { DO }
    | "done"                            { DONE }
    | "true"                            { TRUE true }           (* boolean values *)
    | "false"                           { FALSE false }
    | "print"                           { PRINT }               (* print command *)
    | "let"                             { LET }                 (* let keyword for output stream and variable declarations *)
    | "_empty_string"                   { STRING ""}
    | "_output_count" as output_stuff   { COUNT output_stuff }
    | '('                               { LPAREN }
    | ')'                               { RPAREN }
    | ';'                               { SEMI_COLON } 
    | '='                               { ASSIGN }
    | '+'                               { PLUS }
    | '-'                               { MINUS }
    | '*'                               { TIMES }
    | '/'                               { DIVIDE }
    | '%'                               { MOD }
    | '^'                               { CONCAT }
    | '<'                               { LT }
    | '>'                               { GT }
    | "<="                              { LTE }
    | "||"                              { OR }
    | "&&"                              { AND }
    | ">="                              { GTE }
    | "=="                              { EQ }
    | "!="                              { NEQ }
    | '!'                               { NOT }
    | allStrings as str_id              { STRING( List.nth (Str.split_delim (Str.regexp "\"") str_id) 1) }
    | ['0'-'9']+ as lxm                 { INT (int_of_string lxm) }
    | '#'allChars+ as int_var_id        { IVAR( int_var_id ) }
    | '@'allChars+ as str_var_id        { SVAR( str_var_id ) }
    | '?'allChars+ as bl_var_id         { BVAR( bl_var_id ) }
    | '$'allChars+  as set              { LVAR set }
    | eof                               { raise Eof }

