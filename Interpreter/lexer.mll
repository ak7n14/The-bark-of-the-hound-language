(* File lexer.mll *)
{
open Parser        						(* Token list in parser.mli *)
exception Eof
}
let Chars = ['a'-'z''A'-'Z''_''0'-'9']
let Strings = ['"'](Chars*[' ']*)*['"']
rule token = parse

    (* Ignore all white space type characters *)
    | [' ''\t']                         { token lexbuf }        (* Treat spaces and tabs as nonexistant and ignore *)
    | ['\n']                            { token lexbuf}         (* Treat line returns as nonexistant and ignore*)
    | "/*" [^'|''*']* "*/"              { token lexbuf}         (* Defining comments and ignoring them as with white space *)

    (* Program Structure *)
    | "begin"                           { BEGIN }               (* Start flag for the program *)
    | "end"                             { END }                 (* End flag for the program *)
    | "args"['0'-'9']+ as input	     	{ INPUT input  }        (* Definition to take in input languages for the program *)

    (* If then else Statements *)
    | "if"                              { IF }
    | "then"                            { THEN }
    | "else"                            { ELSE }
    | "fi"                              { FI }                  (* if spelt backwards to mark end of if statement*)

    (* For loop definition *)
    | "for"                             { FOR }
    | "in"                              { IN }
    | "do"                              { DO }
    | "rof"                             { ROF }                 (* for spelt backwards to mark the end of for loop *)

    (* Print for output from programs *)
    | "print"                           { PRINT }               (* Definition for the print command*)

    (* Assignment and helpers *)
    | "let"                             { LET }                 (* Let function for variable declarations *)
    | "_empty_string"                   { STRING ""}
    | "_output_count" as output_stuff   { COUNT output_stuff }

    (* Parenthesis and line ends*)
    | '('                               { LEFTPAR }
    | ')'                               { RIGHTPAR }
    | ';'                               { LINEEND }

    (* Mathematical operations *)
    | '='                               { ASSGN }
    | '+'                               { PLUS }
    | '-'                               { MINUS }
    | '*'                               { TIMES }
    | '/'                               { DIVIDE }
    | '%'                               { MOD }

    (* String and SET operations *)
    | "Insert"                          { INSERT }                (* Add to SET *)
    | "SetMinus"                        { SETMINUS }              (* Remove from SET *)
    | '^'                               { STRCON }                (* Concatenate String *)

    (* Boolean values *)
    | "true"                            { TRUE true }
    | "false"                           { FALSE false }

    (* Boolean operations*)
    | '<'                               { LESS }
    | '>'                               { GREAT }
    | "<="                              { LESSEQUAL }
    | "||"                              { OR }
    | "&&"                              { AND }
    | ">="                              { GREATEQUAL }
    | "=="                              { EQUAL }
    | "!="                              { NOTEQUAL }
    | '!'                               { NOT }

    (*Type declarations*)
    | Strings as str_id                 { STRING( List.nth (Str.split_delim (Str.regexp "\"") str_id) 1) } (* remove quotes ("") from string *)
    | ['0'-'9']+ as lxm                 { INT (int_of_string lxm) }
    | "int "Chars+ as var_name        	{ INTVAR( var_name ) }
    | "str "Chars+ as var_name        	{ STRVAR( var_name ) }
    | "bool "Chars+ as var_name        	{ BOOLVAR( var_name ) }
    | "set "Chars+ as set              	{ SETVAR set }

    | eof                               { raise Eof }           (* Raise End of file at file end*)
