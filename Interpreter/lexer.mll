(* File lexer.mll *)
{
open Parser        (* Token list in parser.mli *)
exception EOF
}
let Chars = ['a'-'z''A'-'Z''_''0'-'9']
let Strings = ['"'](Chars*[' ']*)*['"']
rule token = parse

    (* Ignoring white Spaces*)
    | [' ''\t']                         { token lexbuf }        (* Treat tabs and blanks as white space and ignore *)
    | ['\n']                            { token lexbuf}         (* Treat line returns as white space and ignore*)
    | "/*" [^'|''*']* "*/"              { token lexbuf}         (* Defining comments and ignoring them as white space *)

    (* Program Structure *)
    | "begin"                           { BEGIN }               (* Start flag for the program *)
    | "end"                             { END }                 (* End flag for the program *)
    | eof                               { raise EOF }           (* Raise End of file at file end*)
    | "$args"['0'-'9']+ as input        { INPUT input  }        (* Definition to take in inputs for the program *)

    (*If then else Stetements*)
    | "if"                              { IF }
    | "then"                            { THEN }
    | "else"                            { ELSE }
    | "fi"                              { FI }                  (* if spelled backwards to mark end of if statement*)

    (* boolean values *)
    | "true"                            { TRUE true }
    | "false"                           { FALSE false }


    (* for loop definition *)
    | "for"                             { FOR }
    | "in"                              { IN }
    | "do"                              { DO }
    | "rof"                             { ROF }                 (* for spelled backwards to mark the end of for loop *)


    | "print"                           { PRINT }               (* Definition for the print command*)

    | "let"                             { LET }                 (* Let function for variable declarations *)
    | "_empty_string"                   { STRING ""}
    | "_output_count" as output_stuff   { COUNT output_stuff }

    (* parenthesis and line ends*)
    | '('                               { LEFTPAR }
    | ')'                               { RIGHTPAR }
    | ';'                               { LINEEND }

    (*Mathematical operations*)
    | '='                               { ASSGN }
    | '+'                               { PLUS }
    | '-'                               { MINUS }
    | '*'                               { TIMES }
    | '/'                               { DIVIDE }
    | '%'                               { MOD }

    (*String and SET operations*)
    | "Insert"                           { INSERT }                (* Add to SET *)
    | "SetMinus"                         { SETMINUS }              (* Remove from SET *)
    | '^'                                { STRCON }                (* Concatenate String *)

    (*Boolean operations*)
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
    | Strings as str_id                 { STRING( List.nth (Str.split_delim (Str.regexp "\"") str_id) 1) }
    | ['0'-'9']+ as lxm                 { INT (int_of_string lxm) }
    | '#'Chars+ as int_var_id           { INTVAR( int_var_id ) }
    | '@'Chars+ as str_var_id           { STRVAR( str_var_id ) }
    | '?'Chars+ as bl_var_id            { BOOLVAR( bl_var_id ) }
    | '$'Chars+  as set                 { SETVAR set }
