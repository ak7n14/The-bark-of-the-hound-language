open ParseTree;;
open List;;
open Lexer;;

module VariableBinding = Map.Make(String);;

let intBinding = ref VariableBinding.empty;;
let stringBinding = ref VariableBinding.empty;;
let boolBinding = ref VariableBinding.empty;;
let setBinding = ref VariableBinding.empty;;

let outputCount = ref 0;;

(* Functions used to convert string set into list of strings *)
let get_words input = 
  let remove_stuff = Str.global_replace (Str.regexp "[ '{' | '}' | ' ']") "" in
    Str.split_delim (Str.regexp ",") (remove_stuff input);;

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
;;

let sort_string_list l =
  List.sort compare (uniq l)
;;

let get_uniq_words input =
  sort_string_list (get_words input)
;;

(* Check if a variable is actually defined *)
let exists name =
  try
    VariableBinding.find name !intBinding;
    None
  with Not_found -> try 
    VariableBinding.find name !stringBinding; 
    None
  with Not_found -> 
    VariableBinding.find name !boolBinding;
    None
;;

let processVar name = 
  try
    exists name          
  with Not_found ->  failwith ("Variable "^name^ " is not declared.")
;;

(* Functions to look up the values of variables *)
let lookupStrVar e = match e with
  | StringLiteral s -> s
  | StringVariable sv -> try
        VariableBinding.find sv !stringBinding
      with Not_found -> failwith ("Variable "^sv^" Not Declared as a string type. Hint: Maybe try - let "^sv^" = _empty_string;")
;;

let lookupIntVar e = match e with
  | IntegerLiteral i -> i 
  | IntegerVariable iv -> try
        VariableBinding.find iv !intBinding
      with Not_found -> failwith ("Variable "^iv^" Not Declared as an int type. Hint: Maybe try - let "^iv^" = 0;")
;;

let looupBlVar e = match e with
  | BooleanLiteral b -> b
  | BooleanVariable bv -> try
        VariableBinding.find bv !boolBinding
      with Not_found -> failwith ("Variable "^bv^" Not Declared as a boolean type. Hint: Maybe try - let "^bv^" = false;")
;;

let lookupSet name =
  VariableBinding.find name !setBinding
;;

(* parse colons into the empty set *)
let rec parseColon = function
| [] -> []
| (h::t) when h = ":" -> ""::(parseColon t) 
| (h::t) ->  h::(parseColon t)
;;

(* Add line of input as a set variable *)
let processInput input_line stream_number= 
  setBinding := VariableBinding.add ("args"^(string_of_int !stream_number)) (parseColon (get_uniq_words input_line)) !setBinding
;;

(* action occuring with integer *)
let rec processIntAction e = match e with
  | Integer iv -> lookupIntVar iv
  | Plus (v1, v2) -> (processIntAction v1) + (processIntAction v2)
  | Minus (v1, v2) -> (processIntAction v1) - (processIntAction v2)
  | Times (v1, v2) -> (processIntAction v1) * (processIntAction v2)
  | Divide (v1, v2) ->( try (processIntAction v1) / (processIntAction v2)
                        with Division_by_zero -> failwith "Fatal division by 0 error.")
  | Mod (v1, v2) -> (processIntAction v1) mod (processIntAction v2)
  | UnaryMinus v1 -> -(processIntAction v1)
;;

(* action occuring with string *)
let rec processStrAction e = match e with
  | String sv -> lookupStrVar sv
  | StringConcatenation (s1, s2) -> (processStrAction s1) ^ (lookupStrVar s2)
;;

let rec processBoolAction e = match e with
  | Boolean bv -> looupBlVar bv
  | Less (ia1, ia2) -> (processIntAction ia1) < (processIntAction ia2)
  | Great (ia1, ia2) -> (processIntAction ia1) > (processIntAction ia2)
  | LessEqual (ia1, ia2) -> (processIntAction ia1) <= (processIntAction ia2)
  | GreatEqual (ia1, ia2) -> (processIntAction ia1) >= (processIntAction ia2)
  | IntegerEqual (ia1, ia2) -> (processIntAction ia1) == (processIntAction ia2)
  | IntegerNotEqual (ia1, ia2) -> (processIntAction ia1) != (processIntAction ia2)
  | StringEqual (sa1, sa2) -> let value = String.compare (processStrAction sa1) (processStrAction sa2) in value == 0
  | StringNotEqual (sa1, sa2) -> let value = String.compare (processStrAction sa1) (processStrAction sa2) in value != 0
  | BooleanEqual (ba1, ba2) -> (processBoolAction ba1) == (processBoolAction ba2)
  | BooleanNotEqual (ba1, ba2) -> (processBoolAction ba1) == (processBoolAction ba2)
  | And (ba1, ba2) -> (processBoolAction ba1) && (processBoolAction ba2)
  | Or (ba1, ba2) -> (processBoolAction ba1) || (processBoolAction ba2)
  | Not ba -> not (processBoolAction ba)
;;

let processSetAction e  = match e with
  | Set s -> lookupSet s
  | Insert (name, sv) -> (let string_value = lookupStrVar sv in
                          try let set = lookupSet name in string_value :: set;
                          with Not_found -> failwith ("Variable " ^ name ^ " undefined. Ensure keyword 'set' preceeds variable name"))
  | SetMinus (name, sv) -> (let string_value = lookupStrVar sv in
                          try let set = lookupSet name in
                          	List.filter (fun x-> if (compare string_value x)==0 then false else true) set
                          with Not_found -> failwith ("Variable " ^ name ^ " undefined. Ensure keyword 'set' preceeds variable name"))
;;

let rec processDecAction e = match e with
  | SetVariableDeclaration s -> setBinding := VariableBinding.add s [] !setBinding
  | IntegerVariableDeclaration (s, ia) -> intBinding := VariableBinding.add s (processIntAction ia) !intBinding
  | StringVariableDeclaration (s, sa) -> stringBinding := VariableBinding.add s (processStrAction sa) !stringBinding
  | BooleanVariableDeclaration (s, ba) -> boolBinding := VariableBinding.add s (processBoolAction ba) !boolBinding
;;

let rec empty_to_colon = function
  | [] -> []
  | (h::t) when h = "" -> ":"::(empty_to_colon t) 
  | (h::t) ->  h::(empty_to_colon t)
;;

let car x = match x with 
  | (s, t) -> s;;

let split list n =
  let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  
                       in aux n [] list;;

let formatSet o =
  let truncate = (car (split o !outputCount)) in
  let rec formatSetAux o = match o with 
  | [] -> ""
  | [x] -> x
  | head::body -> head^", "^(formatSetAux body)
                  in "{"^(formatSetAux truncate)^"}";;
(* ============================ helper code ===============================*)

let rec processPrint e = match e with
  | Print (IntegerAction i) -> print_newline (print_int (processIntAction i))
  | Print (StringAction s) -> print_newline (print_string (processStrAction s))
  | Print (BooleanAction b) -> (let result = (processBoolAction b) in
                                  if (result == true) then print_newline (print_string "true") 
                                  else print_newline (print_string "false"))
  | Print (SetAction s) -> (try let set = (processSetAction s) in 
                                print_newline (print_string (formatSet (empty_to_colon (sort_string_list set))))
                            with Not_found -> failwith ("Set undefined. Ensure the set is correctly defined."))
;;

let processMutAction e = match e with
  | IntegerMutible (intName, ia) -> (let new_int = processIntAction ia in
                              intBinding := VariableBinding.add intName new_int !intBinding)
  | StringMutible (strName, sa) -> (let new_str = processStrAction sa in
                              stringBinding := VariableBinding.add strName new_str !stringBinding)
  | BooleanMutible (blName, ba) -> (let new_bl = processBoolAction ba in
                              boolBinding := VariableBinding.add blName new_bl !boolBinding)
  | SetMutible (setName, sa) -> (let new_set = processSetAction sa in 
                              setBinding := VariableBinding.add setName new_set !setBinding)
;;

let processOperation e = match e with
  | IntegerAction ia -> processIntAction ia; ()
  | StringAction sa -> processStrAction sa; ()
  | BooleanAction ba -> processBoolAction ba; ()
  | SetAction sa -> processSetAction sa; ()
;;

let processAction e = match e with
  | Operation op -> processOperation op
  | DeclarationAction s -> processDecAction s
  | MutibleAction s -> processMutAction s
  | PrintAction s -> processPrint s
;;

let rec processMain e = match e with
  | Body any -> processBody any

and processBody e = match e with
  | SingleStatement s -> processSingleStatement s
  | MultipleStatement (s, b) -> processSingleStatement s; processBody b

and processSingleStatement e = match e with
  | IfStatement s -> processIf s
  | ForStatement s -> processFor s
  | ActionStatement s -> processAction s

and processIf e = match e with
  | If (b, bod) -> if (processBoolAction b) then (processBody bod)
  | IfElse (b, bod1, bod2) -> if (processBoolAction b) then (processBody bod1) else (processBody bod2)

and processFor e = match e with
  | ForEach (v, setName, bod) -> 
  	(try exists v;
 	 failwith ("Variable " ^ v ^ " is already in use. Try changing variable name.")
     with Not_found ->
	(try (let set = lookupSet setName in
    	let count = List.length set in
       		processDecAction (StringVariableDeclaration (v, String (StringLiteral "")));
           	for i = 0 to (count - 1) do 
            	processMutAction (StringMutible (v, String (StringLiteral (List.nth set i))));
                processBody bod;
            done;
            stringBinding := VariableBinding.remove v !stringBinding)
     with Not_found -> failwith ("Invalid Input " ^ setName ^ ". Enter valid args#.")))
  | ForBoolean (bl, bod) -> 
    while (processBoolAction bl) do (processBody bod) done
;;

let storeInput = 
  try
    let streamCount = ref 0 in
      while true do
        let line = input_line stdin in
          if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then (
            outputCount := (int_of_string line);
            intBinding := VariableBinding.add "int OUTPUT_COUNT" !outputCount !intBinding
          ) else ( 
            processInput line streamCount;
            streamCount := !streamCount + 1
          )
      done;
      None
  with End_of_file -> None
;;

let run =
  storeInput;
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = (Parser.main Lexer.token lexbuf) in
      processMain result
  with Lexer.Eof ->
  	exit 0

