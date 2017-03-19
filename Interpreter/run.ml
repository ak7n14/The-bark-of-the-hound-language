open ParseTree;;
open List;;
open Lexer;;

module VariableBinding = Map.Make(String);;

let intBinding = ref VariableBinding.empty;;
let stringBinding = ref VariableBinding.empty;;
let boolBinding = ref VariableBinding.empty;;

let setBinding = ref VariableBinding.empty;;

let outputCount = ref 0;;

(* ============================ helper code ===============================*)
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

let lookupStrVar e = match e with
  | Str s -> s
  | StrVar sv -> try
        VariableBinding.find sv !stringBinding
      with Not_found -> failwith ("Variable "^sv^" Not Declared as a string type. Hint: Maybe try - let "^sv^" = _empty_string;")
;;

let lookupIntVar e = match e with
  | Int i -> i 
  | IntVar iv -> try
        VariableBinding.find iv !intBinding
      with Not_found -> failwith ("Variable "^iv^" Not Declared as an int type. Hint: Maybe try - let "^iv^" = 0;")
;;

let looupBlVar e = match e with
  | Bool b -> b
  | BoolVar bv -> try
        VariableBinding.find bv !boolBinding
      with Not_found -> failwith ("Variable "^bv^" Not Declared as a boolean type. Hint: Maybe try - let "^bv^" = false;")
;;

let lookupSet name =
  VariableBinding.find name !setBinding
;;

let rec convert_empty_input = function
| [] -> []
| (h::t) when h = ":" -> ""::(convert_empty_input t) 
| (h::t) ->  h::(convert_empty_input t)
;;

let processInput input_line stream_number= 
  setBinding := VariableBinding.add ("$input"^(string_of_int !stream_number)) (convert_empty_input (get_uniq_words input_line)) !setBinding
;;

(* ============================ helper code ===============================*)

let processSetAction e  = match e with
  | Set s -> lookupSet s
  | SetAdd (name, sv) -> (let string_value = lookupStrVar sv in
                          try
                              let set = lookupSet name in string_value :: set;
                          with Not_found -> failwith ("Set "^name^" Not Found. Hint: Maybe try - let "^name^";"))
  | SetRem (name, sv) -> (let string_value = lookupStrVar sv in
                          try
                              let set = lookupSet name in
                                List.filter (fun x-> if (compare string_value x)==0 then false else true) set
                          with Not_found -> failwith ("Set "^name^" Not Found. Hint: Maybe try - let "^name^";"))
;;

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

let rec processIntAction e = match e with
  | IntOrVar iv -> lookupIntVar iv
  | Plus (v1, v2) -> (processIntAction v1) + (processIntAction v2)
  | Minus (v1, v2) -> (processIntAction v1) - (processIntAction v2)
  | Times (v1, v2) -> (processIntAction v1) * (processIntAction v2)
  | Divide (v1, v2) ->( try
                          (processIntAction v1) / (processIntAction v2)
                        with Division_by_zero -> failwith "Cannot divide by zero.")
  | Mod (v1, v2) -> (processIntAction v1) mod (processIntAction v2)
  | Uminus v1 -> -(processIntAction v1)
;;

let rec processStrAction e = match e with
  | StrOrVar sv -> lookupStrVar sv
  | Cat (s1, s2) -> (processStrAction s1) ^ (lookupStrVar s2)
;;

let rec processBoolAction e = match e with
  | BoolOrVar bv -> looupBlVar bv
  | Les (ia1, ia2) -> (processIntAction ia1) < (processIntAction ia2)
  | Grt (ia1, ia2) -> (processIntAction ia1) > (processIntAction ia2)
  | LesEq (ia1, ia2) -> (processIntAction ia1) <= (processIntAction ia2)
  | GrtEq (ia1, ia2) -> (processIntAction ia1) >= (processIntAction ia2)
  | IntEq (ia1, ia2) -> (processIntAction ia1) == (processIntAction ia2)
  | IntNtEq (ia1, ia2) -> (processIntAction ia1) != (processIntAction ia2)
  | StrEq (sa1, sa2) -> let value = String.compare (processStrAction sa1) (processStrAction sa2) in value == 0
  | StrNtEq (sa1, sa2) -> let value = String.compare (processStrAction sa1) (processStrAction sa2) in value != 0
  | BlEq (ba1, ba2) -> (processBoolAction ba1) == (processBoolAction ba2)
  | BlNtEq (ba1, ba2) -> (processBoolAction ba1) == (processBoolAction ba2)
  | And (ba1, ba2) -> (processBoolAction ba1) && (processBoolAction ba2)
  | Or (ba1, ba2) -> (processBoolAction ba1) || (processBoolAction ba2)
  | Not ba -> not (processBoolAction ba)
;;

let rec processDecAction e = match e with
  | LVarDec s -> setBinding := VariableBinding.add s [] !setBinding
  | IVarDec (s, ia) -> intBinding := VariableBinding.add s (processIntAction ia) !intBinding
  | SVarDec (s, sa) -> stringBinding := VariableBinding.add s (processStrAction sa) !stringBinding
  | BVarDec (s, ba) -> boolBinding := VariableBinding.add s (processBoolAction ba) !boolBinding
;;

(* ============================ helper code ===============================*)
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
  | Print (SetAction s) -> (try 
                              let set = (processSetAction s) in 
                                print_newline (print_string (formatSet (empty_to_colon (sort_string_list set))))
                            with Not_found -> failwith ("Set Not Found. Hint: Maybe didn't declare it?"))
  | Print (IntAction i) -> print_newline (print_int (processIntAction i))
  | Print (BoolAction b) -> (let result = (processBoolAction b) in
                                  if (result == true) then print_newline (print_string "true") 
                                  else print_newline (print_string "false"))
  | Print (StrAction s) -> print_newline (print_string (processStrAction s))
  (*| PrtPrim (CtLeaf c) -> processPrint (PrtPrim (IntLeaf !outputCount))*)
;;

let processMutAction e = match e with
  | SetMut (setName, sa) -> (let new_set = processSetAction sa in 
                              setBinding := VariableBinding.add setName new_set !setBinding)
  | IntMut (intName, ia) -> (let new_int = processIntAction ia in
                              intBinding := VariableBinding.add intName new_int !intBinding)
  | StrMut (strName, sa) -> (let new_str = processStrAction sa in
                              stringBinding := VariableBinding.add strName new_str !stringBinding)
  | BlMut (blName, ba) -> (let new_bl = processBoolAction ba in
                              boolBinding := VariableBinding.add blName new_bl !boolBinding)                             
;;

let processOperation e = match e with
  | SetAction sa -> processSetAction sa; ()
  | IntAction ia -> processIntAction ia; ()
  | StrAction sa -> processStrAction sa; ()
  | BoolAction ba -> processBoolAction ba; ()
;;

let processAction e = match e with
  | Operation op -> processOperation op
  | DecAction s -> processDecAction s
  | MutAction s -> processMutAction s
  | PrintAction s -> processPrint s
;;

let rec processMain e = match e with
  | Body any -> processBody any

and processBody e = match e with
  | SingleStatement s -> processSingleStatement s
  | MultiStatement (s, b) -> processSingleStatement s; processBody b

and processSingleStatement e = match e with
  | IfStatement s -> processIf s
  | ForStatement s -> processFor s
  | ActionStatement s -> processAction s

and processIf e = match e with
  | If (b, bod) -> if (processBoolAction b) then (processBody bod)
  | IfElse (b, bod1, bod2) -> if (processBoolAction b) then (processBody bod1) else (processBody bod2)

and processFor e = match e with
  | ForEach (v, setName, bod) -> (try
                                exists v;
                                failwith ("Variable "^v^" used already! Hint: Try a variable that is not already declared by a let declaration.")
                              with Not_found -> (try
                                                   (let set = lookupSet setName in
                                                    let count = List.length set in
                                                      processDecAction (SVarDec (v, StrOrVar (Str "")));
                                                      for i = 0 to (count - 1) do 
                                                        processMutAction (StrMut (v, StrOrVar (Str (List.nth set i))));
                                                        processBody bod;
                                                      done;
                                                      stringBinding := VariableBinding.remove v !stringBinding)
                                                 with Not_found -> failwith ("Input set "^setName^" Not Found. Hint: $input0 refers to the 1st language in input file and so on.")))
  | ForBool (bl, bod) -> while (processBoolAction bl) do (processBody bod) done
;;

let storeInput = 
  try
    let streamCount = ref 0 in
      while true do
        let line = input_line stdin in
          if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then 
            (
            outputCount := (int_of_string line);
            intBinding := VariableBinding.add "#OUTPUT_COUNT" !outputCount !intBinding)
          else( 
            processInput line streamCount;
            streamCount := !streamCount + 1
          )
      done;
      None
  with
      End_of_file -> None
;;

let run =
  storeInput;
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = (Parser.main Lexer.token lexbuf) in
      processMain result
  with Lexer.Eof ->
    exit 0















