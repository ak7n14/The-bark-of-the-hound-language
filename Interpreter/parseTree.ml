type int_var = 
| Int of int
| IntVar of string
;;

type str_var = 
| Str of string
| StrVar of string
;;

type bool_var = 
| Bool of bool
| BoolVar of string
;;

type intAction =
| IntOrVar of int_var
| Plus of intAction * intAction
| Minus of intAction * intAction
| Times of intAction * intAction
| Divide of intAction * intAction
| Mod of intAction * intAction
| Uminus of intAction
;;

type strAction =
| StrOrVar of str_var
| Cat of strAction * str_var
;;

type setAction =
| Set of string
| SetAdd of string * str_var
| SetRem of string * str_var
;;

type boolAction =
| BoolOrVar of bool_var
| Les of intAction * intAction
| Grt of intAction * intAction
| LesEq of intAction * intAction
| GrtEq of intAction * intAction
| IntEq of intAction * intAction
| IntNtEq of intAction * intAction
| StrEq of strAction * strAction
| StrNtEq of strAction * strAction
| BlEq of boolAction * boolAction
| BlNtEq of boolAction * boolAction
| And of boolAction * boolAction
| Or of boolAction * boolAction
| Not of boolAction
;;

type decAction =
| LVarDec of string
| IVarDec of string * intAction
| SVarDec of string * strAction
| BVarDec of string * boolAction
;;

type operation =
| SetAction of setAction
| IntAction of intAction
| StrAction of strAction
| BoolAction of boolAction
;;

type print =
| Print of operation
;;

type mutAction =
| SetMut of string * setAction
| IntMut of string * intAction
| StrMut of string * strAction
| BlMut of string * boolAction
;;

type action = 
| Operation of operation
| DecAction of decAction
| MutAction of mutAction
| PrintAction of print
;;

type body =
| SingleStatement of statement
| MultiStatement of statement * body
and 
statement = 
| IfStatement of ifElse
| ForStatement of forDo
| ActionStatement of action
and 
ifElse =
| If of boolAction * body
| IfElse of boolAction * body * body
and 
forDo =
| ForBool of boolAction * body
| ForEach of string * string * body
;;

type mainTree = 
| Body of body
;;
