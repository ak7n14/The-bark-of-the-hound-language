type integerValue = 
| IntegerLiteral of int
| IntegerVariable of string
;;

type stringValue = 
| StringLiteral of string
| StringVariable of string
;;

type booleanValue = 
| BooleanLiteral of bool
| BooleanVariable of string
;;

type integerAction =
| Integer of integerValue
| Plus of integerAction * integerAction
| Minus of integerAction * integerAction
| Times of integerAction * integerAction
| Divide of integerAction * integerAction
| Mod of integerAction * integerAction
| UnaryMinus of integerAction
;;

type stringAction =
| String of stringValue
| StringConcatenation of stringAction * stringValue
;;

type setAction =
| Set of string
| Insert of string * stringValue
| SetMinus of string * stringValue
;;

type booleanAction =
| Boolean of booleanValue
| Less of integerAction * integerAction
| Great of integerAction * integerAction
| LessEqual of integerAction * integerAction
| GreatEqual of integerAction * integerAction
| IntegerEqual of integerAction * integerAction
| IntegerNotEqual of integerAction * integerAction
| StringEqual of stringAction * stringAction
| StringNotEqual of stringAction * stringAction
| BooleanEqual of booleanAction * booleanAction
| BooleanNotEqual of booleanAction * booleanAction
| And of booleanAction * booleanAction
| Or of booleanAction * booleanAction
| Not of booleanAction
;;

type declarationAction =
| SetVariableDeclaration of string
| IntegerVariableDeclaration of string * integerAction
| StringVariableDeclaration of string * stringAction
| BooleanVariableDeclaration of string * booleanAction
;;

type operation =
| SetAction of setAction
| IntegerAction of integerAction
| StringAction of stringAction
| BooleanAction of booleanAction
;;

type print =
| Print of operation
;;

type mutibleAction =
| SetMutible of string * setAction
| IntegerMutible of string * integerAction
| StringMutible of string * stringAction
| BooleanMutible of string * booleanAction
;;

type action = 
| Operation of operation
| DeclarationAction of declarationAction
| MutibleAction of mutibleAction
| PrintAction of print
;;

type body =
| SingleStatement of statement
| MultipleStatement of statement * body
and 
statement = 
| IfStatement of ifElse
| ForStatement of forDo
| ActionStatement of action
and 
ifElse =
| If of booleanAction * body
| IfElse of booleanAction * body * body
and 
forDo =
| ForBoolean of booleanAction * body
| ForEach of string * string * body
;;

type mainTree = 
| Body of body
;;
