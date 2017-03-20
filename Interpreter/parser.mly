/* File parser.mly */
%{
open ParseTree;;
%}
%token <int> INT
%token <string> STRING
%token <bool> TRUE
%token <bool> FALSE
%token <string> INPUT
%token <string> COUNT
%token <string> INTVAR STRVAR BOOLVAR SETVAR
%token BEGIN END
%token IF THEN ELSE FI
%token FOR IN DO ROF
%token PRINT
%token LET
%token LINEEND LEFTPAR RIGHTPAR
%token ASSGN
%token PLUS MINUS TIMES DIVIDE MOD
%token STRCON
%token INSERT SETMINUS
%token LESS GREAT LESSEQUAL GREATEQUAL EQUAL NOTEQUAL NOT OR AND
%right OR
%right AND
%left EQUAL NOTEQUAL
%left LESS GREAT LESSEQUAL GREATEQUAL
%right STRCON
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%nonassoc NOT UNARYMINUS
%start main             
%type <ParseTree.mainTree> main
%type <ParseTree.body> body
%type <ParseTree.statement> statement
%type <ParseTree.forDo> for_do_rof
%type <ParseTree.ifElse> if_then_else_fi
%type <ParseTree.action> action
%type <ParseTree.operation> operation
%type <ParseTree.integerAction> integerAction
%type <ParseTree.integerValue> integerValue
%type <ParseTree.stringAction> stringAction
%type <ParseTree.stringValue> stringValue
%type <ParseTree.booleanAction> booleanAction
%type <ParseTree.booleanValue> booleanValue
%type <ParseTree.setAction> setAction
%type <string> setValue
%type <ParseTree.declarationAction> declarationAction
%type <ParseTree.mutibleAction> mutibleAction
%type <ParseTree.print> printAction
%%

main:
 | BEGIN body END 				{ Body $2 }
;

body:
 | statement                    		{ SingleStatement $1 }
 | statement body               		{ MultipleStatement ($1, $2) }
;

statement:
 | if_then_else_fi                 		{ IfStatement $1 }   
 | for_do_rof               			{ ForStatement $1 }
 | action LINEEND	   			{ ActionStatement $1 }
;

if_then_else_fi:
 | IF booleanAction THEN body FI    	     	{ If ($2, $4) }                           
 | IF booleanAction THEN body ELSE body FI 	{ IfElse ($2, $4, $6) } 	   
;

for_do_rof:
 | FOR booleanAction DO body ROF 		{ ForBoolean ($2, $4) }
 | FOR STRVAR IN setValue DO body ROF 		{ ForEach ($2, $4, $6) }
;

action:
 | operation 					{ Operation $1 }
 | declarationAction				{ DeclarationAction $1 }
 | mutibleAction				{ MutibleAction $1 }
 | printAction 					{ PrintAction $1 }
;

operation:
 | integerAction				{ IntegerAction $1 }
 | stringAction 				{ StringAction $1 }
 | booleanAction 				{ BooleanAction $1 }
 | setAction					{ SetAction $1 }
;

integerValue:
 | INT 						{ IntegerLiteral $1 }
 | INTVAR 					{ IntegerVariable $1 }
;

integerAction:
 | LEFTPAR integerAction RIGHTPAR		{ $2 }
 | integerValue 				{ Integer $1 }
 | integerAction PLUS integerAction 	    	{ Plus ($1, $3) }
 | integerAction MINUS integerAction 	    	{ Minus ($1, $3) }
 | integerAction TIMES integerAction       	{ Times ($1, $3) }
 | integerAction DIVIDE integerAction      	{ Divide ($1, $3) }
 | integerAction MOD integerAction  	    	{ Mod ($1, $3) }
 | MINUS integerAction %prec UNARYMINUS    	{ UnaryMinus $2 }
;

stringValue:
 | STRING 					{ StringLiteral $1 }
 | STRVAR 					{ StringVariable $1 }
;

stringAction:
 | stringValue 				    	{ String $1 }	
 | stringAction STRCON stringValue 		{ StringConcatenation ($1, $3) }
;

booleanValue:
 | TRUE						{ BooleanLiteral $1 }
 | FALSE					{ BooleanLiteral $1 }
 | BOOLVAR 					{ BooleanVariable $1 }
;

booleanAction:
 | LEFTPAR booleanAction RIGHTPAR 		{ $2 }
 | booleanValue 				{ Boolean $1 }
 | integerAction LESS integerAction 		{ Less ($1, $3) }
 | integerAction GREAT integerAction		{ Great ($1, $3) }
 | integerAction LESSEQUAL integerAction	{ LessEqual ($1, $3) }
 | integerAction GREATEQUAL integerAction	{ GreatEqual ($1, $3) }
 | integerAction EQUAL integerAction		{ IntegerEqual ($1, $3) }
 | integerAction NOTEQUAL integerAction		{ IntegerNotEqual ($1, $3) }
 | stringAction EQUAL stringAction 		{ StringEqual ($1, $3) }
 | stringAction NOTEQUAL stringAction 		{ StringNotEqual ($1, $3) }
 | booleanAction EQUAL booleanAction 		{ BooleanEqual ($1, $3) }
 | booleanAction NOTEQUAL booleanAction 	{ BooleanNotEqual ($1, $3) }
 | booleanAction AND booleanAction 		{ And ($1, $3) }
 | booleanAction OR booleanAction 		{ Or ($1, $3) }
 | NOT booleanAction 				{ Not $2 }
;

setValue:
 | INPUT 					{ $1 }
 | SETVAR 					{ $1 }
;

setAction:
 | setValue					{ Set $1 }
 | setValue INSERT stringValue			{ Insert ($1, $3) }
 | setValue SETMINUS stringValue		{ SetMinus ($1, $3) }
;

declarationAction:
 | LET SETVAR 					{ SetVariableDeclaration $2 }
 | LET INTVAR 					{ IntegerVariableDeclaration ($2, Integer (IntegerLiteral 0)) }
 | LET INTVAR ASSGN integerAction 		{ IntegerVariableDeclaration ($2, $4) }
 | LET STRVAR  					{ StringVariableDeclaration ($2, String (StringLiteral "")) }
 | LET STRVAR ASSGN stringAction 		{ StringVariableDeclaration ($2, $4) }
 | LET BOOLVAR 					{ BooleanVariableDeclaration ($2, Boolean (BooleanLiteral false)) }
 | LET BOOLVAR ASSGN booleanAction 		{ BooleanVariableDeclaration ($2, $4) }
;

mutibleAction:
 | SETVAR ASSGN setAction			{ SetMutible ($1, $3) }
 | INTVAR ASSGN integerAction			{ IntegerMutible ($1, $3) }
 | STRVAR ASSGN stringAction 			{ StringMutible ($1, $3) }
 | BOOLVAR ASSGN booleanAction 			{ BooleanMutible ($1, $3) }
;

printAction:
 | PRINT operation 				{ Print $2 }
;

