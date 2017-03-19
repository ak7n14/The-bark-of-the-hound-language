/* File parser.mly */
%{
open ParseTree;;
%}
%token <int> INT
%token <string> STRING
%token <string> IVAR BVAR SVAR LVAR
%token <string> INPUT
%token <string> COUNT
%token <bool> TRUE
%token <bool> FALSE
%token BEGIN END
%token ADD REMOVE
%token IF THEN ELSE FI
%token FOR IN DO DONE
%token PRINT
%token LET
%token SEMI_COLON LPAREN RPAREN
%token ASSIGN
%token PLUS MINUS TIMES DIVIDE MOD
%token CONCAT
%token LT GT LTE GTE EQ NEQ NOT OR AND
%right OR
%right AND
%left EQ NEQ
%left LT GT LTE GTE
%right CONCAT
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%nonassoc NOT UMINUS
%start main             
%type <ParseTree.mainTree> main
%type <ParseTree.body> body
%type <ParseTree.statement> statement
%type <ParseTree.forDo> for_do_done
%type <ParseTree.ifElse> if_then_else_fi
%type <ParseTree.action> action
%type <ParseTree.operation> operation
%type <ParseTree.intAction> int_action
%type <ParseTree.int_var> intOrIVar
%type <ParseTree.strAction> str_action
%type <ParseTree.str_var> strOrSVar
%type <ParseTree.boolAction> bool_action
%type <ParseTree.bool_var> boolOrBVar
%type <ParseTree.setAction> set_action
%type <string> set
%type <ParseTree.decAction> dec_action
%type <ParseTree.mutAction> mut_action
%type <ParseTree.print> print_action
%%

/* Program start here */
main:
 | BEGIN body END 					{ Body $2 }
;

/* Body, a list of statements , inside curly brakets to run */
body:
 | statement                    	{ SingleStatement $1 }
 | statement body               	{ MultiStatement ($1, $2) }
;

/* is a statement form a complete instruction, could include a semicolon  */
statement:
 | for_do_done               		{ ForStatement $1 }
 | if_then_else_fi                 	{ IfStatement $1 }   
 | action SEMI_COLON		   		{ ActionStatement $1 }
;

for_do_done:
 | FOR SVAR IN set DO body DONE 	{ ForEach ($2, $4, $6) }
 | FOR bool_action DO body DONE 	{ ForBool ($2, $4) }
;

/* Conditional statements (Note they must always have braces) */
if_then_else_fi:
 | IF bool_action THEN body FI    	     { If ($2, $4) }                           
 | IF bool_action THEN body ELSE body FI { IfElse ($2, $4, $6) } 	   
;

action:
 | operation 						{ Operation $1 }
 | dec_action 						{ DecAction $1 }
 | mut_action 						{ MutAction $1 }
 | print_action 					{ PrintAction $1 }
;

operation:
 | set_action						{ SetAction $1 }
 | int_action						{ IntAction $1 }
 | str_action 						{ StrAction $1 }
 | bool_action 						{ BoolAction $1 }
;

int_action:
 | LPAREN int_action RPAREN 		{ $2 }
 | intOrIVar 					    { IntOrVar $1 }
 | int_action PLUS int_action 	    { Plus ($1, $3) }
 | int_action MINUS int_action 	    { Minus ($1, $3) }
 | int_action TIMES int_action       { Times ($1, $3) }
 | int_action DIVIDE int_action      { Divide ($1, $3) }
 | int_action MOD int_action  	    { Mod ($1, $3) }
 | MINUS int_action %prec UMINUS    { Uminus $2 }
;

intOrIVar:
 | INT 								{ Int $1 }
 | IVAR 							{ IntVar $1 }
;

str_action:
 | strOrSVar 					    { StrOrVar $1 }	
 | str_action CONCAT strOrSVar 		{ Cat ($1, $3) }
;

strOrSVar:
 | STRING 							{ Str $1 }
 | SVAR 							{ StrVar $1 }
;

bool_action:
 | LPAREN bool_action RPAREN 		{ $2 }
 | boolOrBVar 						{ BoolOrVar $1 }
 | int_action LT int_action 		{ Les ($1, $3) }
 | int_action GT int_action			{ Grt ($1, $3) }
 | int_action LTE int_action		{ LesEq ($1, $3) }
 | int_action GTE int_action		{ GrtEq ($1, $3) }
 | int_action EQ int_action			{ IntEq ($1, $3) }
 | int_action NEQ int_action		{ IntNtEq ($1, $3) }
 | str_action EQ str_action 		{ StrEq ($1, $3) }
 | str_action NEQ str_action 		{ StrNtEq ($1, $3) }
 | bool_action EQ bool_action 		{ BlEq ($1, $3) }
 | bool_action NEQ bool_action 		{ BlNtEq ($1, $3) }
 | bool_action AND bool_action 		{ And ($1, $3) }
 | bool_action OR bool_action 		{ Or ($1, $3) }
 | NOT bool_action 					{ Not $2 }
;

boolOrBVar:
 | TRUE								{ Bool $1 }
 | FALSE							{ Bool $1 }
 | BVAR 							{ BoolVar $1 }
;

set_action:
 | set 								{ Set $1 }
 | set ADD strOrSVar				{ SetAdd ($1, $3) }
 | set REMOVE strOrSVar				{ SetRem ($1, $3) }
 /* set UNION SUB INTER set */
;

set:
 | INPUT 							{ $1 }
 | LVAR 							{ $1 }
;

dec_action:
 | LET LVAR 						{ LVarDec $2 }
 | LET IVAR 						{ IVarDec ($2, IntOrVar (Int 0)) }
 | LET IVAR ASSIGN int_action 		{ IVarDec ($2, $4) }
 | LET SVAR  						{ SVarDec ($2, StrOrVar (Str "")) }
 | LET SVAR ASSIGN str_action 		{ SVarDec ($2, $4) }
 | LET BVAR 						{ BVarDec ($2, BoolOrVar (Bool false)) }
 | LET BVAR ASSIGN bool_action 		{ BVarDec ($2, $4) }
;

mut_action:
 | LVAR ASSIGN set_action			{ SetMut ($1, $3) }
 | IVAR ASSIGN int_action			{ IntMut ($1, $3) }
 | SVAR ASSIGN str_action 			{ StrMut ($1, $3) }
 | BVAR ASSIGN bool_action 			{ BlMut ($1, $3) }
;

print_action:
 | PRINT operation 					{ Print $2 }
;



