%{

%}

%token <int> NUM
%token <string> ID
%token VOID INT PLUS MINUS STAR SLASH EQUAL EQUALEQUAL LE LT GE GT NOT AND OR IF ELSE WHILE DO READ PRINT SEMICOLON COMMA
%token LBRACE RBRACE LBLOCK RBLOCK LPAREN RPAREN EOF

%left SEMICOLON
%left OR
%left AND
%left LT LE GT GE EQUALEQUAL  
%left PLUS MINUS
%left STAR SLASH
%right NOT
%nonassoc RPAREN
%nonassoc ELSE

%start program
%type <C.program> program
%%

program:
    function_defs EOF { $1 }
;

function_defs:
  | function_defs function_def { $1@[$2] }
  | { [] }
;

function_def:
  | typ ID LPAREN formals RPAREN block { C.FUNC ($2, $4, $6) }
;

block:
  | LBRACE decls stmts RBRACE { C.BLOCK ($2, $3) }
;

formals:
  | formals formal { $1@[$2] }
  | { [] }
;

formal:
  | typ ID { ($1, $2) }
  | typ ID COMMA { ($1, $2) }
    
decls:
   | decls decl  { $1@[$2]  }
   |  { [] }
;

decl:
   | typ ID SEMICOLON { ($1,$2) }
;

typ:
   | INT LBLOCK NUM RBLOCK { C.TARR ($3) }
   | INT   { C.TINT }
   | VOID  { C.TVOID }
;

stmts:
   | stmts stmt { $1@[$2]  }
   |  { [] }
;

stmt:
  | lv EQUAL exp SEMICOLON { C.ASSIGN ($1,$3) }
  | lv PLUS PLUS SEMICOLON { C.ASSIGN ($1,C.ADD(C.LV $1, C.NUM 1)) }
  | IF LPAREN exp RPAREN block ELSE block { C.IF ($3,$5,$7) }
  | IF LPAREN exp RPAREN block { C.IF ($3,$5,C.BLOCK ([],[])) }
  | WHILE LPAREN exp RPAREN block { C.WHILE ($3,$5) }
  | DO block WHILE LPAREN exp RPAREN SEMICOLON { C.DOWHILE ($2,$5) }
  | READ LPAREN ID RPAREN SEMICOLON { C.READ $3 }
  | PRINT LPAREN exp RPAREN SEMICOLON { C.PRINT $3 }
;

lv:
  | ID { C.ID $1 }
  | ID LBLOCK exp RBLOCK { C.ARR ($1, $3) }
;

exp:
  | exp PLUS exp  { C.ADD ($1,$3) }
  | exp MINUS exp { C.SUB ($1,$3) }
  | exp STAR exp  { C.MUL ($1,$3) }
  | exp SLASH exp { C.DIV ($1,$3) }
  | MINUS exp { C.MINUS $2 }
  | lv { C.LV $1 }
  | NUM { C.NUM $1 }
  | NOT exp { C.NOT $2 }
  | exp EQUALEQUAL exp { C.EQ ($1,$3) }
  | exp LT exp { C.LT ($1,$3) } 
  | exp LE exp { C.LE ($1,$3) } 
  | exp GT exp { C.GT ($1,$3) } 
  | exp GE exp { C.GE ($1,$3) } 
  | exp OR exp { C.OR ($1,$3) }
  | exp AND exp { C.AND ($1,$3) }
  | LPAREN exp RPAREN { $2 }
  | ID LPAREN args RPAREN { C.CALL ($1, $3) }
;

args:
  | args exp { $1@[$2] }
  | { [] }
;

%%

let parse_error s = print_endline s
