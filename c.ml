
type program = function_defs
and function_defs = function_def list
and function_def  = FUNC of id * formals * block
and block         = BLOCK of decls * stmts
and id            = string
and formals       = formal list
and formal        = typ * id
and decls         = decl list
and decl          = typ * id
and typ           = TVOID | TINT | TARR of int
and stmts         = stmt list
and stmt          = ASSIGN of lv * exp
                  | IF of exp * block * block
                  | WHILE of exp * block
                  | DOWHILE of block * exp
                  | READ of id
                  | PRINT of exp

and lv            = ID of id | ARR of id * exp
and exp           = NUM of int
                  | LV of lv
                  | ADD of exp * exp
                  | SUB of exp * exp
                  | MUL of exp * exp
                  | DIV of exp * exp
                  | MINUS of exp         
                  | NOT of exp
                  | LT of exp * exp 
                  | LE of exp * exp 
                  | GT of exp * exp 
                  | GE of exp * exp 
                  | EQ of exp * exp 
                  | AND of exp * exp
                  | OR  of exp * exp
                  | CALL of id * exp list

let pp pgm = ()
