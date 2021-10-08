
type program = function_defs
and function_defs = function_def list
and function_def  = FUNC of id * formals * block
and block         = BLOCK of stmts
and id            = string
and formals       = formal list
and formal        = id
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


let p x = print_string (x)

let rec p_indent n = if n = 0 then () else (p "    "; p_indent (n-4))

let rec p_lv lv = 
  match lv with
  | ID x -> p x
  | ARR (x, e) -> p x; p "["; p_exp e; p "]"

and p_exp e = 
  begin
  match e with
  | ADD (e1,e2) -> p_exp e1; p "+"; p_exp e2
  | SUB (e1,e2) -> p_exp e1; p "-"; p_exp e2
  | MUL (e1,e2) -> p_exp e1; p "*"; p_exp e2
  | DIV (e1,e2) -> p_exp e1; p "/"; p_exp e2
  | MINUS e -> p "-"; p_exp e
  | LV lv -> p_lv lv;
  | NUM i -> print_int i
  | LT (e1,e2) -> p_exp e1; p "<"; p_exp e2
  | LE (e1,e2) -> p_exp e1; p "<="; p_exp e2
  | GT (e1,e2) -> p_exp e1; p ">"; p_exp e2
  | GE (e1,e2) -> p_exp e1; p ">="; p_exp e2
  | EQ (e1,e2) -> p_exp e1; p "=="; p_exp e2
  | NOT e -> p "not "; p_exp e
  | AND (e1,e2) -> p_exp e1; p" and "; p_exp e2
  | OR (e1,e2) -> p_exp e1; p" or "; p_exp e2
  | CALL (id, exprs) -> p id; p "("; List.iter p_exp exprs; p ")"
  end
  
and p_stmt : int -> stmt -> unit
=fun indent stmt  -> 
  p_indent indent;
  begin
  match stmt with
  | ASSIGN (lv, exp) -> p_lv lv; p " = "; p_exp exp;
  | IF (bexp,then_block,else_block) -> p "if "; p_exp bexp; p ": "; p_block indent then_block; p "else: "; p_block indent else_block
  | WHILE (cond, block) -> p "while "; p_exp cond; p ": "; p_block indent block
  | DOWHILE (block, cond) -> p "do"; p_block (indent+4) block; p_indent indent; p "while"; p_exp cond;
  | PRINT e -> p "print("; p_exp e; p ")"
  | READ x -> p x; p " = input()"
  end

and p_stmts : int -> stmts -> unit
=fun indent stmts -> List.iter (fun stmt -> p_stmt indent stmt; p "\n") stmts

and p_block : int -> block -> unit
=fun indent (BLOCK stmts) ->
  p_indent indent; p "\n";
  p_stmts (indent + 4) stmts;
  p_indent indent; p "\n" 

and p_function_def : int -> function_def -> unit
=fun indent (FUNC (id, formals, block)) ->
  p_indent indent; p ("def " ^ id ^ "(");
  let n = List.length formals in
  List.iteri (fun i formal ->
      if i != n then print_string (formal ^ ", ")
      else print_string formal) formals;
  p "):\n";
  p_block indent block

let pp : program -> unit
=fun pgm ->
  List.iter (fun function_def -> p_function_def 0 function_def) pgm
