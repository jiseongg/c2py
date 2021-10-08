
(* translation of expression *)
let rec trans_e : C.exp -> Python.exp
= fun exp ->
  match exp with
  | C.NUM x -> Python.NUM x
  | C.LV (C.ID id) -> Python.LV (Python.ID id)
  | C.LV (C.ARR (id, exp)) -> Python.LV (Python.ARR (id, trans_e exp))
  | C.ADD (exp1, exp2) -> Python.ADD (trans_e exp1, trans_e exp2)
  | C.SUB (exp1, exp2) -> Python.SUB (trans_e exp1, trans_e exp2)
  | C.MUL (exp1, exp2) -> Python.MUL (trans_e exp1, trans_e exp2)
  | C.DIV (exp1, exp2) -> Python.DIV (trans_e exp1, trans_e exp2)
  | C.MINUS exp -> Python.MINUS (trans_e exp)
  | C.NOT exp -> Python.NOT (trans_e exp)
  | C.LT (exp1, exp2) -> Python.LT (trans_e exp1, trans_e exp2)
  | C.LE (exp1, exp2) -> Python.LE (trans_e exp1, trans_e exp2)
  | C.GT (exp1, exp2) -> Python.GT (trans_e exp1, trans_e exp2)
  | C.GE (exp1, exp2) -> Python.GE (trans_e exp1, trans_e exp2)
  | C.EQ (exp1, exp2) -> Python.EQ (trans_e exp1, trans_e exp2)
  | C.AND (exp1, exp2) -> Python.AND (trans_e exp1, trans_e exp2)
  | C.OR (exp1, exp2) -> Python.OR (trans_e exp1, trans_e exp2)
  | C.CALL (id, exprs) -> Python.CALL (id, List.map trans_e exprs)

(* translation of statements*)
and trans_s : C.stmt -> Python.stmt
=fun stmt ->
  match stmt with
  | C.ASSIGN (C.ID id, exp) -> Python.ASSIGN (Python.ID id, trans_e exp)
  | C.ASSIGN (C.ARR (id, exp1), exp2) -> Python.ASSIGN (Python.ARR (id, trans_e exp1), trans_e exp2)
  | C.IF (exp, then_block, else_block) -> Python.IF (trans_e exp, trans_block then_block, trans_block else_block)
  | C.WHILE (exp, block) -> Python.WHILE (trans_e exp, trans_block block)
  | C.DOWHILE (block, exp) -> Python.DOWHILE (trans_block block, trans_e exp)
  | C.READ id -> Python.READ id
  | C.PRINT exp -> Python.PRINT (trans_e exp)

and trans_block : C.block -> Python.block
= fun (C.BLOCK (decls, stmts)) ->
  Python.BLOCK (List.map trans_s stmts)

let trans_formal : C.formal -> Python.formal
= fun c_formal -> snd c_formal 

let rec trans_f : C.function_def -> Python.function_def
= fun (C.FUNC (id, formals, block)) -> Python.FUNC (id, List.map trans_formal formals, trans_block block)

let translate : C.program -> Python.program
=fun function_defs ->
  List.map trans_f function_defs
