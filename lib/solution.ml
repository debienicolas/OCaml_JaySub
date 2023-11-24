type program = Ast.program
type store (* = ??? *)

open Jaysub.Store

let parse      = Parser.program Lexer.token (* Do not change *)

let eval_statement s id statement = match statement with
  | ModStmt 
  | SwapStmt 

(* Given current store, identifier and list of statements *)
let eval_statements s id statements = match statements with
  | [] -> s
  | statement :: statements' -> 
      let s' = eval_statement s id statement in
      eval_statements s' id statements'

let feval  (ids,procedures) = 
  let rec eval_procedures s procs = match procs with
    | [] -> s
    | (id, statements) :: procs' -> 
          let s' = eval_statements s id statements in
          eval_procedures s' procs'
  in
  let init_store = create ids in
  eval_procedures init_store procedures






let beval      = fun _ -> failwith "implement me!"
let invert     = fun _ -> failwith "implement me!"
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

let string_of_program = Ast.string_of_program (* Do not change *)
let string_of_store   = fun _ -> failwith "implement me!"