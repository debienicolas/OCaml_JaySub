type program = Ast.program
open Ast
open Store

let parse      = Parser.program Lexer.token (* Do not change *)


(* This function evaluates an expression and returns its integer value*)


(* Given current store, identifier and list of statements *)



let rec feval ((ids,procedures) : Ast.program) :  store = 
          Printf.printf "feval: %s\n" (Ast.show_program (ids,procedures));
  let init_store = create ids in
  let last_proc = last_elem_proc procedures in
  Printf.printf "last_proc: %s\n" (Ast.show_procedure last_proc);
  eval_procedure init_store last_proc procedures
  and last_elem_proc lst = 
      match lst with
      | [] -> failwith "Not a single procedure was given to the program"
      | [x] -> x
      | _:: xs -> last_elem_proc xs 

  and eval_procedure s (_,statements) p =
    eval_statements s statements p
    
  and  eval_statements s statements p= 
    match statements with
    | [] -> s
    | statement :: statements' -> 
        let s' = eval_statement s statement p in
        eval_statements s' statements' p
    
  and eval_statement s statement p = 
      match statement with
      | ModStmt (id, op, e) -> 
          let v = eval_expression s e in
          let original = get_value s id in
          let new_value = match op with
            | Plus -> original + v
            | Min -> original - v
            | Mult -> original * v
            | Div -> original / v
            | _ -> failwith "Invalid operator for ModStmt" in
          set_value s id new_value
      | SwapStmt (id1, id2) -> 
          let v1 = get_value s id1 in
          let v2 = get_value s id2 in
          let s = set_value s id1 v2 in
          set_value s id2 v1
      | IfStmt (cond,then_branch,else_branch,assert_expr) -> 
          (* check if the conditional is true*)
          if (eval_expression s cond) <> 0 then
            let s' = eval_statements s then_branch p in
            if (eval_expression s' assert_expr) = 0 then failwith "Assertion failed in then branch"
            else s'
          else
            let s' = eval_statements s else_branch p in
            if (eval_expression s' assert_expr) <> 0 then failwith "Assertion failed in else branch"
            else s'
      | CallStmt id -> eval_statements s (find_procedure id p) p
      | UncallStmt _ -> failwith "implement me UncallStmt!"
      | SkipStmt -> s
      | LoopStmt (_,_,_) -> failwith "implement me LoopStmt!"

    and eval_expression s e = 
        Printf.printf "eval_expression: %s\n" (Ast.string_of_expression e);
        match e with
        | Ident id -> get_value s id
        | Constant c -> c
        | Binop (e1, op, e2) -> 
            let v1 = eval_expression s e1 in
            let v2 = eval_expression s e2 in
            match op with
            | Plus -> v1 + v2
            | Min -> v1 - v2
            | Mult -> v1 * v2
            | Div -> v1 / v2
            | Eq -> if v1 = v2 then 1 else 0
            | NEq -> if v1 <> v2 then 1 else 0
    and find_procedure id p =
      Printf.printf "find_procedure with id: %s\n" id;
      match p with
      | [] -> failwith "No procedure with given id"
      | (id2,proc)::procs -> 
        Printf.printf "find_procedure current id: %s\n" id2;
        if id2 = id then proc else find_procedure id procs






let beval      = fun _ -> failwith "implement me beval !"
let invert     = fun _ -> failwith "implement me invert!"
let optimize   = fun _ ->
  let constant_fold  = fun _ -> failwith "implement me" in
  let dead_code_elim = fun _ -> failwith "implement me" in
  let proc_inline    = fun _ -> failwith "implement me" in
  failwith "implement me!"

let string_of_program = Ast.string_of_program (* Do not change *)

let string_of_store = string_of_store