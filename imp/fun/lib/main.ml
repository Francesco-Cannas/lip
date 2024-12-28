open Ast
open Types

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let eval_decl (initial_state : state) (declarations : decl list) : state =
  let initial_environment, initial_location = topenv initial_state, getloc initial_state in
  let final_location, final_environment = 
    List.fold_left (fun (current_location, current_environment) declaration -> match declaration with
        IntVar identifier -> let updated_location = current_location + 1 in
          let updated_environment = bind_env current_environment identifier (IVar current_location) in
          (updated_location, updated_environment)
      | Fun (function_name, parameters, body, expression) -> let updated_location = current_location + 1 in
        let updated_environment = bind_env current_environment function_name (IFun (parameters, body, expression)) in
        (updated_location, updated_environment)) 
        (initial_location, initial_environment) declarations 
    in let updated_environment_stack = final_environment :: getenv initial_state in
    let updated_memory = getmem initial_state in
    make_state updated_environment_stack updated_memory final_location

let rec trace_expr (current_state : state) (expression : expr) : state * expr = match expression with
    True -> (current_state, True)
  | False -> (current_state, False)
  | Const number -> (current_state, Const number)
  | Var variable -> (match (topenv current_state) variable with
      IVar location -> (current_state, Const ((getmem current_state) location))
    | _ -> failwith "apply error")
  | Not expr -> (match trace_expr current_state expr with
      new_state, True -> (new_state, False)
    | new_state, False -> (new_state, True)
    | new_state, new_expr -> (new_state, Not new_expr))
  | And (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, False -> (new_state, False)
    | new_state, True -> (match trace_expr new_state expr2 with
      | final_state, final_expr -> (final_state, final_expr))
    | new_state, new_expr -> (new_state, And (new_expr, expr2)))
  | Or (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, True -> (new_state, True)
    | new_state, False -> (match trace_expr new_state expr2 with
      | final_state, final_expr -> (final_state, final_expr))
    | new_state, new_expr -> (new_state, Or (new_expr, expr2)))
  | Add (expr1, expr2) -> (match trace_expr current_state expr1 with
        new_state, Const num1 -> (match trace_expr new_state expr2 with
            final_state, Const num2 -> (final_state, Const (num1 + num2))
          | final_state, final_expr -> (final_state, Add (Const num1, final_expr)))
      | new_state, new_expr -> (new_state, Add (new_expr, expr2)))
  | Sub (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, Const num1 -> (match trace_expr new_state expr2 with
          final_state, Const num2 -> (final_state, Const (num1 - num2))
        | final_state, final_expr -> (final_state, Sub (Const num1, final_expr)))
    | new_state, new_expr -> (new_state, Sub (new_expr, expr2)))
  | Mul (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, Const num1 -> (match trace_expr new_state expr2 with
        final_state, Const num2 -> (final_state, Const (num1 * num2))
      | final_state, final_expr -> (final_state, Mul (Const num1, final_expr)))
    | new_state, new_expr -> (new_state, Mul (new_expr, expr2)))
  | Eq (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, Const num1 -> (match trace_expr new_state expr2 with
          final_state, Const num2 -> if num1 = num2 then (final_state, True) else (final_state, False)
        | final_state, final_expr -> (final_state, Eq (Const num1, final_expr)))
    | new_state, new_expr -> (new_state, Eq (new_expr, expr2)))
  | Leq (expr1, expr2) -> (match trace_expr current_state expr1 with
      new_state, Const num1 -> (match trace_expr new_state expr2 with
          final_state, Const num2 -> if num1 <= num2 then (final_state, True) else (final_state, False)
        | final_state, final_expr -> (final_state, Leq (Const num1, final_expr)))
    | new_state, new_expr -> (new_state, Leq (new_expr, expr2)))
  | Call (func, expr) -> (match trace_expr current_state expr with
      new_state, Const num -> (match (topenv new_state) func with
          IFun (param, cmd, body_expr) ->
            let new_env = bind_env (topenv new_state) param (IVar (getloc new_state)) in
            let new_mem = bind_mem (getmem new_state) (getloc new_state) num in
            let new_loc = getloc new_state + 1 in
            (make_state (pushenv new_state new_env) new_mem new_loc, CallExec (cmd, body_expr))
        | IVar _ -> raise (TypeError "Is not a function"))
    | new_state, new_expr -> (new_state, Call (func, new_expr)))
  | CallExec (cmd, expr) -> (match trace1 (Cmd (cmd, current_state)) with
      St new_state -> (new_state, CallRet expr)
    | Cmd (new_cmd, new_state) -> (new_state, CallExec (new_cmd, expr)))
  | CallRet expr -> (match trace_expr current_state expr with
      new_state, Const num -> (make_state (popenv new_state) (getmem new_state) (getloc new_state), Const num)
    | new_state, new_expr -> (new_state, CallRet new_expr))    
    
and trace1 (configuration : conf) : conf = match configuration with
    St _ -> raise NoRuleApplies
  | Cmd (Skip, current_state) -> St current_state
  | Cmd (Assign (variable, expression), current_state) -> (match expression with
    Const value -> (match (topenv current_state) variable with
        IVar location -> St (make_state (getenv current_state) (bind_mem (getmem current_state) location value) (getloc current_state))
      | _ -> raise (TypeError "not valid symbol"))
    | _ -> let new_state, new_expression = trace_expr current_state expression in
      Cmd (Assign (variable, new_expression), new_state))
  | Cmd (Seq (command1, command2), current_state) -> (match trace1 (Cmd (command1, current_state)) with
      Cmd (new_command1, new_state) -> Cmd (Seq (new_command1, command2), new_state)
    | St new_state -> Cmd (command2, new_state))
  | Cmd (If (condition, command1, command2), current_state) -> (match trace_expr current_state condition with
      new_state, True -> Cmd (command1, new_state)
    | new_state, False -> Cmd (command2, new_state)
    | _, Const _ -> raise (TypeError "unbound type")
    | new_state, new_condition -> Cmd (If (new_condition, command1, command2), new_state))
  | Cmd (While (condition, body), current_state) -> Cmd (If (condition, Seq (body, While (condition, body)), Skip), current_state)

let trace (num_steps : int) (Prog (declarations, command) : prog) : conf list =
  let initial_state = make_state [bottom_env] bottom_mem 0 in
  let initial_conf = Cmd (command, eval_decl initial_state declarations) in
  let rec trace_steps remaining_steps current_conf =
    if remaining_steps <= 0 then [current_conf]
    else
      try
        let next_conf = trace1 current_conf in current_conf :: trace_steps (remaining_steps - 1) next_conf
        with NoRuleApplies -> [current_conf]
  in trace_steps num_steps initial_conf  