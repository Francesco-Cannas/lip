open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
ast

let rec eval_expr (state : state) (expr : expr) : exprval = match expr with 
    True -> Bool true 
  | False -> Bool false 
  | Var x -> (try state x with _ -> raise (UnboundVar x)) 
  | Const n -> Nat n 
  | Not e -> (match eval_expr state e with 
      Bool b -> Bool (not b) 
    | _ -> raise (TypeError "Expected boolean")) 
  | And(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Bool b1, Bool b2 -> Bool (b1 && b2) 
    | _ -> raise (TypeError "Expected boolean")) 
  | Or(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Bool b1, Bool b2 -> Bool (b1 || b2) 
    | _ -> raise (TypeError "Expected boolean")) 
  | Add(e1,e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Nat n1, Nat n2 -> Nat (n1 + n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Sub(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Nat n1, Nat n2 -> Nat (n1 - n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Mul(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Nat n1, Nat n2 -> Nat (n1 * n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Eq(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Nat n1, Nat n2 -> Bool (n1 = n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Leq(e1,e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Nat n1, Nat n2 -> Bool (n1 <= n2) 
    | _ -> raise (TypeError "Expected integer"))

let bind st v x y = 
  if x = y then v else st y

let rec trace1 (conf : conf) : conf = match conf with
    Cmd(Skip, st) -> St(st) 
  | Cmd(Assign(x, e), st) -> (match eval_expr st e with
    | Nat v -> St(bind st (Nat v) x) 
    | _ -> raise (TypeError "Expected integer"))
  | Cmd(Seq(c1, c2), st) -> (match trace1 (Cmd(c1, st)) with
    | Cmd(c1', st') -> Cmd(Seq(c1', c2), st') 
    | St(st') -> Cmd(c2, st')) 
  | Cmd(If(e, c1, c2), st) -> (match eval_expr st e with
    | Bool true -> Cmd(c1, st)
    | Bool false -> Cmd(c2, st) 
    | _ -> raise (TypeError "Expected boolean"))
  | Cmd(While(e, c), st) -> (match eval_expr st e with
    | Bool true -> Cmd(Seq(c, While(e, c)), st) 
    | Bool false -> St(st) 
    | _ -> raise (TypeError "Expected boolean"))
  | _ -> raise NoRuleApplies

let initial_state : state = function _ -> Nat 0
  
let trace n cmd =
  let initial_conf = Cmd(cmd, initial_state) in
    let rec trace_aux n conf =
      if n <= 0 then [conf]
      else
        try
          let conf' = trace1 conf in
          conf :: trace_aux (n - 1) conf'
        with NoRuleApplies -> [conf]
  in trace_aux n initial_conf