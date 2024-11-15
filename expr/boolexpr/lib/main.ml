open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"
  | And(e0, e1) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ",False)"  
  | Or(e0, e1) -> "If(" ^ (string_of_boolexpr e0) ^ ",True," ^ (string_of_boolexpr e1) ^ ")"  
  | If(e0, e1, e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"

let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  let rec desugar = function 
    If(e0, e1, e2) -> If(desugar e0, desugar e1, desugar e2)
      | Not(e0) -> If(desugar e0, False, True)
      | And(e0, e1) -> If(e0, e1, False) 
      | Or(e0, e1) -> If(e0, True, e1) 
      | e -> e 
    in desugar ast
exception NoRuleApplies

let rec trace1 = function
    If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | Not(e1) -> trace1 (If(e1, False, True))
  | And(e1, e2) -> trace1 (If(e1, e2, False))
  | Or(e1, e2) -> trace1 (If(e1, True, e2))
  | If(e1, e2, e3) -> let e1' = trace1 e1 in If(e1', e2, e3)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let rec eval = function
    True -> true
  | False -> false
  | Not(e) -> not (eval e)
  | And(e1, e2) -> (match eval e1 with
      true -> eval e2 
    | false -> false)
  | Or(e1, e2) -> (match eval e1 with 
      true -> true 
    | false -> eval e2)
  | If(e1, e2, e3) -> (match eval e1 with
      true -> eval e2
    | false -> eval e3) 