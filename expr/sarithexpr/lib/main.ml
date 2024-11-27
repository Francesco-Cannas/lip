open Ast

type exprtype = 
    BoolT 
  | NatT 

let string_of_val = function
    Bool b -> string_of_bool b
  | Nat n -> string_of_int n

let string_of_type = function
    BoolT -> "Bool"
  | NatT -> "Nat"

let rec string_of_expr = function
    True -> "True"
  | False -> "False" 
  | Zero -> "0"
  | Succ(e0) -> "Succ(" ^ (string_of_expr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_expr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ (string_of_expr e0) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | And(e0, e1) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ", False)"  
  | Or(e0, e1) -> "If(" ^ (string_of_expr e0) ^ ",True," ^ (string_of_expr e1) ^ ")"  
  | If(e0, e1, e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"

exception TypeError of string
let rec typecheck = function
    True -> BoolT 
  | False -> BoolT
  | Not e -> (match typecheck e with 
      BoolT -> BoolT
    | _ -> raise (TypeError "Type error"))
  | And (e1, e2) -> (match typecheck e1, typecheck e2 with 
      BoolT, BoolT -> BoolT
    | _ -> raise (TypeError "Type error"))
  | Or (e1, e2) -> (match typecheck e1, typecheck e2 with 
      BoolT, BoolT -> BoolT
    | _ -> raise (TypeError "Type error"))
  | If (e1, e2, e3) -> (match typecheck e1, typecheck e2, typecheck e3 with 
      BoolT, BoolT, BoolT -> BoolT
    | BoolT , NatT , NatT -> NatT
    | _ -> raise (TypeError "Type error"))
  | Zero -> NatT
  | IsZero e -> (match typecheck e with 
      NatT -> BoolT
    | _ -> raise (TypeError "Type error"))
  | Pred e -> (match typecheck e with 
      NatT -> NatT
    | _ -> raise (TypeError "Type error"))
  | Succ e -> (match typecheck e with 
      NatT -> NatT
    | _ -> raise (TypeError "Type error"))

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  let rec desugar = function 
    If(e0, e1, e2) -> If(desugar e0, desugar e1, desugar e2)
      | Not(e0) -> If(desugar e0, False, True)
      | And(e0, e1) -> If(e0, e1, False) 
      | Or(e0, e1) -> If(e0, True, e1) 
      | Succ(e0) -> Succ(desugar e0)
      | Pred(e0) -> Pred(desugar e0)
      | IsZero(e0) -> IsZero(desugar e0)
      | e -> e 
    in desugar ast
exception NoRuleApplies

let rec trace1 = function
    If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | Not(e1) -> trace1 (If(e1, False, True))
  | And(e1, e2) -> trace1 (If(e1, e2, False))
  | Or(e1, e2) -> trace1 (If(e1, True, e2))
  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(e)) -> e
  | Pred(e) -> Pred(trace1 e) 
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | If(e1, e2, e3) -> let e1' = trace1 e1 in If(e1', e2, e3)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Not(e) -> (match eval e with 
      Bool b -> Bool (not b) 
    | _ -> failwith "Type error")
  | And(e1, e2) -> (match eval e1, eval e2 with 
      Bool b1, Bool b2 -> Bool (b1 && b2) 
    | _ -> failwith "Type error")
  | Or(e1, e2) -> (match eval e1, eval e2 with 
        Bool b1, Bool b2 -> Bool (b1 || b2) 
      | _ -> failwith "Type error")
  | If(e1, e2, e3) -> (match eval e1 with 
      Bool true -> eval e2 
    | Bool false -> eval e3 
    | _ -> failwith "Type error")
  | Zero -> Nat 0
  | Succ(e) -> (match eval e with 
      Nat n -> Nat (n + 1) 
    | _ -> failwith "Type error")
  | Pred(e) -> (match eval e with 
      Nat n when n > 0 -> Nat (n - 1) 
    | _ -> failwith "Type error")
  | IsZero(e) -> (match eval e with 
      Nat n -> Bool (n = 0) 
    | _ -> failwith "Type error")