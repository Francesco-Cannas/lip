type boolExpr =
    True
  | False
  | Not of boolExpr
  | And of boolExpr * boolExpr
  | Or of boolExpr * boolExpr
  | If of boolExpr * boolExpr * boolExpr
;;

let is_value : boolExpr -> bool = function
  | True -> true
  | False -> true
  | _ -> false