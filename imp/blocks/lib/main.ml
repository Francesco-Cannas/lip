open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
ast

let lookup_var state v = let env1 = topenv state in 
  match env1 v with 
    BVar l 
  | IVar l -> (getmem state) l 

let rec eval_expr : state -> expr -> memval =
  fun state e -> match e with
    True -> Bool true 
  | False -> Bool false
  | Const n -> Int n 
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
      Int n1, Int n2 -> Int (n1 + n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Sub(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Int n1, Int n2 -> Int (n1 - n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Mul(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Int n1, Int n2 -> Int (n1 * n2) 
    | _ -> raise (TypeError "Expected integer")) 
  | Eq(e1, e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Int n1, Int n2 -> Bool (n1 = n2)
    | Bool b1, Bool b2 -> Bool (b1 = b2)
    | _ -> raise (TypeError "Incompatible value"))
  | Leq(e1,e2) -> (match eval_expr state e1, eval_expr state e2 with 
      Int n1, Int n2 -> Bool (n1 <= n2) 
    | _ -> raise (TypeError "Incompatible value"))
  | Var v -> lookup_var state v

let eval_decl : state -> decl list -> state =
  fun st ds ->
    let env, loc = List.fold_left (fun (env, loc) d -> match d with
      IntVar x -> (bind_env env x (IVar loc), loc + 1)
    | BoolVar x -> (bind_env env x (BVar loc), loc + 1)) 
    (topenv st, getloc st) ds in
  make_state (env :: getenv st) (getmem st) loc

let rec trace1 : conf -> conf = function
    Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) ->
      let env, mem = (topenv st, getmem st) in
      let new_mem = match eval_expr st e with
          Int n -> (match env x with 
              IVar i -> bind_mem mem i (Int n) 
            | _ -> raise (TypeError "Incompatible value"))
        | Bool b -> (match env x with 
            BVar i -> bind_mem mem i (Bool b) 
          | _ -> raise (TypeError "Incompatible value"))
      in St (make_state (getenv st) new_mem (getloc st))
  | Cmd (Seq (c1, c2), st) -> ( match trace1 (Cmd (c1, st)) with
      Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
    | St st' -> Cmd (c2, st'))
  | Cmd (If (e, c1, c2), st) -> (match eval_expr st e with
      Bool b -> if b then Cmd (c1, st) else Cmd (c2, st)
    | _ -> raise (TypeError "Incompatible value"))
  | Cmd (While (e, c), st) -> (match eval_expr st e with
      Bool true -> Cmd (Seq (c, While (e, c)), st)
    | Bool false -> St st
    | _ -> raise (TypeError "Incompatible value"))
  | Cmd (Decl (dl, c), st) -> let new_st = eval_decl st dl in
    (match trace1 (Cmd (c, new_st)) with
      St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
    | Cmd (c', st') -> Cmd (Block c', st'))
  | Cmd (Block c, st) -> (match trace1 (Cmd (c, st)) with
      St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
    | Cmd (c', st') -> Cmd (Block c', st'))
  | _ -> raise NoRuleApplies

let initial_state : state = make_state [bottom_env] bottom_mem 0
  
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