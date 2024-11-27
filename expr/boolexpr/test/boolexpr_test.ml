open Boolexpr.Main
open Boolexpr.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)
let%test "test_eval_false" = test_eval "false" false
let%test "test_eval_true" = test_eval "true" true
let%test "test_eval_if_true_then_false_else_true" = test_eval "if true then false else true" false
let%test "test_eval_if_false_then_false_else_true" = test_eval "if false then false else true" true
let%test "test_eval_nested_if_1" = test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval_nested_if_2" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval_nested_if_3" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)
let%test "trace1_if_true_then_false" = 
  let expr = parse "if true then false else true" in
  trace1 expr = parse "false"

let%test "trace1_if_false_then_true" = 
  let expr = parse "if false then false else true" in
  trace1 expr = parse "true"

let%test "trace1_nested_if" = 
  let expr = parse "if true then (if true then false else true) else (if true then true else false)" in
  trace1 expr = parse "if true then false else true"

let%test "trace1_no_progress_is_value_true" = 
  let expr = parse "true" in
  try 
    let _ = trace1 expr in 
    false 
  with NoRuleApplies -> is_value expr

let%test "trace1_no_progress_is_value_false" = 
  let expr = parse "false" in
  try 
    let _ = trace1 expr in 
    false 
  with NoRuleApplies -> is_value expr

let%test "trace_fully_reduced_in_10_steps" = 
  let expr = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" in
  let steps = trace expr in
  List.length steps <= 10

(* ### Unit tests for task 6 *)  
let%test "test_eval_not_true_or_true" = test_eval "not true || true" true
let%test "test_eval_not_true_and_false" = test_eval "not true && false" false
let%test "test_eval_false_and_false_or_true" = test_eval "false && false || true" true
let%test "test_eval_true_or_false_and_false" = test_eval "true || false && false" true
let%test "test_eval_if_true_then_true_else_false_and_false" = test_eval "if true then true else false && false" true
let%test "test_eval_if_true_then_false_else_false_or_true" = test_eval "if true then false else false || true" false