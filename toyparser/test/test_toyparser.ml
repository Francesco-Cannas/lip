open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "2 * 3 - 1 + 4 / 2" |> eval = Ok 7
let%test "test_eval_3" = parse "2 / 0" |> eval = Error "Division by zero"
let%test "test_eval_2" = parse "-2 * 3 - 1 - 4 / 2" |> eval = Ok (-9)