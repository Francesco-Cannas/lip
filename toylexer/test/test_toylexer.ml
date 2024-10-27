open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

let%test "test_frequencies_2" =
  lexer "A=3; B=A+2" |> frequency 3 = [(ATOK "A", 2); (ASSIGN, 2); (CONST "3", 1)]

let%test "test_frequencies_3" =
  lexer "0x1F + 0x2A" |> frequency 3 = [(ETOK "0x1F", 1); (PLUS, 1); (ETOK "0x2A", 1)]

let%test "test_frequencies_4" =
  lexer "a=1; e=2; i=3" |> frequency 3 = [(ASSIGN, 3); (SEQ, 2); (BTOK "a", 1)]

let%test "test_frequencies_5" =
  lexer "-3.14 + 2.71" |> frequency 3 = [(DTOK "-3.14", 1); (PLUS, 1); (DTOK "2.71", 1)]