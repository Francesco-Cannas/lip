open Types

(* Use this grammar structure as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let rec count_zero list = match list with
  | [] -> 0
  | x::xs -> if x = 0 then 1 + count_zero xs else count_zero xs

let rec count_one list = match list with
  | [] -> 0
  | x::xs -> if x = 1 then 1 + count_one xs else count_zero xs

let zero_n_one_n (list : int list) : grammar =
  let zeros = count_zero list in
    let ones = count_one list in 
    {
      symbols = [S; A; B];
      terminals = ['0'; '1'];
      productions = [
        S --> (String.make zeros '0' ^ String.make ones '1');
        A --> "0";
        B --> "1"
      ];
      start = S;
    }

(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = todo

(* #### Exercise 3, medium (balanced_parentheses)*)
let balanced_parentheses : grammar = todo

(* #### Exercise 4, hard (same_amount)

   Hint 1: you can use 'a' and 'b' for terminals.
   Hint 2: think of the language of words where the number of 0s is one greater
   than the number of 1s and viceversa, then combine them.
*)
let same_amount : grammar = todo