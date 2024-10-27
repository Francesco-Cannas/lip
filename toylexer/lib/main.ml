open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)
let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)
let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
(* Conta le occorrenze di un elemento in una lista *)
let rec counter n lista = match lista with 
    [] -> 0
  | x::restoflista when x = n -> 1 + counter n restoflista
  | _::restoflista -> counter n restoflista
 
let index_of element lista =
  let rec aux i = function
    | [] -> -1
    | x::xs -> if x = element then i else aux (i + 1) xs
  in aux 0 lista
    
let frequency n lista =
  let unique_tokens = List.sort_uniq compare lista in
  let token_counts = List.map (fun t -> (t, counter t lista)) unique_tokens in
  let rec aux acc count = function
    | [] -> List.rev acc
    | (t, tn)::xs when count < n -> aux ((t, tn)::acc) (count + 1) xs
    | _ -> List.rev acc
  in
  let sorted_token_counts = List.sort (fun (t1, count1) (t2, count2) ->
    let cmp = compare count2 count1 in
      if cmp = 0 then compare (index_of t1 lista) (index_of t2 lista) else cmp
      ) token_counts in aux [] 0 sorted_token_counts 

(*
let rec tronco n lista = match lista with  
    [] -> []
  | _::restoflista when n > 0 -> if n > 0 then tronco (n-1) restoflista else restoflista
  | _ -> lista

let frequency n lista =
  let unique_tokens = List.sort_uniq compare lista in
  let tronchi_tokens = tronco ((List.length unique_tokens) - n-1) unique_tokens in
  let final = List.map (fun t -> (t, counter t lista)) tronchi_tokens in
  List.rev(List.sort (fun (_, count1) (_, count2) -> compare count1 count2) final);;
*) 