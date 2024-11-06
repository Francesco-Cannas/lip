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
(* Funzione ricorsiva per contare le occorrenze di un elemento n in una lista *)
let rec counter n lista = match lista with 
    [] -> 0 (* Se la lista è vuota, ritorna 0 *)
  | x::restoflista when x = n -> 1 + counter n restoflista (* Se l'elemento corrente è uguale a n, aggiungi 1 e continua a contare nel resto della lista *)
  | _::restoflista -> counter n restoflista (* Altrimenti, continua a contare nel resto della lista *)

(* Funzione per trovare l'indice di un elemento in una lista *)
let index_of element lista =
  let rec aux i = function
    | [] -> -1 (* Se la lista è vuota, ritorna -1 *)
    | x::xs -> if x = element then i else aux (i + 1) xs (* Se l'elemento corrente è uguale all'elemento cercato, ritorna l'indice corrente, altrimenti continua a cercare incrementando l'indice *)
  in aux 0 lista (* Inizia la ricerca dall'indice 0 *)

(* Funzione per calcolare la frequenza degli elementi in una lista *)
let frequency n lista =
  let unique_tokens = List.sort_uniq compare lista in (* Ordina e rimuove i duplicati dalla lista *)
  let token_counts = List.map (fun t -> (t, counter t lista)) unique_tokens in (* Conta le occorrenze di ogni elemento unico *)
  let rec aux acc count = function
    | [] -> List.rev acc (* Se la lista è vuota, ritorna l'accumulatore invertito *)
    | (t, tn)::xs when count < n -> aux ((t, tn)::acc) (count + 1) xs (* Se il conteggio è inferiore a n, aggiungi l'elemento e il suo conteggio all'accumulatore e continua *)
    | _ -> List.rev acc (* Altrimenti, ritorna l'accumulatore invertito *)
  in
  let sorted_token_counts = List.sort (fun (t1, count1) (t2, count2) ->
    let cmp = compare count2 count1 in (* Confronta i conteggi in ordine decrescente *)
      if cmp = 0 then compare (index_of t1 lista) (index_of t2 lista) else cmp (* Se i conteggi sono uguali, confronta gli indici degli elementi *)
      ) token_counts in aux [] 0 sorted_token_counts (* Ordina i conteggi e chiama la funzione ausiliaria *)