let rec lang1 lista = match lista with
    ['0'] -> true
  | ['1'] -> true
  | _:: rest_lista -> lang1 rest_lista
  | _ -> false

let rec alone lista = match lista with
    [] -> true
  | x::rest_lista when x = '1' -> alone rest_lista
  | _ -> false

let lang2 lista = match lista with
    [] -> true
  | ['0'] -> true
  | x::rest_lista when x = '1' -> alone rest_lista
  | x::rest_lista when x = '0' -> alone rest_lista
  | _ -> false

let rec aux lista = match lista with
    ['0';'1'] -> true
  | x::y::rest_lista when List.length lista mod 2 = 0 && x = '0' && y = '1' -> aux rest_lista  
  | _ -> false

let lastzero lista = match lista with
    [] -> false
  | x::rest_lista when x = '0' -> aux (List.rev rest_lista)
  | _ -> false

let lang3 lista = match lista with
    ['0';'0'] -> false
  | x::rest_lista when x = '0' && lastzero (List.rev rest_lista) -> true
  | _ -> false

let rec counter lista = match lista with
    [] -> 0
  | x::rest_lista when x = '1' -> 1+counter rest_lista
  | _::rest_lista -> counter rest_lista

let lang4 lista = match lista with 
    ['1';'1'] -> true
  | _ when counter lista = 2 -> true
  | _ -> false 

let rec lang5 lista = match lista with
    ['0';'0'] -> true
  | ['1';'1'] -> true  
  | x::y::rest_lista when (x = '0' && y = '0') || (x = '1' && y = '1') -> lang5 rest_lista
  | _ -> false
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers