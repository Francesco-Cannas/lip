{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let digit = ['0'-'9']
let capital_letter = ['A'-'Z']
let lowercase_vowel = ['a' 'e' 'i' 'o' 'u']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | capital_letter (letter | digit)* { ATOK (Lexing.lexeme lexbuf) }
  | lowercase_vowel + { BTOK (Lexing.lexeme lexbuf) }
  | letter* lowercase_vowel letter* { if String.length (Lexing.lexeme lexbuf) - String.length (Str.global_replace (Str.regexp "[aeiou]") "" (Lexing.lexeme lexbuf)) <= 1 
              then CTOK (Lexing.lexeme lexbuf) else ID (Lexing.lexeme lexbuf) }
  | ['-']? digit+ ['.'] digit* { DTOK (Lexing.lexeme lexbuf) }
  | "0x" hex_digit+ | "0X" hex_digit+ { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }