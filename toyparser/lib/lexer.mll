{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | "0x" hex_digit+ | "0X" hex_digit+ { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
