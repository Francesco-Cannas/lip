type ast =
    Const of int
  | Add of ast * ast
  | Diff of ast * ast
  | Negs of ast 
  | Molt of ast * ast
  | Divs of ast * ast