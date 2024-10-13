(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let rec addlist l =
  match l with
  | [] -> 0
  | header_element :: tail_element -> header_element + addlist tail_element