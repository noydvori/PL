type 'a binary_tree = 
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec insert_bst bst value =
  match bst with
  | Empty -> Node (value, Empty, Empty)
  | Node (val2, left_bst, right_bst) ->
    if value < val2 then
      Node (val2, insert_bst left_bst value, right_bst)
    else
      Node (val2, left_bst, insert_bst right_bst value);;

let rec construct l =
  match l with
  | [] -> Empty
  | head :: tail -> insert_bst (construct tail) head
;;
