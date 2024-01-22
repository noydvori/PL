let rec sum_list l = match l with
| [] -> 0
| x :: u -> x + sum_list u;;

let rec compress l = match l with
| one :: (two :: tail) -> if (one = two) then compress(two::tail) else one::(compress (two::tail))
| smaller -> smaller;;
