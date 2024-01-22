let head lst = match lst with
| [] -> 0
| x::tail -> x;;

let tail lst = match lst with
| [] -> 0
| x::tail -> head(List.rev tail);;

let remove_head_tail lst =
  match lst with
  | [] | [_] -> []
  | _ :: tail -> List.rev (List.tl (List.rev tail));;

let rec arithmetic_hell_helper lst checksum sum acc =
  match lst with
  | [] -> acc
  | [x] ->
    if (x + sum) = checksum || (sum - x) = checksum then
      (acc + 1)
    else
      acc
  | x :: rest ->
    let plus = arithmetic_hell_helper rest checksum (sum + x) acc in
    let minus = arithmetic_hell_helper rest checksum (sum - x) acc in
    plus + minus;;

let arithmetic_hell lst = match lst with
| [] -> 0
| [x] -> 0
| [x;y] -> if x = y then 1 else 0
| l -> arithmetic_hell_helper ( remove_head_tail l) (tail l) (head l) 0;;
