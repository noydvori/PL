type shape = Circle of float | Square of float | Rectangle of float * float;;

let area s = match s with
| Circle radius -> 3.14159 *. radius *. radius
| Square a -> a *. a
| Rectangle (a , b) -> a *. b;;

let rec total_area shape_list = match shape_list with
| [] -> 0.
| head :: tail -> (area head) +. (total_area tail);;
