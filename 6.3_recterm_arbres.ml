(* GRADE:  100% *)
let rec size1 a = match a with
    Nil -> 0
  |Node(x,g,d) -> 1+size1 g + size1 d;;

let rec size' l n = match l with
    []-> n
  |t::q -> match t with
    |Nil -> size' (q) n
    |Node(x,g,d) ->
        size'(g::d::q )(n+1);;


let size a = size' [a] 0;;


