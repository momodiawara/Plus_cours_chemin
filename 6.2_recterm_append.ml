(* GRADE:  100% *)
let rev_append left right =
  let rec aux a b= match a with
    |[] -> b
    |t::q -> aux q (t::b)
  in aux left right;;


let append left right =
  rev_append (List.rev left) right;;

