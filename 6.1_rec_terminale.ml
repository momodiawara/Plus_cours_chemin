(* GRADE:  100% *)
let listn1 n =
  let rec aux e l = 
    if e < 0 then l
    else  aux (e-1) (e::l)
  in List.rev (aux n []);;


let length1 l =
  let rec aux e acc = match e with
      []->acc
    |t::q -> aux q (acc+1)
  in aux l 0;;
    
