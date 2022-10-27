(* GRADE:  88% *)
let rec size a = match a with
    Nil -> 0
  |Node(_,c,b)-> 1 + size c + size b;;

let rec contains_bst x a = match a with
  |Nil -> false
  |Node(b,g,d) ->
      if(x=b) then true
      else if (x>b) then contains_bst x d
      else contains_bst x g;;

let rec add_bst x a = match a with
    Nil -> Node(x,Nil,Nil)
  |Node(r,g,d)->
      if( contains_bst x a) then a
      else if(x>r) then Node(r,g, add_bst x d)
      else Node(r,add_bst x g, d);;
      
let rec bst_of_list l = match l with
  |[]-> Nil
  |a::b -> add_bst a (bst_of_list b);;

let divise l = 
  let rec aux i acc = function
      [] -> List.rev acc, []
    | h :: t as l -> if i=0 then List.rev acc,l
        else aux (i-1) (h::acc) t in 
  aux ((List.length l) /2) [] l;;

let rec bst_of_list_opt l = match l with
    []-> Nil
  |t::q -> add_bst t (bst_of_list_opt q);;




let rec est_triee l= match l with
    []|[_]-> true
  |a::y::q ->  a<=y && est_triee (y::q);; 

let rec elements a = match a with
    Nil ->[]
  |Node(x,y,z) -> elements y @[x]@ elements z;;

let is_bst a = est_triee (elements a);; 

