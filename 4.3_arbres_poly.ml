(* GRADE:  100% *)
let rec forall_labels p a = match a with
  |Nil ->true
  |Node(x,g,d) -> 
      if(p x = false) then false
      else forall_labels p g && forall_labels p d;;
"Remplacez cette chaîne par votre code" ;;

let is_uniform x a =
  forall_labels (fun y -> if y=x then true else false) a;;
"Remplacez cette chaîne par votre code" ;;

let rec forall_subtrees pn a = match a with
  |Nil -> true
  |Node(x,g,d) ->
      if( pn x g d = false) then false
      else (forall_subtrees pn g )&& (forall_subtrees pn d);;

let rec is_right_comb a = match a with
    Nil -> true
  |Node (_,g,d) -> if g=Nil then is_right_comb d else false;;
  
let rec sum a= match a with
    Nil -> 0
  |Node(x,g,d) -> x+ sum g + sum d;;

let rec map_tree f a = match a with
    Nil -> Nil
  |Node(x,g,d) -> Node(f x, map_tree f g, map_tree f d);;
"Remplacez cette chaîne par votre code" ;;

