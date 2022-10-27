(* GRADE:  95% *)
let rev_append left right =
  let rec aux a b= match a with
    |[] -> b
    |t::q -> aux q (t::b)
  in aux left right;;

let append left right =
  rev_append (List.rev left) right;;

let  insert x l =
  let rec aux x1 l1 acc =match l1 with
    | []-> rev_append acc [x1]
    | a::q ->
        if a <x1  then aux x1 q  (a::acc) 
        else if a=x1 then rev_append acc l1
        else rev_append acc (x1::a::q)
  in aux x l [];; 

let sort l =
  let rec aux li acc= match li with
      []-> acc
    |t::q -> aux q (insert t acc) 
  in aux l [];;

let rec union_sorted l1 l2 = 
  if List.length l1 =0 then l2
  else if List.length l2=0 then l1
  else if List.hd l1 < List.hd l2 then List.hd l1 :: union_sorted (List.tl l1) l2
  else if List.hd l1 = List.hd l2 then union_sorted (List.tl l1) l2
  else List.hd l2 :: union_sorted l1 (List.tl l2);;

let rec appartient e l= match l with
  |[] -> false;
  |a::q -> a=e || appartient e q ;;
let rec remove x = function
  | [] -> failwith "x not in list"
  | h::t -> if h = x then t else h::(remove x t);;

let rec inter_sorted a b= match a with
  | [] -> if b = [] then [] else  inter_sorted b a
  | h::t ->
      if appartient h b then
        let b' = remove h b in
        h::(inter_sorted t b')
      else
        inter_sorted t b;;

