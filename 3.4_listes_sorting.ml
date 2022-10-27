(* GRADE:  100% *)
let rec insert x l = match l with
    []->[x]
  |a::b -> if (a>x) then x::l
      else if (a==x) then l
      else a::(insert x b);;

let rec sort l = match l with
    []->[]
  |[a]->[a]
  |a::b -> insert a(sort b);;

let rec mem_sorted x l = match l with
    []-> false
  |a::b -> if(a=x) then true
      else if (a>x) then false
      else mem_sorted x b;;

let rec union_sorted l1 l2 = match l1 with
    []-> l2
  |a::b -> union_sorted b (insert a l2);;
let rec appartient x l= match l with
    []-> false
  |a::b ->if(a=x) then true else appartient x b;;  
let rec inter_sorted l1 l2 = match l1 with
    []-> []
  |t::q -> if(appartient t l2) then t::inter_sorted q l2 else inter_sorted q l2;; 

let rec partition l x = match l with
    []->[],[]
  |a::b -> 
      let (l1,l2) = partition b x in 
      if a< x then ((a::l1), l2) else (l1,(a::l2));;
let rec quicksort l = match l with
  |[]->[];
  |a::q ->
      let (l1,l2)=partition q a in
      (quicksort l1)@(a::(quicksort l2));;


