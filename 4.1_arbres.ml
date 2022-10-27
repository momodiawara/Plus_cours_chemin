(* GRADE:  100% *)
let rec size a = match a with
    Nil -> 0
  |Node(_,c,b)-> 1 + size c + size b;;


let rec depth a = match a with
    Nil -> 0
  |Node(_,c,b)-> 1 + max (depth c) (depth b);;


let rec sum a = match a with
    Nil-> 0
  |Node(x,a,b) -> x+ sum a +sum b;;
"Remplacer cette chaîne par votre code" ;;

let rec contains x a = match a with
    Nil -> false 
  |Node(r,b,c)-> 
      if(r=x) then true 
      else (contains x b) || (contains x c);;  
      
let rec elements a =match a with
    Nil ->[]
  |Node(x,g,d) -> elements g @[x]@ elements d;;
"Remplacer cette chaîne par votre code" ;;

let rec perfect a = match a with
    Nil -> true
  |Node(_,x,y) -> if(depth x != depth y) then false
      else perfect x && perfect y;;
"Remplacer cette chaîne par votre code" ;;

