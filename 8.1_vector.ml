(* GRADE:  26% *)
let create t valeur =
  {
    contents = Array.make t valeur;
    default=valeur;
    size=0;
  }
let of_list l d c =
  if(List.length l > c) then failwith "la capacite est petite"
  else 
    let x = create c d
    in
    let rec aux l'= match l' with
      |[]-> x
      |a::q -> 
          x.contents.(x.size)<-a;
          x.size<- x.size+1;
          aux q
    in aux l
;;

let get v i =
  if(i<0 || i>=v.size ) then raise   (Invalid_argument "get")
  else v.contents.(i);;

(*let set v i x=
   if(i<0 || i>= v.size || v.contents.(i)=None) then Invalid_argument "set"
   else  v.contents.(i)<-x;;*)



