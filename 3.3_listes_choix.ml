(* GRADE:  100% *)
let rec taille l= match l with
    []-> 0
  |a::b -> 1+ taille b;;

let rec nbElement l x = match l with
    []-> failwith " Error"
  |a::b -> if(x=0) then a else nbElement b (x-1);;

let choose l =
  nbElement l (Random.int (taille l));;

let rec appartient l x = match l with
    []-> false;
  |a::b-> if(x=a) then true else appartient b x;;


let supprimer l x = 
  let rec aux a b c= match a with
      []->c
    |t::q -> 
        if(t=b) then  c @ q
        else aux q b (t::c) 
  in aux l x [];;

let choose_elements l n =
  let rec aux a b acc= match b with
      0 -> acc
    |_->
        let temp = choose a
        in aux (supprimer a temp) (b-1) (temp::acc) 
  in aux l n [];;
  
