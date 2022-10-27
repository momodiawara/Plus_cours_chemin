(* GRADE:  100% *)
let  ligne (m : 'a Matrix.t) (i : int) : 'a list =
  let rec  aux m' acc i' j= match j with
      0 -> Matrix.get m' (i',0)::acc
    |_ -> aux m' (Matrix.get m' (i,j) :: acc) i' (j-1)
  in aux m [] i ((Matrix.size m) -1) ;;
    
  

let colonne (m : 'a Matrix.t) (j : int) : 'a list =
  let rec  aux m' acc i' j= match j with
      0 -> Matrix.get m' (0,i')::acc
    |_ -> aux m' (Matrix.get m' (j,i') :: acc) i' (j-1)
  in aux m [] j ((Matrix.size m) -1) ;;
  

let diagonale (m : 'a Matrix.t) (k : int) : 'a list =
  if(k=0) then 
    let rec  aux m' acc j= match j with
        0 -> Matrix.get m' (0,0)::acc
      |_ -> aux m' (Matrix.get m' (j,j) :: acc) (j-1)
    in aux m [] ((Matrix.size m) -1) 
  else 
    let rec  aux m' acc i' j=
      if(i'=(Matrix.size m) -1) then List.rev (Matrix.get m' (i',0)::acc)
      else aux m' (Matrix.get m' (i',j) :: acc) (i'+1) (j-1)
    in aux m [] 0 ((Matrix.size m) -1) ;;
  
let rec valeur_constante (l : 'a option list) : 'a option = match l with
  | [] -> None 
  | [x] -> x
  | x::q -> 
      if x = (valeur_constante q) then x 
      else None 
;;

let gagnant_lignes (m : 'a option Matrix.t) : 'a option =
  let rec aux m compt = 
    if compt = (Matrix.size m) then None
    else 
      let a = valeur_constante (ligne m compt) in 
      match a with
        None -> aux m (compt+1)
      | Some x -> Some x
  in aux m 0
;;

let gagnant_colonnes (m : 'a option Matrix.t) : 'a option =
  let rec aux m compt = 
    if compt = (Matrix.size m) then None
    else 
      let a = valeur_constante (colonne m compt) in 
      match a with
        None -> aux m (compt+1)
      | Some x -> Some x
  in aux m 0
;;

let gagnant_diagonales (m : 'a option Matrix.t) : 'a option =
  if valeur_constante (diagonale m 0)!= None then valeur_constante (diagonale m 0)
  else valeur_constante (diagonale m 1) 
;;

let gagnant (m : 'a option Matrix.t) : 'a option =
  if(gagnant_diagonales(m)!=None) then gagnant_diagonales (m)
  else if (gagnant_colonnes(m) !=None) then gagnant_colonnes(m)
  else if(gagnant_lignes(m)!=None) then gagnant_lignes(m)
  else None;;

let termine (m : 'a option Matrix.t) : bool =
  if( gagnant (m) !=None) then true
  else false
;;

