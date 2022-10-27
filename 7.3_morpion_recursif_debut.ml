(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (function _ -> Matrix.init taille (function _ -> None))
;;

let ligne (m : 'a Matrix.t) (i : int) : 'a list =
  let rec aux m i acc compt =
    if compt=(Matrix.size m) then acc
    else aux m i (Matrix.get m (i,compt)::acc) (compt+1)
  in List.rev (aux m i [] 0)
;;

let colonne (m : 'a Matrix.t) (j : int) : 'a list =
  let rec aux m i acc compt =
    if compt=(Matrix.size m) then acc
    else aux m i (Matrix.get m (compt,j)::acc) (compt+1)
  in List.rev (aux m j [] 0)
;;

let diagonale (m : 'a Matrix.t) (k : int) : 'a list =
  if k = 0 then 
    let rec aux m acc compt = 
      if compt=(Matrix.size m) then acc
      else aux m (Matrix.get m (compt,compt)::acc) (compt+1) 
    in List.rev (aux m [] 0)
  else if k = 1 then
    let rec aux2 m acc compt1 compt2 = 
      if compt1=(Matrix.size m) then acc
      else aux2 m (Matrix.get m (compt1,compt2)::acc) (compt1+1) (compt2-1)
    in List.rev (aux2 m [] 0 ((Matrix.size m) -1))
  else failwith"mauvais chiffre"
;;

let rec valeur_constante (l : 'a option list) : 'a option = match l with
  | [] -> None 
  | [x] -> x
  | x::reste -> if x = (valeur_constante reste) then x else None
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
  if gagnant_lignes m != None then gagnant_lignes m
  else if gagnant_colonnes m != None then gagnant_colonnes m
  else gagnant_diagonales m
;;

let sous_morpion_termine m = 
  let rec aux m compt = 
    if List.mem None (ligne m compt) then false else true && aux m (compt+1)
  in aux m 0
;; 

let sous_morpion_valide
    (plateau : plateau)
    (dernier_coup : coup option)
    ((i,j) : int * int)
  : bool = match dernier_coup with
    None -> true
  | Some e -> 
      let sous_morpion_prec = Matrix.get plateau (e.position_dim_2) and sous_morpion_act = Matrix.get plateau (i,j) 
      and sous_morpion_aux = Matrix.get plateau (e.position_dim_1) in
      if sous_morpion_termine sous_morpion_act then false 
      else if (gagnant sous_morpion_aux)!=None then true
      else sous_morpion_act == Matrix.get plateau (e.position_dim_1)
;; 
  
" if gagnant sous_morpion_act!=None then true
      else"
let coup_legal
    (plateau : plateau)
    (dernier_coup : coup option)
    (coup : coup) 
  : bool = match dernier_coup with
    None -> (Matrix.get (Matrix.get plateau (coup.position_dim_2)) coup.position_dim_1)=None
  | Some e -> (sous_morpion_valide plateau dernier_coup coup.position_dim_2) && (Matrix.get (Matrix.get plateau (coup.position_dim_2)) coup.position_dim_1)=None
;;


let pos_sous_morpion x =
  if x = 0 || x = 1 || x = 2 then 0
  else if x = 3 || x = 4 || x = 5 then 1
  else if x = 6 || x = 7 || x = 8 then 2
  else failwith"Error";;

let coup_des_coordonnees_absolues
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    ((x,y) : int * int)
  : coup option =  if (x>=9 || y>=9 || x<0 || y<0)  then None else
    match dernier_coup with
    | None -> Some {joueur=0;position_dim_2=(pos_sous_morpion x, pos_sous_morpion y);position_dim_1=(x mod 3,y mod 3)}
    | Some e -> Some {joueur=((e.joueur + 1) mod nb_joueurs);position_dim_2=(pos_sous_morpion x, pos_sous_morpion y);position_dim_1=(x mod 3,y mod 3)}
;;

