
type couleur = Rouge | Jaune;;
type jeton = {
  pos : (int * int);
  coul : couleur;
};;

type arb =
    Vide
  | Feuille of int
  | Cons of  (arb list * int);;

(*CONSTANTES - DEBUT*)
let largeGrille = 7;;
let hautGrille = 6;;
(*CONSTANTES - FIN*)


let grille = [];;
(*JETONS DE TEST - DEBUT *)
let j1 = {pos = (1,1); coul = Rouge;};;
let j2 = {pos = (1,3); coul = Rouge;};;
let j3 = {pos = (3,5); coul = Jaune;};;
(*JETONS DE TEST - FIN*)

exception ColonnePleine;;

let max a b =
  if a > b then a
  else b;;

let min a b = 
  if a < b then a
  else b;;

let succCoul = function
    Rouge -> Jaune
  | Jaune -> Rouge;;

let estSupEgalJeton j1 j2 = 
  if (fst j1.pos) > (fst j2.pos) then
    true
 else 
   if (fst j1.pos) = (fst j2.pos) then
     if (snd j1.pos) >= (snd j2.pos) then
       true
     else 
       false 
   else 
     false;;

let rec  insertDecroisLex x l = match l with 
    []   -> [x]
  |e::l' -> 
    if not (estSupEgalJeton x e) then
      e::(insertDecroisLex x l')
    else 
      x::e::l';;
      
    
let rec estPlace x l = match l with
    [] -> false 
  | e::l' when  (estSupEgalJeton x e) -> 
    if x.pos = e.pos then
      true 
    else 
      false 
  |e::l' -> (estPlace x l');;

let rec sommetColonne col l = match l with
    [] -> (col, 0)
  | e::l' when (fst e.pos) < col -> (col, 0)
  | e::l' when (fst e.pos) = col -> e.pos
  | e::l'                        -> (sommetColonne col l');;

let ajoutJeton col l c= 
  let sommet = (sommetColonne col l) in
  if snd sommet = hautGrille then
    raise ColonnePleine
  else 
    let newJeton = {pos = (col, snd(sommet)+1); coul = c;} in
    (insertDecroisLex newJeton l);;

let rec est_couleur c p g = match g with
    [] -> (false,(fst p),(snd p))
  | e::g' ->
    if ((fst e.pos) = (fst p)) && ((snd e.pos) = (snd p)) && (e.coul = c) then 
      (true,(fst p),(snd p)) 
    else
      if (e.pos = p) && (e.coul <> c) then
	(false,(fst p),(snd p))
      else 
	(est_couleur c p g');;
  
let first c = match c with
    (x,_,_) -> x;;
    
let second c = match c with
    (_,x,_) -> x;;

let third c = match c with
    (_,_,x) -> x;;

let est_gagnant j g = 
  let max4 a b c d =
    if (a >= b) && (a >= c) && (a >= d) then
      a
    else 
      if (b >= a) && (b >= c) && (b >= d) then
	b
      else 
	if (c >= a) && (c >= b) && (c >= d) then
	  c 
	else 
	  d
  in
  let rec est_gagnant_aux pos co g dx dy c m =
    if c = 4 then c
    else 
      if ((snd pos) > 6) || ((fst pos) > 10) then
	m
      else 
	if (first (est_couleur co pos g)) then
	  if (c+1) > c then
	    est_gagnant_aux ((fst pos)+dx,(snd pos)+dy) co g dx dy (c+1) (c+1)
	  else 
	    est_gagnant_aux ((fst pos)+dx,(snd pos)+dy) co g dx dy (c+1) m
	else 
	  est_gagnant_aux ((fst pos)+dx,(snd pos)+dy) co g dx dy 0 m
  in 
  
   max4 (est_gagnant_aux (((fst j.pos)-3),((snd j.pos))) j.coul g 1 0 0 0)
        (est_gagnant_aux ((fst j.pos),((snd j.pos)-3)) j.coul g 0 1 0 0)
        (est_gagnant_aux (((fst j.pos)-3),((snd j.pos)-3)) j.coul g 1 1 0 0)
        (est_gagnant_aux (((fst j.pos)+3),((snd j.pos)-3)) j.coul g (-1) 1 0 0);;


let maxAlignCo c g =
  let rec aux co gr m = 
    match gr with
	[] -> m
      | j::gr' ->
	if j.coul = co then 
	  begin
	    aux co gr' (max m (est_gagnant j g))
	  end
	else 
	  aux co gr' m
  in
  
  aux c g 0;;

let rec minM l = match l with
    [Vide] -> 4
  | Vide::l' -> minM l'
  | [Feuille(a)] -> a
  | [Cons (_,i)] -> i
  | Feuille(a)::l' -> min a (minM l')
  | (Cons (_,i))::l' -> min  i (minM l');;

let rec maxM l = match l with
    [Vide] -> 0
  | Vide::l' -> maxM l'
  | [Feuille(a)] -> a
  | [Cons (_,i)] -> i
  | Feuille(a)::l' -> max a (maxM l')
  | (Cons (_,i))::l' -> max i (maxM l');;

let consArbr c lv gr =
  let rec consArb co lvl g = match lvl with
      0 -> Feuille(maxAlignCo c g)
    | _ -> Cons (   (try [consArb (succCoul co) (lvl-1) (ajoutJeton 1 g co)] with ColonnePleine -> [Vide]) @
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 2 g co)] with ColonnePleine -> [Vide]) @ 
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 3 g co)] with ColonnePleine -> [Vide]) @
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 4 g co)] with ColonnePleine -> [Vide]) @
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 5 g co)] with ColonnePleine -> [Vide]) @
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 6 g co)] with ColonnePleine -> [Vide]) @
		    (try [consArb (succCoul co) (lvl-1) (ajoutJeton 7 g co)] with ColonnePleine -> [Vide]) , 0)
  in
  consArb c lv gr;;

let rec appFListe f l a= 
  match l with 
      [] -> []
    | e::l' -> (f e a)::(appFListe f l' a);;

let fillArb a =
  let rec aux a t = 
    match a with
	Vide -> Vide
      | Feuille(a) -> Feuille(a)
      | Cons(l,_) when t = 1 -> let lapp = appFListe aux l 0 in Cons(lapp, maxM (lapp))
      | Cons(l,_)  -> let lapp = appFListe aux l 1 in Cons(lapp, minM (lapp))
  in
  
  aux a 1;;

(*let jeuIA a =
  let rec aux l c m max=
    match l with
	[] -> (m, max)
      | Vide::l' -> aux l' (c+1) m max
      | Feuille(r)::l' -> 
	if r > max then
	  aux l' (c+1) c r
	else 
	  aux l' (c+1) m max
      | Cons(_,i)::l' -> 
	if i > max then
	  aux l' (c+1) c i
	else 
	  aux l' (c+1) m max
  in
  match a with
      Cons(l,n) -> aux l 1 0 (-1);;*)

let jeuIA a =
  let rec aux i l c =
    match l with
	[] -> c
      | Vide::l' -> (aux i l' (c+1))
      | Feuille(r)::l' -> if r = i then c else (aux i l' (c+1))
      | Cons(_,r)::l' -> if r = i then c else (aux i l' (c+1))
  in 
  match a with
      Cons(l, i) -> ((aux i l 1), i);;

let jeuIAplus j g niv=
  let jeuJaune = jeuIA (fillArb (consArbr (succCoul j) 1 g))
  in
  let jeuRouge = jeuIA (fillArb (consArbr j 1 g))
  in
  if (snd jeuRouge) = 4 then
      fst jeuRouge
  else
    if snd jeuJaune >= 3 then
      fst jeuJaune 
    else
      let jeuRouge2 = fst(jeuIA (fillArb (consArbr j niv g)))
      in
      if snd(jeuIA (fillArb (consArbr (succCoul j) 1 (ajoutJeton jeuRouge2 g j)))) = 4 then
	fst jeuJaune
      else
	jeuRouge2;;
	

let rec pause n =
  if n > 0 then
    pause (n-1)
  else 
    ();;
