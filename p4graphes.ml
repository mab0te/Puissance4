#load "graphics.cma";;
open Graphics;;

let ouvrir_fenetre  () = 
  open_graph " 700x600";
;;

let fermer_fenetre () = 
  close_graph ();
;;

let dessin_grille l = 
  let x = size_x ()
  in 
  let y = size_y ()
  in
  let r = (min (x/15) (y/15))
  in
  let rec vide a b = match a,b with
     1,1 -> fill_circle (x/14) (y/7) (r-1)
    |n,1 -> 
      begin
	fill_circle (n*(x/7)-x/14) (y/7) (r-1);
	vide (n-1) 6;
      end
    |n,m ->
      begin
	fill_circle (n*(x/7)-x/14) (m*(y/7)) (r-1);
	vide n (m-1);
      end
  in
  let rec remplir l = match l with
      [] -> set_color (blue)
    | e::l' ->
      let n = (fst e.pos)
      and m = (snd e.pos)
      in 
      if e.coul = Jaune then
	begin
	  set_color (yellow);
	  fill_circle (n*(x/7)-x/14) (m*(y/7)) (r);
	  remplir l';
	end 
      else 
	begin
	  set_color(red);
	  fill_circle (n*(x/7)-x/14) (m*y/7) (r);
	  remplir l';
	end
  in 
  begin 
    clear_graph ();
    set_color (blue);
    fill_rect 0 0 x y;
    set_color (white);
    vide 7 6;
    remplir l;
  end;
;;

let rec get_colonne = function () ->
  begin
    wait_next_event [Button_down];
    let x = (fst (mouse_pos() ))
    in 
    if (0<x) && (x <= (size_x()) ) then
     int_of_float (((float_of_int x) /. (float_of_int (size_x ()) ) *. 7.)) + 1
    else 
      get_colonne ();
  end;
;;

let coul_to_string = function
    Rouge -> "Rouge"
  | Jaune -> "Jaune";;

let afficheGagnant = function coul -> function nbeCoup -> function g ->
  begin
    dessin_grille g;
    moveto ((5*size_x())/100) ((95*size_y())/100);
    set_color black;
    if nbeCoup = 42 then
      draw_string("Mince alors, match nul !")
    else
    begin
      draw_string("Bravo joueur ");
      draw_string(coul_to_string coul);
      draw_string(". Tu es le vainqueur !");
    end
  end;
;;
    
let afficheJoueur co = 
  begin
     moveto ((5*size_x())/100) ((95*size_y())/100);
       if co = Rouge then
	 begin
	   set_color red;
	   draw_string "A vous joueur rouge !"
	 end
       else 
	 begin
	   set_color yellow;
	   draw_string "A vous joueur jaune !"
	 end
  end;
;;

let rec affiche_menu num = 
  let l  = 4 * ((size_x()) / 10)
  in
  let h  = (size_y())/(size_x())*l
  in
  let x  = (size_x()/2)-(l/2)
  in
  let y1 = ((size_y())/10)*9 - h
  in
  let y2 = ((size_y())/10)
  in 
  begin
    set_line_width 5;
    set_color blue;
    clear_graph ();
    fill_rect 0 0 (size_x()) (size_y());
    set_color black;
    draw_rect 0 0 (size_x()) (size_y());
    set_color red;
    fill_rect x y1 l h;
    set_color black;
    draw_rect x y1 l h;
    set_color yellow;
    fill_rect x y2 l h;
    set_color black;
    draw_rect x y2 l h;
    if num = 1 then
      begin
	moveto (x+3*l/8) (y1+(h/2));
	draw_string "1 joueur";
	moveto (x+3*l/8) (y2+(h/2));
	draw_string "2 joueurs";
      end
    else
      if num = 2 then
	begin
	  moveto  (40*(size_x())/100) (95*(size_y())/100);
	  draw_string "Choisissez votre couleur :";
	end
      else
	begin
	  moveto (x+3*l/8) (y1+(h/2));
	  draw_string "Rejouer";
	  moveto (x+3*l/8) (y2+(h/2));
	  draw_string "Quitter";
	end;
    wait_next_event [Button_down];

    let mx = fst(mouse_pos())
    in
    let my = snd(mouse_pos())
    in
    if num = 1 then
      if  (x < mx) && (mx<(x+l)) && (y1<my) && (my<(y1+h)) then
	false
      else 
	if (x < mx) && (mx<(x+l)) && (y2<my) && (my<(y2+h)) then
	  true
	else 
	  affiche_menu 1
    else 
      if num = 2 then
	if  (x < mx) && (mx<(x+l)) && (y1<my) && (my<(y1+h)) then
	  true (*rouge*)
	else 
	  if (x < mx) && (mx<(x+l)) && (y2<my) && (my<(y2+h)) then
	    false(*jaune*)
	  else 
	    affiche_menu 2
      else 
	if  (x < mx) && (mx<(x+l)) && (y1<my) && (my<(y1+h)) then
	  true 
	else 
	  if (x < mx) && (mx<(x+l)) && (y2<my) && (my<(y2+h)) then
	    false
	  else 
	    affiche_menu 3
  end;
;;
