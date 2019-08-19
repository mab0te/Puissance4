#use "p4engine.ml";;
#use "p4graphes.ml";;

let sonVictoire () =
  begin
     sound 800 200;
     pause 5000000;
     sound 500 200;
     pause 5000000;
     sound 800 200;
     pause 5000000;
     sound 700 200;
     pause 10000000;
     sound 700 200;
     pause 5000000;
     sound 500 400;
     pause 5000000;
     sound 500 400;
     pause 5000000;
     sound 700 700;
  end;
;;

let rec main niv =
  let rec mainAux joueur g p c nbeCoup jcj niv coJoueur= 
    begin
      dessin_grille g;
      afficheJoueur joueur;
      try 
	if  ((est_gagnant {pos = p; coul = c} g)) <> 4 && (nbeCoup <> 42) then
	  if jcj || (joueur = coJoueur) then
	    let col = get_colonne()
	    in
	    let sommetCol = (sommetColonne col g)
	    in 
	    mainAux (succCoul joueur) (ajoutJeton col g joueur) ((fst sommetCol),(snd sommetCol)+1) joueur (nbeCoup+1) jcj niv coJoueur
	  else
	    let col = jeuIAplus joueur g niv
	    in
	    let sommetCol = (sommetColonne col g)
	    in
	    mainAux (succCoul joueur) (ajoutJeton col g joueur) ((fst sommetCol),(snd sommetCol)+1) joueur (nbeCoup+1) jcj niv coJoueur
	else
	  begin
	    afficheGagnant (succCoul joueur) nbeCoup g;
	    sonVictoire ();
	  end
      with
	  ColonnePleine ->
	    begin
	      sound 100 300;
	      mainAux joueur g p (succCoul joueur) nbeCoup jcj niv coJoueur
	    end
    end
	
  in 
  try
    ouvrir_fenetre ();
    set_window_title ("puissance 4");
    let pvp = affiche_menu 1
    in
    if not pvp then
      let coJoueur = if (affiche_menu 2) then Rouge else Jaune
      in
      begin
	mainAux Rouge grille (42,42) Rouge 0 pvp niv coJoueur;
	wait_next_event [Button_down;Key_pressed];
	if affiche_menu 3 then
	  main 3
	else 
	  fermer_fenetre () 
      end
    else 
      let coJoueur = Rouge
      in
      begin
	mainAux Rouge grille (42,42) Rouge 0 pvp niv coJoueur;
	wait_next_event [Button_down;Key_pressed];
	if affiche_menu 3 then
	  main 3
	else 
	  fermer_fenetre () 
      end
  with
      Graphics.Graphic_failure("fatal I/O error") -> fermer_fenetre()
 ;;
