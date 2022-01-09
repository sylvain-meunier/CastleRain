#use  "topfind" ;;
#load "unix.cma";;
#require "graphics";;

#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

module Player =
struct
	type player = {mutable x : int; mutable  y : int; mutable  anim : int; texture : string; mutable dir : int; name : string; points : int array; dpoints : int array}

	(* Padding d'affiche du nom *)
	let namepady = 47 and namepadx = 20

	(* Permet de charger les sprites (voir le module Sprite pour une version améliorée) *)
	let paths = [|"../../Images/Sprite/Alfred_w_0.img"; "../../Images/Sprite/Alfred_w_1.img"; "../../Images/Sprite/Alfred_w_2.img"; "../../Images/Sprite/Alfred_e_0.img"; "../../Images/Sprite/Alfred_e_1.img"; "../../Images/Sprite/Alfred_e_2.img"; "../../Images/Sprite/Alfred_n_0.img"; "../../Images/Sprite/Alfred_n_1.img"; "../../Images/Sprite/Alfred_n_2.img"; "../../Images/Sprite/Alfred_s_0.img"; "../../Images/Sprite/Alfred_s_1.img"; "../../Images/Sprite/Alfred_s_2.img"|]

	(* Renvoie un tableau contenant les animations de marche de Alfred *)
	let read_paths paths =
	let tab = Array.make_matrix 4 5 (Graphics.make_image (Array.make_matrix 1 1 (-1))) in
	begin
		for i = 0 to 3 do
			for j = 0 to 2 do
				tab.(i).(j) <- Graphics.make_image (Sprite.read paths.(i*3 + j));
				if j=1 then tab.(i).(3) <- tab.(i).(j);
			done
		done;
		tab.(3).(4) <- Graphics.make_image (Sprite.read "../../Images/Sprite/Alfred_s_4.img");
		tab;
	end

	(* Contient ce tableau *)
	let imgs = read_paths paths

	(* Contient une image blanche *)
  let white_img = Graphics.make_image (Array.make_matrix 60 38 Graphics.white)

	(* Place le joueur aux coordonnées indiquées *)
	let set_pos player x y =
		begin
			Graphics.draw_image white_img player.x player.y ;
			player.x <- x;
			player.y <- y;
			Graphics.draw_image imgs.(player.dir).(player.anim) player.x player.y;
			player.anim <- (player.anim + 1) mod 4;
			Graphics.set_color Graphics.black;
			let nx, ny = Graphics.text_size player.name in
			Graphics.moveto (player.x - nx/2 + namepadx) (player.y + namepady) ;
			Graphics.draw_string player.name;
			Unix.sleepf 0.001;
		end

	let display_player player =
		begin
			Graphics.draw_image imgs.(player.dir).(player.anim) player.x player.y;
			Graphics.set_color Graphics.black;
			let nx, ny = Graphics.text_size player.name in
			Graphics.moveto (player.x - nx/2 + namepadx) (player.y + namepady) ;
			Graphics.draw_string player.name;
		end

	(* Déplace le joueur *)
	let move player dx dy = set_pos player (player.x + dx) (player.y + dy)

	(* Crée un joueur *)
	and creer x y anim texture dir name = {x=x; y=y; dir=dir; anim=anim; texture=texture; name=name; points = Array.make 0 4; dpoints = Array.make 0 4}
	
	(* Permet de gérer les déplacements du joueur *)
	let deplacement com player =
		begin
			if com = Fleche.left then (if player.dir <> 0 then (player.dir <- 0; player.anim <- 1; move player 0 0) else if player.x  >= 5 then move player (-8) 0);
			if com = Fleche.right then (if player.dir <> 1 then (player.dir <- 1; player.anim <- 1; move player 0 0) else if player.x + 40 <= Graphics.size_x () then move player 8 0);
			if com = Fleche.up then (if player.dir <> 2 then (player.dir <- 2; player.anim <- 1; move player 0 0) else if player.y + 50 <= Graphics.size_y () then move player 0 8);
			if com = Fleche.down then (if player.dir <> 3 then (player.dir <- 3; player.anim <- 1; move player 0 0) else if player.y  >= 5 then move player 0 (-8));
		end
	
	(* Devrait gérer les interactions du joueur avec son environnement, son inventaire, ses items... *)
	let interaction com player =
		begin
			if com = '\r' then ();
			if com = 'e' then ();
			if com = 'q' then ();
			if com = ' ' then let cond = player.anim <> 4 in
				begin
					if cond then (player.anim <- 4) else (player.anim <- 1);
					move player 0 0;
					if cond then player.anim <- 4;
				end
		end
	
	(* Réunit les fonctions précédentes *)
	let manage com player =
		begin
			deplacement com player;
			interaction com player;
		end

	(* Renvoie la distance entre 2 points *)
	let distance_2 a b c d = let dx = c - a and dy = b - d in int_of_float (float_of_int (dx*dx + dy*dy))

	(* Rayon des cercles *)
	let radius = 20

	type rond = {x:int; y:int; color:int; points:int}

	(* Crée un cercle suffisament éloigné du joueur *)
	let rec create_rond (player:player) = match 20 + Random.int (Graphics.size_x () - 40), 20 + Random.int (Graphics.size_y () - 40) with
		| x, y when distance_2 x y player.x player.y >= 2500 ->
			let point = if Random.int 5 = 1 then 2 else 1 in
			let color = if point = 1 then Graphics.cyan else Graphics.blue in
			begin
				Graphics.set_color color ;
				Graphics.fill_circle x y radius ;
				{x=x; y=y; color=color; points = point} ;
			end
		| _ -> create_rond player

	(* Renvoie si un rond est ou non atteint par le joueur *)
	let rond_func rond (player:player) = distance_2 player.x player.y (rond.x-radius) (rond.y-radius) <= 1400

	type foe = {mutable x:float; mutable y:float; mutable dx:float; mutable dy:float; mutable lifetime : int; caracteristique : float*float*float}

	(* Crée un ennemi suffisament éloigné du joueur *)
	let rec create_foe (player:player) = match 20 + Random.int (Graphics.size_x () - 40), 20 + Random.int (Graphics.size_y () - 40) with
		| x, y when distance_2 x y player.x player.y >= 25000 -> {x=float_of_int x; y=float_of_int y; dx=0.; dy=0.; lifetime = 3000 + (Random.int 2000); caracteristique = if Random.int 2 = 0 then (0.0008, 1., 10.) else if Random.int 3 <> 0 then (0.0001, 1., 10.) else (0.05, 0.8, 0.6)}
		| _ -> create_foe player

	(* Met à jour les liste des ronds en déterminant si le joueur en touche un. Peut alors faire apparaître un ennemi ainsi qu'un autre rond et met à jour le score du joueur *)
	let rec manage_ronds rond_list mem points (player:player) foes = match rond_list with
		| [] -> mem
		| rond::q -> if rond_func rond player then (points := !points + rond.points; if Random.int 3 >= 1 then foes:=(create_foe player)::(!foes); Graphics.set_color Graphics.white; Graphics.fill_circle rond.x rond.y radius; if Random.int 15 = 1 then ((create_rond player)::(create_rond player)::mem@q) else ((create_rond player)::mem@q)) else manage_ronds q (rond::mem) points player foes

	(* Gère les ennemis et renvoie 2 booléens indiquant si : l'ennemi est mort, l'ennemi touche le joueur *)
	let foe_func foe (player:player) =
		if foe.lifetime <= 0 then (Graphics.set_color Graphics.white; Graphics.fill_rect (int_of_float foe.x) (int_of_float foe.y) 20 20; (true, false))
		else let dist = distance_2 (int_of_float foe.x) (int_of_float foe.y) player.x player.y in
			let pas, inertie, vmax = foe.caracteristique in
			begin
				foe.lifetime <- foe.lifetime - 1 ;
				Graphics.set_color Graphics.white ;
				Graphics.fill_rect (int_of_float foe.x) (int_of_float foe.y) 20 20 ;
				foe.x <- foe.x +. foe.dx ;
				foe.y <- foe.y +. foe.dy ;
				let ndx = foe.dx*.inertie +. pas *. ((float_of_int player.x) -. foe.x) /. (sqrt (float_of_int dist)) and ndy = foe.dy*.inertie +. pas *. ((float_of_int player.y) -. foe.y) /. (sqrt (float_of_int dist)) in
				if ndx*.ndx +. ndy*.ndy < vmax then
					begin
						foe.dx <- ndx ;
						foe.dy <- ndy ;
					end ;
				Graphics.set_color Graphics.red ;
				Graphics.fill_rect (int_of_float foe.x) (int_of_float foe.y) 20 20 ;
				if dist <= 600 then true, true
				else false, false
			end
			
	(* Met à jour les liste des ennemis en les déplaçant et supprime ceux dont le temps de vie est nul *)
	let rec manage_foes foe_list mem (player:player) = match foe_list with
		| [] -> (mem, true)
		| foe::q -> let dead, touch = foe_func foe player in (if touch then ([], false) else manage_foes q (if dead then mem else (foe::mem)) player)

	let func () =
		begin
			let j = creer 20 20 0 "../../Images/Sprite/Alfred" 0 "Alfred" and com = ref 'a' and foes = ref [] and points = ref 0 and maxpoints = ref 0 in
			let ronds = ref [create_rond j; create_rond j; create_rond j] in
				try
					(* Initialisation *)
					Random.self_init () ;
          Graphics.auto_synchronize false;
					let f = open_in "./game.score" in let score = Scanf.sscanf (input_line f) "%d" (fun x -> x) in maxpoints := score ;
					(* Permet d'afficher le joueur à l'écran *)
          manage Fleche.down j;
					
					Graphics.moveto 450 450 ;
					Graphics.set_color Graphics.black ;
					Graphics.draw_string ("Meilleur score : "^(string_of_int !maxpoints)) ;
					Graphics.moveto 50 450 ;
					Graphics.set_color Graphics.black ;
					Graphics.draw_string ("Points : "^(string_of_int !points)) ;
					
					(* Boucle principale *)
					while true do
						(* Attends que le joueur appuie sur une touche *)
						while not (Graphics.key_pressed ()) do
							(* Met à jour la liste des ennemis, si le joueur est touché, reset le jeu et met à jour le meilleur score *)
							let foe, alive = manage_foes !foes [] j in (if alive then foes := foe else (foes := []; maxpoints := max !points !maxpoints; let f = open_out "./game.score" in (output_string f (string_of_int !maxpoints); flush f; close_out f); points := 0; Graphics.clear_graph (); ronds:=[create_rond j; create_rond j; create_rond j]; set_pos j 20 20)) ;
							
							(* Met à jour l'affichage du score *)
							Graphics.moveto 50 450 ;
							Graphics.set_color Graphics.white ;
							let dx, dy = Graphics.text_size "Points : 1000" in Graphics.fill_rect 50 450 dx dy ;
							Graphics.set_color Graphics.black ;
							Graphics.draw_string ("Points : "^(string_of_int !points)) ;
						
							let ev = Graphics.wait_next_event [Graphics.Poll] in let c = ev.key in
							if c = Fleche.down || c = Fleche.up || c = Fleche.left || c = Fleche.right then Printf.printf "%c\n" c ;

							Graphics.moveto 450 450 ;
							Graphics.set_color Graphics.white ;
							let dx, dy = Graphics.text_size "Meilleur score : 1000" in Graphics.fill_rect 450 450 dx dy ;
							Graphics.set_color Graphics.black ;
							Graphics.draw_string ("Meilleur score : "^(string_of_int !maxpoints)) ;
							Graphics.synchronize () ;
							Unix.sleepf 0.001;
						done; 
				
						(* Met à jour les liste des ronds *)
						ronds := manage_ronds !ronds [] points j foes ;

						(* Déplace le joueur *)
						com := Graphics.read_key ();
						manage !com j;

					done
				with _ -> ()
		end
end ;;