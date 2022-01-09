(* Ce module permet d'afficher des sprites à l'écran.
Il pourrait cependant être optimisé au niveau de l'affichage avec davantage d'informations sur la façon dont Graphics le gère *)

#use  "topfind" ;;
#require "graphics";;

module Sprite =
struct
	type token = {mutable anim:int; texture:string; mutable priorite:int; mutable anim_tot : int; mutable posx:int; mutable posy:int; dx:int; dy:int; mutable vx:int; mutable vy:int; imgs: Graphics.image array; mutable visible:bool; mutable fire : unit -> unit}

	(* Liste globale qui contient tous les tokens *)
	let tokens = ref []

	(* Range une liste de tokens par ordre de priorité croissant *)
	let order liste = List.stable_sort (fun a b -> a.priorite - b.priorite) liste

	(* Décode un fichier .img *)
	let read path = let file = open_in path and tab = ref (Array.make_matrix 0 0 0) in
	  	try
		  	let ligne = ref (input_line file) and i = ref 0 and j = ref 0 in
			  let x, y = Scanf.sscanf (!ligne) "%d %d" (fun x y -> x, y) in
				  begin
					  tab := Array.make_matrix x y 0;
					  while true do
						  ligne := input_line file;
						  !tab.(!i).(!j) <- (int_of_string (List.hd (String.split_on_char '\r' !ligne)));
  						j := !j + 1;
  						if !j >= y then
  							begin
  								j := 0;
  								i := !i + 1;
  							end
  					done;
  					!tab
  				end
  		with End_of_file -> !tab

	(* Convertit la sortie de la fonction précédente en un tableau contenant des images (au sens de Graphics) de toutes les frames d'animation du token *)
	let load texture anim_tot =
		let imgs = Array.make anim_tot (Graphics.make_image (Array.make_matrix 1 1 (-1))) and dx = ref 0 and dy = ref 0 in
		for i = 0 to (anim_tot-1) do
			let tab = read (texture ^ "_" ^ (string_of_int i) ^ ".img") in
			imgs.(i) <- Graphics.make_image (tab) ;
			dy := max !dy (Array.length tab) ;
			dx := max !dx (Array.length tab.(0)) ;
		done; imgs, !dx, !dy
		
	(* Crée un token *)
	let make posx posy texture anim_tot priorite visible =
		let imgs, dx, dy = load texture anim_tot in
		{texture = texture; priorite = priorite; anim = 0; anim_tot = anim_tot; posx = posx; posy = posy; dx = dx; dy = dy; vx = posx; vy = posy; imgs = imgs; visible=visible; fire=(fun () -> ())}

		(* Crée un token et l'ajoute à la liste gloable *)
		let create posx posy texture anim_tot priorite visible =
		let tok = make posx posy texture anim_tot priorite visible in
		begin
			tokens := order (tok::!tokens);
			tok;
		end

	(* Renvoie tous les tokens situés aux coordonnées indiquées *)
	let rec find_on x y dx dy tokens found = match tokens with
	| [] -> found
	(* Test de la collision entre 2 rectangles *)
	| a::b -> if a.visible && (if x <= a.posx then x+dx >= a.posx && x <= a.posx + a.dx else a.posx + a.dx >= x && a.posx <= x+dx) && (if y <= a.posy then y+dy >= a.posy && y <= a.posy + a.dy else a.posy + a.dy >= y && a.posy <= y+dy) then find_on x y dx dy b (a::found) else find_on x y dx dy b found

	(* Ordonne la sortie de la fonction précédente par ordre croissant de priorité *)
	let check_on x y dx dy tokens = order (find_on x y dx dy tokens [])

	(* Renvoie le token ayant la plus haute coordonnées aux coordonnées indiquées *)
	let rec find_higher_prio x y tokens mem = match tokens with
	| [] -> mem ;
	| a::q -> if a.visible && a.priorite > mem.priorite && x >= a.posx && y >= a.posy && a.posy+a.dy >= y && a.posx+a.dx >= x then find_higher_prio x y q a else find_higher_prio x y q mem

	(* Simplifie l'utilisation de la fonction précédente *)
	let tokens_at x y = if x >=0 && y >= 0 then find_higher_prio x y !tokens (List.hd !tokens) else (List.hd !tokens)

	(* Affiche un token *)
	let display_on token = Graphics.draw_image token.imgs.(token.anim) token.posx token.posy

	(* Centre un token *)
	let center_x token = let x =  Graphics.size_x () in (token.posx <- (x-token.dx)/2; token.vx <- token.posx)
	let center_y token = let y =  Graphics.size_y () in (token.posy <- (y-token.dy)/2; token.vy <- token.posy)

	(* Lie une fonction à un token *)
	let set_function token func = token.fire <- func

	(* Affiche tous les token dans la liste passée en argument *)
	let rec display_toks token_liste = match token_liste with
	| [] -> () ;
	| token::queue ->
			begin
				if token.visible then display_on token ;
				display_toks queue ;
			end
	
	(* N'affiche que les tokens dont la priorité est supérieure à celle indiquée *)
	let rec display_toks_higher prio token_liste = match token_liste with
	| [] -> ()
	| token::queue ->
		begin
			if token.visible && token.priorite >= prio then display_on token ;
			display_toks_higher prio queue ;
		end

	(* Permet d'animer un token *)
	let animation token cond = if cond then token.anim <- (token.anim + 1) mod token.anim_tot else ()

	(* Permet de se placer à un frame précise de l'animation *)
	let set_animation token ind = token.anim <- ind

	(* Met à jour les coordonnées d'un token, avec ou sans animation *)
	let move token x y animate =
		begin
			token.vx <- x;
			token.vy <- y;
			animation token animate;
		end
	
	(* Permet de définir si un token est visible ou non *)
	let visibility token cond = token.visible <- cond

	let rmove token dx dy animate = move token (token.posx + dx) (token.posy + dy) animate
	
	(* Affiche un token *)
	let show token update =
		let x = token.posx and y = token.posy in
		begin
			token.posx <- token.vx;
			token.posy <- token.vy;
			if update then
				begin
					if token.posx <> token.vx || token.posy <> token.vy then let liste = check_on x y token.dx token.dy !tokens in display_toks liste ;
					let liste = check_on token.posx token.posy token.dx token.dy !tokens in display_toks_higher token.priorite liste ;
				end
			else display_on token
	end

	(* Met à jour un token *)
	let update_one token =
		begin
			token.posx <- token.vx ;
			token.posy <- token.vy ;
		end

	(* Met à jour une liste de tokens *)
	let rec update_toks l = match l with
	| [] -> ()
	| a::b -> (update_one a; update_toks b)
	
	let update_all () = update_toks !tokens

	let show_all () =	display_toks !tokens

	(* Supprime un token de la liste globale *)
	let rec remove_token token liste mem = match liste with
	| [] -> mem
	| t::q -> if t = token then (remove_token token q mem) else remove_token token q (t::mem)

	(* Met en application la fonction précédente *)
	let delete token =
		(tokens := remove_token token !tokens []; move token 0 0 false)

end ;;