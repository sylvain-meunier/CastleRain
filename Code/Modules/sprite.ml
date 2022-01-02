#use "topfind" ;;
#require "graphics" ;;

#use "../Modules/castlerain.ml" ;;

module Sprite =
struct
	type token = {mutable anim:int; texture:string; mutable priorite:int; mutable anim_tot : int; mutable posx:int; mutable posy:int; dx:int; dy:int; mutable vx:int; mutable vy:int; imgs: Graphics.image array; mutable visible:bool; mutable fire : unit -> unit}

	let tokens = ref []

	let order liste = List.stable_sort (fun a b -> a.priorite - b.priorite) liste

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

	let load texture anim_tot =
		let imgs = Array.make anim_tot (Graphics.make_image (Array.make_matrix 1 1 (-1))) and dx = ref 0 and dy = ref 0 in
		for i = 0 to (anim_tot-1) do
			let tab = read (texture ^ "_" ^ (string_of_int i) ^ ".img") in
			imgs.(i) <- Graphics.make_image (tab) ;
			dy := max !dy (Array.length tab) ;
			dx := max !dx (Array.length tab.(0)) ;
		done; imgs, !dx, !dy
		
	let make posx posy texture anim_tot priorite visible =
		let imgs, dx, dy = load texture anim_tot in
		{texture = texture; priorite = priorite; anim = 0; anim_tot = anim_tot; posx = posx; posy = posy; dx = dx; dy = dy; vx = posx; vy = posy; imgs = imgs; visible=visible; fire=(fun () -> ())}

	let create posx posy texture anim_tot priorite visible =
		let tok = make posx posy texture anim_tot priorite visible in
		begin
			tokens := order (tok::!tokens);
			tok;
		end

	let rec find_on x y dx dy tokens found = match tokens with
	| [] -> found
	(* Test de la collision entre 2 rectangles *)
	| a::b -> if a.visible && (if x <= a.posx then x+dx >= a.posx && x <= a.posx + a.dx else a.posx + a.dx >= x && a.posx <= x+dx) && (if y <= a.posy then y+dy >= a.posy && y <= a.posy + a.dy else a.posy + a.dy >= y && a.posy <= y+dy) then find_on x y dx dy b (a::found) else find_on x y dx dy b found

	let check_on x y dx dy tokens = order (find_on x y dx dy tokens [])

	let rec find_higher_prio x y tokens mem = match tokens with
	| [] -> mem ;
	| a::q -> if a.visible && a.priorite > mem.priorite && x >= a.posx && y >= a.posy && a.posy+a.dy >= y && a.posx+a.dx >= x then find_higher_prio x y q a else find_higher_prio x y q mem

	let tokens_at x y = if x >=0 && y >= 0 then find_higher_prio x y !tokens (List.hd !tokens) else (List.hd !tokens)

	let display_on token = Graphics.draw_image token.imgs.(token.anim) token.posx token.posy

	let center_x token = let x =  Graphics.size_x () in (token.posx <- (x-token.dx)/2; token.vx <- token.posx)
	let center_y token = let y =  Graphics.size_y () in (token.posy <- (y-token.dy)/2; token.vy <- token.posy)

	let set_function token func = token.fire <- func

	let rec display_toks token_liste = match token_liste with
	| [] -> () ;
	| token::queue ->
			begin
				if token.visible then display_on token ;
				display_toks queue ;
			end
	
	let rec display_toks_higher prio token_liste = match token_liste with
	| [] -> ()
	| token::queue ->
		begin
			if token.visible && token.priorite >= prio then display_on token ;
			display_toks_higher prio queue ;
		end
	
	let animation token cond = if cond then token.anim <- (token.anim + 1) mod token.anim_tot else ()

	let set_animation token ind = token.anim <- ind

	let move token x y animate =
		begin
			token.vx <- x;
			token.vy <- y;
			animation token animate;
		end
	
	let visibility token cond = token.visible <- cond

	let rmove token dx dy animate = move token (token.posx + dx) (token.posy + dy) animate
	
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

	let update_one token =
		begin
			token.posx <- token.vx ;
			token.posy <- token.vy ;
		end

	let rec update_toks l = match l with
	| [] -> ()
	| a::b -> (update_one a; update_toks b)
	
	let update_all () = update_toks !tokens

	let show_all () =	display_toks !tokens

	let rec remove_token token liste mem = match liste with
	| [] -> mem
	| t::q -> if t = token then (remove_token token q mem) else remove_token token q (t::mem)

	let delete token =
		(tokens := remove_token token !tokens []; move token 0 0 false)

end ;;

module Player =
struct
	let paths = [|"../../Images/Sprite/Alfred_w_0.img"; "../../Images/Sprite/Alfred_w_1.img"; "../../Images/Sprite/Alfred_w_2.img"; "../../Images/Sprite/Alfred_e_0.img"; "../../Images/Sprite/Alfred_e_1.img"; "../../Images/Sprite/Alfred_e_2.img"; "../../Images/Sprite/Alfred_n_0.img"; "../../Images/Sprite/Alfred_n_1.img"; "../../Images/Sprite/Alfred_n_2.img"; "../../Images/Sprite/Alfred_s_0.img"; "../../Images/Sprite/Alfred_s_1.img"; "../../Images/Sprite/Alfred_s_2.img"|]

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

	let imgs = read_paths paths

	type player = {mutable x : int; mutable  y : int; mutable  anim : int; texture : string; mutable dir : int; name : string; points : int array; dpoints : int array}

	let namepady = 47 and namepadx = 20

	let set_pos player x y =
		begin
			Graphics.clear_graph ();
			player.x <- x;
			player.y <- y;
			Graphics.draw_image imgs.(player.dir).(player.anim) player.x player.y;
			player.anim <- (player.anim + 1) mod 4;
			Graphics.set_color Graphics.black;
			let nx, ny = Graphics.text_size player.name in
			Graphics.moveto (player.x - nx/2 + namepadx) (player.y + namepady) ;
			Graphics.draw_string player.name;
			Unix.sleepf 0.01;
		end

	let move player dx dy = set_pos player (player.x + dx) (player.y + dy)

	and creer x y anim texture dir name = {x=x; y=y; dir=dir; anim=anim; texture=texture; name=name; points = Array.make 0 4; dpoints = Array.make 0 4}
	
	let deplacement com player =
		begin
			if com = Fleche.left then (if player.dir <> 0 then (player.dir <- 0; player.anim <- 1; move player 0 0) else move player (-4) 0);
			if com = Fleche.right then (if player.dir <> 1 then (player.dir <- 1; player.anim <- 1; move player 0 0) else move player 4 0);
			if com = Fleche.up then (if player.dir <> 2 then (player.dir <- 2; player.anim <- 1; move player 0 0) else move player 0 4);
			if com = Fleche.down then (if player.dir <> 3 then (player.dir <- 3; player.anim <- 1; move player 0 0) else move player 0 (-4));
		end
	
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
	
		let manage com player =
		begin
			deplacement com player;
			interaction com player;
		end
end ;;