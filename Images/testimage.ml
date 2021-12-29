#use "topfind";;
#load "unix.cma";;
#require "graphics";;
#use "../Code/Modules/castlerain.ml";;

Init.init ();;

Graphics.open_graph " 700x500";;

module Fleche =
struct
	let left = Char.chr 240 and right = Char.chr 241 and up = Char.chr 242 and down = Char.chr 243
end ;;

module Player =
struct
	let paths = [|"./Sprite/Alfred_w_0.img"; "./Sprite/Alfred_w_1.img"; "./Sprite/Alfred_w_2.img"; "./Sprite/Alfred_e_0.img"; "./Sprite/Alfred_e_1.img"; "./Sprite/Alfred_e_2.img"; "./Sprite/Alfred_n_0.img"; "./Sprite/Alfred_n_1.img"; "./Sprite/Alfred_n_2.img"; "./Sprite/Alfred_s_0.img"; "./Sprite/Alfred_s_1.img"; "./Sprite/Alfred_s_2.img"|]

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

	let read_paths paths =
	let taille = Array.length paths in
	let tab = Array.make_matrix 4 4 (Graphics.make_image (Array.make_matrix 1 1 (-1))) in
	begin
		for i = 0 to 3 do
			for j = 0 to 2 do
				tab.(i).(j) <- Graphics.make_image (read paths.(i*3 + j));
				if j=1 then tab.(i).(3) <- tab.(i).(j);
			done
		done;
		tab;
	end

	let imgs = read_paths paths

	type player = {mutable x : int; mutable  y : int; mutable  anim : int; texture : string; mutable dir : int; name : string; points : int array; dpoints : int array}

	let namepady = 42 and namepadx = 20


	let load map = 0


	let move player dx dy = 
		begin
			Graphics.clear_graph ();
			player.x <- player.x + dx;
			player.y <- player.y + dy;
			Graphics.draw_image imgs.(player.dir).(player.anim) player.x player.y;
			player.anim <- (player.anim + 1) mod 4;
			Graphics.set_color Graphics.black;
			let nx, ny = Graphics.text_size player.name in
			Graphics.moveto (player.x - nx/2 + namepadx) (player.y + namepady) ;
			Graphics.draw_string player.name;
			Unix.sleepf 0.01;
		end
	and creer x y anim texture dir name = {x=x; y=y; dir=dir; anim=anim; texture=texture; name=name; points = Array.make 0 4; dpoints = Array.make 0 4}
	let deplacement com player =
		begin
			if com = Fleche.left then (player.dir <- 0; move player (-2) 0);
			if com = Fleche.right then (player.dir <- 1; move player 2 0);
			if com = Fleche.up then (player.dir <- 3; move player 0 2);
			if com = Fleche.down then (player.dir <- 3; move player 0 (-2));
		end
	let interaction com player = if com = '\r' then ()
	let manage com player =
		begin
			deplacement com player;
			interaction com player;
		end
end ;;


let j = Player.creer 20 20 0 "./Sprite/Alfred" 0 "Alfred" and com = ref 'a' in
try
	while true do
		while not (Graphics.key_pressed ()) do
			Unix.sleepf 0.01;
		done;
		com := (Graphics.wait_next_event [Graphics.Key_pressed]).key;
		Player.deplacement !com j;
	done
with _ -> ignore (Close.close ()); exit 0;;