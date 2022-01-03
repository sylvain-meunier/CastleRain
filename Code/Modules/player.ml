#use  "topfind" ;;
#load "unix.cma";;
#require "graphics";;

#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

module Player =
struct
	type player = {mutable x : int; mutable  y : int; mutable  anim : int; texture : string; mutable dir : int; name : string; points : int array; dpoints : int array}

	let namepady = 47 and namepadx = 20

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

  let white_img = Graphics.make_image (Array.make_matrix 100 100 Graphics.white)

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
			Unix.sleepf 0.01;
		end

	let move player dx dy = set_pos player (player.x + dx) (player.y + dy)

	and creer x y anim texture dir name = {x=x; y=y; dir=dir; anim=anim; texture=texture; name=name; points = Array.make 0 4; dpoints = Array.make 0 4}
	
	let deplacement com player =
		begin
			if com = Fleche.left then (if player.dir <> 0 then (player.dir <- 0; player.anim <- 1; move player 0 0) else if player.x  >= 5 then move player (-4) 0);
			if com = Fleche.right then (if player.dir <> 1 then (player.dir <- 1; player.anim <- 1; move player 0 0) else if player.x + 40 <= Graphics.size_x () then move player 4 0);
			if com = Fleche.up then (if player.dir <> 2 then (player.dir <- 2; player.anim <- 1; move player 0 0) else if player.y + 50 <= Graphics.size_y () then move player 0 4);
			if com = Fleche.down then (if player.dir <> 3 then (player.dir <- 3; player.anim <- 1; move player 0 0) else if player.y  >= 5 then move player 0 (-4));
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

		let func () =
			begin
				let j = creer 20 20 0 "../../Images/Sprite/Alfred" 0 "Alfred" and com = ref 'a' in
				try
          Graphics.auto_synchronize false;
          manage Fleche.down j;
					while true do
						while not (Graphics.key_pressed ()) do
							Graphics.synchronize ();
							Unix.sleepf 0.001;
						done;
						com := (Graphics.wait_next_event [Graphics.Key_pressed]).key;
						manage !com j;
					done
				with _ -> ()
			end
end ;;