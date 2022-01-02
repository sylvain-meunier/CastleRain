#use "topfind";;
#use "../Modules/castlerain.ml";;

#load "unix.cma";;
#require "graphics";;

Init.init ();;

Graphics.open_graph " 700x500";;

module Clear =
struct
	let full () = Graphics.clear_graph ()
	let clear () = Graphics.clear_graph ()
end ;;

module Fleche =
struct
	let left = Char.chr 240 and right = Char.chr 241 and up = Char.chr 242 and down = Char.chr 243
end ;;

module Player =
struct
	type player = {mutable x : int; mutable  y : int; mutable  anim : int; texture : string; mutable dir : int; name : string; points : int array; dpoints : int array}

	let namepady = 10


	let load map = ()


	let move player dx dy = 
		begin
			Clear.clear ();
			Graphics.set_color Graphics.yellow;
			player.x <- player.x + dx;
			player.y <- player.y + dy;
			Graphics.fill_circle player.x player.y 10;
			Graphics.set_color Graphics.black;
			let nx, ny = Graphics.text_size player.name in
			Graphics.moveto (player.x - nx/2) (player.y + namepady) ;
			Graphics.draw_string player.name;
		end
	and creer x y anim texture dir name = {x=x; y=y; dir=dir; anim=anim; texture=texture; name=name; points = Array.make 0 4; dpoints = Array.make 0 4}
	let deplacement com player =
		begin
			if com = Fleche.left then move player (-2) 0;
			if com = Fleche.right then move player 2 0;
			if com = Fleche.up then move player 0 2;
			if com = Fleche.down then move player 0 (-2);
		end
	let interaction com player = if com = '\r' then ()
	let manage com player =
		begin
			deplacement com player;
			interaction com player;
		end
end ;;


module Inventory =
struct
	let manage com player = ()
end

module Mode =
struct
	let mode = ref 0 and modefunc = ref Inventory.manage
	let set_default () =
		if !mode != 0 then
		begin
			mode := 0;
			modefunc := Player.manage;
			
		end
	let set_inventory () =
		if !mode != 1 then
		begin
			mode := 1;
			modefunc := Inventory.manage
			
		end 
	let get () = !mode
	let manage com player = (!modefunc) com player
end ;;

Mode.set_default ();;

let j1 = Player.creer 10 10 0 "default" 0 "Alfred" and com = ref 'a' in
try
	while true do
		while not (Graphics.key_pressed ()) do
			Unix.sleepf 0.01;
		done;
		com := (Graphics.wait_next_event [Graphics.Key_pressed]).key;
		if !com = 'e' then Mode.set_inventory ();
		if !com = 'q' then Mode.set_default ();
		Mode.manage !com j1;
	done
with _ -> ignore (Close.close ()); exit 0;;