#use "topfind";;
#load "unix.cma";;
#require "graphics";;
#use "../Code/Modules/castlerain.ml" ;;
#use "../Code/Modules/sprite.ml";;

Init.init ();;

Graphics.open_graph " 700x500";;
Graphics.auto_synchronize false;;


let j = Player.creer 20 20 0 "./Sprite/Alfred" 0 "Alfred" and com = ref 'a' in
try
	while true do
		while not (Graphics.key_pressed ()) do
			Graphics.synchronize ();
			Unix.sleepf 0.001;
		done;
		com := (Graphics.wait_next_event [Graphics.Key_pressed]).key;
		Player.manage !com j;
	done
with _ -> ignore (Close.close ()); exit 0;;