#use "topfind" ;;
#load "unix.cma" ;;
#require "graphics" ;;

module Init =
struct
	let screen_x, screen_y = let screen = Unix.open_process_in "xrandr" in Scanf.sscanf (input_line screen) "%s %s %s %s %s %s current %d x %d" (fun s1 s2 s3 s4 s5 s6 y z -> y, z)

	let launch_x = "600" and launch_y = "450"
	let game_x = "700" and game_y = "500"
	let chat_x = "300" and chat_y = game_y

	let launchername = "[CastleRain - Launcher]"
	and gamename = "[CastleRain - Game]"
	and chatname = "[CastleRain - Chat]"

	(* Crée une fenêtre centrée *)
	let create_win winx winy title truetitle filename =
		begin
			Graphics.open_graph (" " ^ winx ^ "x" ^ winy ^ "+0+0") ;
			Graphics.set_window_title title ;
			let window_id = (let screen = Unix.open_process_in ("xdotool search --name \"" ^ title ^ "\"") in Scanf.sscanf (input_line screen) "%s" (fun x -> x)) in
				begin
					ignore (Unix.system ("xdotool set_window --name \"" ^ truetitle ^ "\" " ^ window_id)) ;
					if filename <> "" then ignore (let oc = open_out filename in (Printf.fprintf oc "%s" window_id; close_out oc)) ;
					window_id ;
				end
			end
	
	(* Renvoie l'identifiant d'une fenêtre en fonction de son nom *)
	let get_window_id title = (let screen = Unix.open_process_in ("xdotool search --name \"" ^ launchername ^ "\"") in Scanf.sscanf (input_line screen) "%s" (fun x -> x))

	(* Permet de détecter l'appui sur les flèches avec Graphics (qui ignore normalement ces évènements) *)
	let allow_arrow_press () =
		begin
		ignore (Unix.system "xmodmap -pke > ./touchmem.xmm");
		ignore (Unix.system "xmodmap -e \"keysym  Left = 0xf0\" ");
		ignore (Unix.system "xmodmap -e \"keysym  Right = 0xf1\"");
		ignore (Unix.system "xmodmap -e \"keysym  Up = 0xf2\"");
		ignore (Unix.system "xmodmap -e \"keysym  Down = 0xf3\"");
		end
	
	(* Réinitialise les fichiers *)
	let empty_file () = ignore (Unix.system ">chat.tsin >game.tsin >chat.tsout >game.tsout")

	(* Raccourci pour les fonctions précédentes *)
	let init () =
		begin
			allow_arrow_press () ;
			empty_file () ;
		end
end ;;

module Fleche =
struct
	(* Facilite la détection de l'appui sur les touches *)
	let left = Char.chr 240 and right = Char.chr 241 and up = Char.chr 242 and down = Char.chr 243
	let left_maj = Char.uppercase_ascii left and rigth_maj = Char.uppercase_ascii right and down_maj = Char.uppercase_ascii down and up_maj = Char.uppercase_ascii up
end ;;

module Close =
struct
	(* Tue les processus contenus dans la liste et met fin à l'exécution *)
	let killall pids =
		for i=0 to (Array.length pids) - 1 do
			try
				Unix.kill pids.(i) Sys.sigkill
			with _ -> ()
		done
	
	(* Rétablit les paramètres par défauts et met fin à l'exécution *)
	let close pidlist =
		begin
			ignore (Unix.system "xmodmap ./touchmem.xmm");
			killall pidlist ;
			exit 0;
		end
end ;;