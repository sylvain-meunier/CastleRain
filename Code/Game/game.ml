#use "topfind" ;;

#load "unix.cma" ;;
#require "graphics" ;;

#use "../Modules/castlerain.ml" ;;
#use "./chat.ml" ;;

let launch_game () =
  let window_id = Init.create_win Init.game_x Init.game_y "game_geeettttttt dunked on!!" Init.gamename "game.id" in
  let xpos = string_of_int ((Init.screen_x - (int_of_string Init.game_x) - (int_of_string Init.chat_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;;

Init.init () ;;

let pids = ref [||] ;;

(* Lance les fonctions correspondants aux différentes parties du programme *)

(* Premièrement le chat *)
match Unix.fork () with
| 0 -> Chat.func ()
| cpid -> pids := [|cpid; Unix.getpid ()|] ;;

launch_game () ;;

(* Pas très élégant mais nécessaire vu la qualité de Graphics *)
#use "../Modules/player.ml" ;;

(* Affichage du message *)
Graphics.moveto 200 350 ;
Graphics.draw_string "Essayez de toucher les cercles (Utilisez les fleches pour vous deplacer)" ;
Graphics.moveto 200 300 ;
Graphics.draw_string "(Seul le mode Showcase est malheureusement disponible pour l'instant...)" ;
Graphics.moveto 200 250 ;
Graphics.draw_string "Vous pouvez egalement utiliser le chat a cote ->" ;
Graphics.moveto 200 200 ;
Graphics.draw_string "Utilisez entree pour envoyer un message et les fleches pour deplacer le curseur" ;
Graphics.moveto 200 150 ;
Graphics.draw_string "Assurez-vous que la fenetre a bien le focus !" ;
Graphics.moveto 200 400 ;
Graphics.draw_string "Bienvenue sur CastleRain !" ;;

Unix.sleepf 1. ;;

(* Lancement du mini-jeu *)

try
  Player.func () ;
  Close.close !pids ;
with _ -> Close.close !pids ;;