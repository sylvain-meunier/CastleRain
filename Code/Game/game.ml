#use "topfind" ;;

#load "unix.cma" ;;
#require "graphics" ;;

#use "../Modules/pyliste.ml" ;;
#use "../Modules/castlerain.ml" ;;
#use "./chat.ml" ;;

let launch_game () =
  let window_id = Init.create_win Init.game_x Init.game_y "game_geeettttttt dunked on!!" Init.gamename "game.id" in
  let xpos = string_of_int ((Init.screen_x - (int_of_string Init.game_x) - (int_of_string Init.chat_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;;

Init.init () ;;

(* Contient la liste des sous-processus initialisés *)
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
Graphics.draw_string "Les ronds bleu clairs valent 1 point, bleu fonces valent 2 points, les verts 5" ;
Graphics.moveto 200 300 ;
Graphics.draw_string "Evitez les carres rouges !" ;
Graphics.moveto 200 250 ;
Graphics.draw_string "Appuyez sur une touche pour commencer !" ;
Graphics.moveto 200 200 ;
Graphics.draw_string "Assurez-vous que la fenetre a bien le focus !" ;
Graphics.moveto 200 150 ;
Graphics.draw_string "Vous pouvez passer cette fenetre en plein ecran !" ;;
Graphics.moveto 200 400 ;
Graphics.draw_string "Essayez de toucher les cercles (Utilisez les fleches pour vous deplacer)" ;;

(* Lancement du mini-jeu *)

try
  Player.func () ;
  Close.close !pids ;
with _ -> Close.close !pids ;;