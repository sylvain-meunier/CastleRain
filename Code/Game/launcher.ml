#use "topfind" ;;

#load "unix.cma" ;;
#require "graphics" ;;
#directory "+threads" ;;
#load "threads.cma" ;;

#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

(* ============================================================== *)

(* Interface prévue du launcher

Lancer une partie -> soit nouvelle (crée les fichiers) soit ancienne (charge)
Rejoindre une partie -> soit nouvelle (crée), soit ancienne (charge)
Tutoriel -> les tutos claires et précis -> crédits
Quitter le jeu *)

(* ============================================================== *)

exception Game ;;

(* Affichage de la fenêtre *)

let window_id = Init.create_win Init.launch_x Init.launch_y "LauncherCRrain" Init.launchername "" in
let xpos = string_of_int ((Init.screen_x - (int_of_string Init.launch_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.launch_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;;

let draw_texte texte = 
  let x = Graphics.size_x () and y = Graphics.size_y () and dx, dy = Graphics.text_size texte in Graphics.moveto ((x-dx)/2) ((y-dy)/2);
  Graphics.draw_string texte ;;

let texte = "Chargement en cours ..." in draw_texte texte ;;
Graphics.synchronize () ;;

(* Chargement des sprites utilisés *)

let chateau = Sprite.create 0 (-300) "../../Images/launcher/cas" 3 1 true and bg = Sprite.create 0 0 "../../Images/launcher/bg" 8 0 true and logo = Sprite.create 300 520 "../../Images/launcher/logo" 7 10 true and alfred = Sprite.create 300 460 "../../Images/Sprite/Alfred_s" 5 2 true and name = Sprite.create 0 (-252) "../../Images/launcher/name" 1 5 false ;;

let bout_join = Sprite.create 0 (-104) "../../Images/launcher/bouton_join" 2 15 false
and bout_launch = Sprite.create 0 (-44) "../../Images/launcher/bouton_launch" 2 15 false
and bout_leave = Sprite.create 0 (-164) "../../Images/launcher/bouton_leave" 2 15 false ;;

let boutons = [bout_join; bout_launch; bout_leave] ;;

(* Remet l'affichage des boutons par défaut *)
let rec reset_button boutons = match boutons with
| [] -> ()
| a::q -> (Sprite.set_animation a 0; reset_button q) ;;

(* Permet de faire apparaître les boutons à l'écran *)

let show_main_button () = let dy = 3 in
  begin
    Sprite.visibility bout_join true ;
    Sprite.visibility bout_launch true ;
    Sprite.visibility bout_leave true ;
    Sprite.visibility name true ;
    for i=0 to 82 do
      Sprite.rmove bout_join 0 dy false ;
      Sprite.rmove bout_leave 0 dy false ;
      Sprite.rmove bout_launch 0 dy false ;
      Sprite.rmove name 0 dy false ;
      Sprite.update_one name ;
      Sprite.update_one bout_join ;
      Sprite.update_one bout_leave ;
      Sprite.update_one bout_launch ;
      Unix.sleepf 0.05 ;
    done ;
  end ;;

(* Ferme la fenêtre lors de l'appui sur le bouton adéquat *)
let close_window () = Graphics.close_graph () in Sprite.set_function bout_leave close_window ;;
(* Devrait lancer le jeu (en pratique, fait crasher l'application) *)
let game_start (a:unit):unit = raise Game in Sprite.set_function bout_launch game_start ;;

Sprite.center_x logo ;;
Sprite.center_x alfred ;;
Sprite.center_x bout_join ;;
Sprite.center_x bout_launch ;;
Sprite.center_x bout_leave ;;
Sprite.center_x name ;;
Sprite.update_one logo ;;

alfred.anim_tot <- 4 ;;
Sprite.set_animation alfred 1 ;

for i=0 to 105 do
  Sprite.animation bg (i mod 3 = 0) ;
  Sprite.animation chateau true ;

  if i <= 38 then Sprite.rmove chateau 0 5 false ;
  if i <= 105 then Sprite.rmove logo 0 (-2) (i mod 12 = 0)  ;
  if i <= 100 then (Sprite.rmove alfred 0 (-2) false; Sprite.animation alfred (i mod 3 = 0)) ;
  if i = 100 then Sprite.animation alfred true ;
  if i = 12 then ignore (Thread.create (show_main_button) ()) ;
  if i = 103 then Sprite.set_animation alfred 4;
  if i = 105 then Sprite.set_animation alfred 1;

  Sprite.update_all () ;
  Sprite.show_all () ;
  Graphics.synchronize () ;

  Unix.sleepf 0.05 ;
done ;;

let anim_logo i = int_of_float (5. *. (sin (Float.pi /. 5.25 *. (float_of_int i)))) ;;

try
  let logo_x = logo.posx and logo_y = logo.posy in
  while true do
    for i = 1 to 21 do
      reset_button boutons ;

      (* Test si la souris passe sur un bouton *)
      let ev = Graphics.wait_next_event [Graphics.Poll] in
      let mx = ev.mouse_x and my = ev.mouse_y in let tok = Sprite.tokens_at mx my in
        if tok.priorite >= 15 then
        begin
            (* Auquel cas change l'affichage de ce bouton *)
            Sprite.set_animation tok 1 ;
            if ev.button then tok.fire () ;
          end
        else () ;

      (* Gère les animations des images de fond *)
      Sprite.animation bg (i mod 3 = 0) ;
      Sprite.animation chateau true ;

      Sprite.move logo logo_x (logo_y + anim_logo i) (true) ;
      Sprite.update_one logo ;

      Sprite.show_all () ;
      Graphics.synchronize () ;
      Unix.sleepf 0.3 ;
    done ;
  done
with
  | Game -> ()
  | _ -> exit 0 ;;

(* ===================================================================== *)

(*/!\ Le code qui suit ne marche pas car Graphics ne tolère pas qu'on ferme une fenêtre pour en ouvrir une autre, et car faire un fork sur une fenêtre déjà ouverte semble poser de sérieux problèmes aux X serveur *)
(* Pour voir ce qui aurait dû s'afficher, il faudra lancer le fichier Code/Game/game.ml *)

(* ===================================================================== *)

(* A terme ceci sera dans un autre fichier : game.ml *)

#use "./chat.ml" ;;

(* let launch_game () =
  let window_id = Init.create_win Init.game_x Init.game_y "game_geeettttttt dunked on!!" "[CastleRain - Game]" "game.id" in
  let xpos = string_of_int ((Init.screen_x - (int_of_string Init.game_x) - (int_of_string Init.chat_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;;
  *)

let launch_game () =
  let window_id = Init.get_window_id Init.launchername in
				begin
					ignore (Unix.system ("xdotool set_window --name \"" ^ Init.gamename ^ "\" " ^ window_id)) ;
          let xpos = string_of_int ((Init.screen_x - (int_of_string Init.game_x) - (int_of_string Init.chat_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;
          ignore (Unix.system ("xdotool windowsize " ^ window_id ^ " " ^ Init.game_x ^ " " ^ Init.game_y)) ;
        end ;;

let pids = ref [||] ;;

(* Lance les fonctions correspondants aux différentes parties du programme *)

match Unix.fork () with
| 0 -> (Chat.func (); exit 0)
| cpid -> pids := [|cpid; Unix.getpid ()|] ;;

launch_game () ;;

(* Pas très élégant mais nécessaire vu la qualité de Graphics *)
#use "../Modules/player.ml" ;;

draw_texte "Utilisez les flèches pour vous déplacer !\n(Seul le mode Showcase est malheureusement disponible...)\nVous pouvez également utiliser le chat à côté ->\nUtilisez entrée pour envoyer un message et les flèches pour déplacer le curseur" ;;

Player.func () ;;

Close.close !pids ;;