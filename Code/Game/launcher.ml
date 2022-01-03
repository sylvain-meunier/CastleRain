#use "topfind" ;;

#load "unix.cma" ;;
#require "graphics" ;;
#directory "+threads" ;;
#load "threads.cma" ;;

#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

exception Game ;;

let window_id = Init.create_win Init.launch_x Init.launch_y "LauncherCRrain" Init.launchername "" in
let xpos = string_of_int ((Init.screen_x - (int_of_string Init.launch_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.launch_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;;

let draw_texte texte = 
  let x = Graphics.size_x () and y = Graphics.size_y () and dx, dy = Graphics.text_size texte in Graphics.moveto ((x-dx)/2) ((y-dy)/2);
  Graphics.draw_string texte ;;

let texte = "Chargement en cours ..." in draw_texte texte ;;
Graphics.synchronize () ;;

let chateau = Sprite.create 0 (-300) "../../Images/launcher/cas" 3 1 true and bg = Sprite.create 0 0 "../../Images/launcher/bg" 8 0 true and logo = Sprite.create 300 520 "../../Images/launcher/logo" 7 10 true and alfred = Sprite.create 300 460 "../../Images/Sprite/Alfred_s" 5 2 true and name = Sprite.create 0 (-252) "../../Images/launcher/name" 1 5 false ;;

let bout_join = Sprite.create 0 (-104) "../../Images/launcher/bouton_join" 2 15 false
and bout_launch = Sprite.create 0 (-44) "../../Images/launcher/bouton_launch" 2 15 false
and bout_leave = Sprite.create 0 (-164) "../../Images/launcher/bouton_leave" 2 15 false ;;

let boutons = [bout_join; bout_launch; bout_leave] ;;

let rec reset_button boutons = match boutons with
| [] -> ()
| a::q -> (Sprite.set_animation a 0; reset_button q) ;;

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
      Unix.sleepf 0.2 ;
    done ;
  end ;;
    
let close_window () = Graphics.close_graph () in Sprite.set_function bout_leave close_window ;;
let game_start (a:unit):unit = raise Game in Sprite.set_function bout_launch game_start ;;

Sprite.center_x logo ;;
Sprite.center_x alfred ;;
Sprite.center_x bout_join ;;
Sprite.center_x bout_launch ;;
Sprite.center_x bout_leave ;;
Sprite.center_x name ;;
Sprite.update_one logo ;;

alfred.anim_tot <- 4 ;;

for i=0 to 105 do
  Sprite.animation bg (i mod 3 = 0) ;
  Sprite.animation chateau true ;
  Sprite.animation logo (i mod 3 <> 0) ;

  if i <= 38 then Sprite.rmove chateau 0 5 false ;
  if i <= 105 then Sprite.rmove logo 0 (-2) false ;
  if i <= 100 then (Sprite.rmove alfred 0 (-2) false; Sprite.animation alfred (i mod 3 <> 0)) ;
  if i = 100 then Sprite.animation alfred true ;
  if i = 12 then ignore (Thread.create (show_main_button) ()) ;
  if i = 103 then Sprite.set_animation alfred 4;
  if i = 105 then Sprite.set_animation alfred 1;

  Sprite.update_all () ;
  Sprite.show_all () ;
  Graphics.synchronize () ;

  Unix.sleepf 0.2 ;
done ;;

try
while true do
  for i = 0 to 9 do
    reset_button boutons ;

    let ev = Graphics.wait_next_event [Graphics.Poll] in
    let mx = ev.mouse_x and my = ev.mouse_y in let tok = Sprite.tokens_at mx my in
      if tok.priorite >= 15 then
        begin
          Sprite.set_animation tok 1 ;
          if ev.button then tok.fire () ;
        end
      else () ;

    Sprite.animation bg (i mod 3 = 0) ;
    Sprite.animation chateau true ;
    Sprite.animation logo (i mod 3 <> 0) ;

    Sprite.show_all () ;
    Graphics.synchronize () ;
    Unix.sleepf 0.15 ;
  done ;
done
with
  | Game -> ()
  | _ -> Close.close [||] ;;

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