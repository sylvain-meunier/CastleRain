#use "topfind" ;;
#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

#load "unix.cma" ;;
#require "graphics" ;;

let screen_x, screen_y = let screen = Unix.open_process_in "xrandr" in Scanf.sscanf (input_line screen) "%s %s %s %s %s %s current %d x %d" (fun s1 s2 s3 s4 s5 s6 y z -> y, z) ;;

let game_x = "700" and game_y = "500" ;;
let chat_x = "300" and chat_y = winy ;;

let create_win winx winy title truetitle filename = 
begin
  Graphics.open_graph (" " ^ winx ^ "x" ^ winy^"+0+0") ;
  Graphics.set_window_title title ;
  let window_id = let screen = Unix.open_process_in "xdotool search --name \"game_geeettttttt dunked on!!\"" in Scanf.sscanf (input_line screen) "%s" (fun x -> x) ;
  Unix.system ("xdotool set_window --name " ^ truetitle ^ " " ^ window_id) ;
  let oc = open_out filename in (Printf.fprintf oc "%s" window_id; close_out oc) ;
  window_id;
end ;;

create_win game_x game_y "game_geeettttttt dunked on!!" "[CastleRain - Game]" "game.id" ;;

let xpos = string_of_int ((screen_x - (int_of_string game_x))/2 - (int_of_string chat_x)) and ypos = string_of_int ((screen_y - (int_of_string game_y))/2) in Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos) ;;

Unix.sleep 200 ;;