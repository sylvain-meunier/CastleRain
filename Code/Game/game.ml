#use "topfind" ;;
#use "../Modules/castlerain.ml" ;;
#use "../Modules/sprite.ml" ;;

#load "unix.cma" ;;
#require "graphics" ;;

let screen_x, screen_y = let screen = Unix.open_process_in "xrandr" in Scanf.sscanf (input_line screen) "%s %s %s %s %s %s current %d x %d" (fun s1 s2 s3 s4 s5 s6 y z -> y, z) ;;

let game_x = "700" and game_y = "500" ;;
let chat_x = "300" and chat_y = game_y ;;
let dial_x = game_x and dial_y = "200" ;;

module Init =
struct
  let pids = ref [||]

  let create_win winx winy title truetitle filename =
    begin
      Graphics.open_graph (" " ^ winx ^ "x" ^ winy ^ "+0+0") ;
      Graphics.set_window_title title ;
      Graphics.auto_synchronize false ;
      let window_id = (let screen = Unix.open_process_in ("xdotool search --name \"" ^ title ^ "\"") in Scanf.sscanf (input_line screen) "%s" (fun x -> x)) in
        begin
          ignore (Unix.system ("xdotool set_window --name \"" ^ truetitle ^ "\" " ^ window_id)) ;
          let oc = open_out filename in (Printf.fprintf oc "%s" window_id; close_out oc) ;
          window_id ;
        end
      end
  
  let init func = 
    try
      begin
        func () ;
        Unix.sleep 200 ;
        Graphics.close_graph () ;
      end
    with _ -> exit 0

  let killall pids =
    for i=0 to (Array.length pids) - 1 do
      try
        Unix.kill pids.(i) Sys.sigkill
      with _ -> ()
    done
end ;;

module Game =
struct
  let func () =
    begin
      let window_id = Init.create_win game_x game_y "game_geeettttttt dunked on!!" "[CastleRain - Game]" "game.id" in
      let xpos = string_of_int ((screen_x - (int_of_string game_x) - (int_of_string chat_x))/2) and ypos = string_of_int ((screen_y - (int_of_string game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;
      let com = ref 'a' in
      while true do
        while not (Graphics.key_pressed ()) do
          Graphics.synchronize ();
          Unix.sleepf 0.001;
        done;
        com := (Graphics.wait_next_event [Graphics.Key_pressed]).key ;
      done ;
    end
end ;;

module Chat =
struct
  let func () =
    begin
      let window_id = Init.create_win chat_x chat_y "chat_A12Z46F_Cirno" "[CastleRain - Chat]" "chat.id" in
      let xpos = string_of_int ((screen_x + (int_of_string game_x) - (int_of_string chat_x))/2) and ypos = string_of_int ((screen_y - (int_of_string chat_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;
      let com = ref 'a' in
      while true do
        while not (Graphics.key_pressed ()) do
          Graphics.synchronize ();
          Unix.sleepf 0.001;
        done;
        com := (Graphics.wait_next_event [Graphics.Key_pressed]).key ;
      done ;
    end
end ;;

module Dial =
struct
  let func () =
    begin
      let window_id = Init.create_win dial_x dial_y "dial_Plato_1728_Zefzeruzaz" "[CastleRain - Dialogues]" "dial.id" in
      let xpos = string_of_int ((screen_x - (int_of_string game_x) - (int_of_string chat_x))/2) and ypos = string_of_int ((screen_y - (int_of_string game_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;
      ignore (Unix.system ("xdotool windowunmap " ^ window_id)) ;
    end
end ;;

(* Lance les fonctions correspondants aux différentes parties du programme *)

match Unix.fork () with
| 0 -> Init.init Game.func
| gpid -> (match Unix.fork () with
  | 0 -> Init.init Chat.func
  | cpid -> (match Unix.fork () with
    | 0 -> Init.init Dial.func
    | dpid ->
      begin
        Unix.sleep 20 ;
        Init.pids := [|gpid; cpid; dpid|] ;
        Init.killall (!Init.pids) ;
        exit 0 ;
      end )) ;;