module Init =
struct
  let create_win winx winy title truetitle filename = 
    begin
      Graphics.open_graph (" " ^ winx ^ "x" ^ winy^"+0+0") ;
      Graphics.set_window_title title ;
      let window_id = let screen = Unix.open_process_in "xdotool search --name \"" ^ title ^ "\"" in Scanf.sscanf (input_line screen) "%s" (fun x -> x) ;
      Unix.system ("xdotool set_window --name " ^ truetitle ^ " " ^ window_id) ;
      let oc = open_out filename in (Printf.fprintf oc "%s" window_id; close_out oc) ;
      window_id;
    end
end