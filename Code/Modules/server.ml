#use "topfind" ;;

#load "unix.cma" ;;

module Server =
struct
   let port = 2400

   let get_my_addr () = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

   let establish_server server_fun sockaddr =
      let domain = Unix.domain_of_sockaddr sockaddr in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 
      in Unix.bind sock sockaddr ;
         Unix.listen sock 20;
         Printf.printf "Server Ready\n%!" ;
         while true do
            let (s, caller) = Unix.accept sock
            in if Unix.fork() = 0 then
               let inchan = Unix.in_channel_of_descr s 
               and outchan = Unix.out_channel_of_descr s
               in (server_fun inchan outchan ;
                  close_out outchan ;
                  exit 0) ;
         done
      
   let main_server serv_fun sport =
      let mon_adresse = get_my_addr()
      in establish_server serv_fun  (Unix.ADDR_INET(mon_adresse, sport))
   
   let rec pseudo_valide p plist = match plist with
   | [] -> true
   | a::b -> not (String.equal p a) && pseudo_valide p b

   let rec shutdownclients out = match out with
   | [] -> exit 0
   | a::q -> (close_out a; shutdownclients q)

   let answer oc text =
      begin
         output_string oc (text^"\n") ;
         flush oc ;
      end

   let rec answer_all outc text = match outc with
   | [] -> ()
   | a::b -> (try Printf.printf "ANSWERING : %s\n%!" text ; answer a text with _ -> (); answer_all b text)

   let get_player_info pseudo_info room ic = pseudo_info ^ "ROOM " ^ room

   let rec envoi_info_joueur info room outc rooms = match outc, rooms with
   | [], _ | _, [] -> ()
   | a::l1, b::l2 -> (if b = room then answer a info; envoi_info_joueur info room l1 l2)
   
   let rec count_player current_nb inc outc pseud rooms msg mem1 mem2 mem3 mem4 = match (inc, outc, pseud, rooms) with
   | [], _, _, _ | _, [], _, _ | _, _, [], _ | _, _, _, [] -> mem1, mem2, mem3, mem4
   | a::l1, b::l2, c::l3, d::l4 ->
      try (answer b msg; count_player current_nb l1 l2 l3 l4 msg (a::mem1) (b::mem2) (c::mem3) (d::mem4))
      with _ -> (current_nb := !current_nb - 1 ; count_player current_nb l1 l2 l3 l4 msg mem1 mem2 mem3 mem4)
   
   let manage_msg ic outc rooms = let command = input_line ic in
      begin
         answer_all outc command ;
         if String.starts_with ~prefix:"PLAYER" command then let pseudo_info = input_line ic in let room = Scanf.sscanf (input_line ic) "ROOM %s" (fun x -> x) in envoi_info_joueur (get_player_info pseudo_info room ic) room outc rooms ;(* Envoi d'informations relatives à la position des joueurs en jeu *)
         if String.starts_with ~prefix:"CHAT" command then answer_all outc command ; (* Envoi d'un message dans le chat *)
         if String.starts_with ~prefix:"DEND" command then answer_all outc "DEND" ; (* Annonce que le dialogue a été lu *)
      end
   
   let rec manage_clients msgs outc rooms = match msgs with
   | [] -> ()
   | a::l1 -> (let ic = Unix.in_channel_of_descr a in manage_msg ic outc rooms; manage_clients l1 outc rooms)
   
   let get_pseudo ic = let text = (input_line ic) in if String.starts_with ~prefix:"PSEUDO" text then Scanf.sscanf text "%s %s" (fun x y -> y) else "Alfred"
   let get_room ic = let text = (input_line ic) in if String.starts_with ~prefix:"ROOM" text then Scanf.sscanf text "%s %s" (fun x y -> y) else ""

   let init_game clients current_nb =
      let inc, outc, pseud, rooms = !clients in
      begin
         answer_all outc "START 0" ;
         try
            while true do
               let couplemsgs = Unix.select (List.map (Unix.descr_of_in_channel) inc) [] [] 1. in
               let msgs, _, _ = couplemsgs in
               manage_clients (msgs) outc rooms ;
            done
         with _ -> (Printf.printf "SHUTTING DOWN !%!"; shutdownclients outc) ;
      end

   let game_server c nb sock =
      let (s, caller) = Unix.accept sock in
      let inchan = Unix.in_channel_of_descr s
      and outchan = Unix.out_channel_of_descr s in
      let pseudo = get_pseudo inchan in
      let room = get_room inchan in
      let clients = ref ([inchan], [outchan], [pseudo], [room]) and current_nb = ref 1 in
      begin
         answer outchan "NB 1" ;
         while !current_nb < nb do
            let (s, caller) = Unix.accept sock in
            let inchan = Unix.in_channel_of_descr s
            and outchan = Unix.out_channel_of_descr s in
            let pseudo = ref (get_pseudo inchan) in
            let room = get_room inchan and inc, outc, pseud, rooms = !clients in
            begin
               while not (pseudo_valide !pseudo pseud) do
                  pseudo := !pseudo ^ "_" ;
               done ;
               current_nb := !current_nb + 1 ;
               let msg = "NB " ^ string_of_int !current_nb in
               clients := count_player current_nb (inchan::inc) (outchan::outc) ((!pseudo)::pseud) (room::rooms) msg [] [] [] [] ;
               answer_all (outchan::outc) ("NB "^(string_of_int !current_nb)) ;
               if !current_nb = 0 then exit 0 ;
            end ;
         done ;
         Printf.printf "STARTING ON SUBSERVER : %d\n%!" c ;
         init_game clients current_nb ;
      end

   let find_port oc nb =
      let c = ref 1 in
      begin
         let mon_adresse = get_my_addr () and cond = ref true in
         while !c < 400 && !cond do
            try
               let sockaddr = (Unix.ADDR_INET(mon_adresse, port + !c)) in
               let domain = Unix.domain_of_sockaddr sockaddr in
               let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
               begin
                  Unix.bind sock sockaddr ;
                  cond := false ;
                  Printf.printf "Subserveur launched on port : %d\n%!" !c ;
                  Unix.listen sock 20;
                  answer oc ("LAUNCHED " ^ (string_of_int (port + !c))) ;
                  if Unix.fork () = 0 then game_server !c nb sock ;
               end
            with _ -> c := !c + 1
         done ;
         answer oc "FAILURE 0" ;
      end

   let answer_back ic oc =
      begin
         let s = input_line ic in
         let command, nb = Scanf.sscanf s "%s %d" (fun x y -> x, y) in
            begin
               if String.equal command "LAUNCH" then find_port oc nb
               else if String.equal command "PING" then answer oc "PONG"
               else (Printf.printf "Invalid command : %s\n%!" command; answer oc "INVALID") ;
            end
      end

   let launch () = main_server (answer_back) port

end ;;

Server.launch () ;;