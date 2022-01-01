#use "topfind" ;;

#load "unix.cma" ;;
#use "./pyliste.ml" ;;

module Server =
struct
   let port = 240

   let get_my_addr () = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

   let establish_server server_fun sockaddr =
      let domain = Unix.domain_of_sockaddr sockaddr in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 
      in Unix.bind sock sockaddr ;
         Unix.listen sock 20;
         while true do
            let (s, caller) = Unix.accept sock 
            in match Unix.fork() with
                  0 -> if Unix.fork() <> 0 then exit 0 ; 
                        let inchan = Unix.in_channel_of_descr s 
                        and outchan = Unix.out_channel_of_descr s
                        in server_fun inchan outchan ;
                           close_in inchan ;
                           close_out outchan ;
                           exit 0
                  id -> Unix.close s; ignore(Unix.waitpid [] id)
         done
      
   let main_server serv_fun sport =
      let mon_adresse = get_my_addr()
      in establish_server serv_fun  (Unix.ADDR_INET(mon_adresse, sport))

   let answer oc text =
      begin
         output_string oc text ;
         flush oc ;
      end

   let game_server c nb =
      let mon_adresse = get_my_addr() in
      let sockaddr = (Unix.ADDR_INET(mon_adresse, c)) in
      let domain = Unix.domain_of_sockaddr sockaddr in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
         Unix.bind sock sockaddr ;
         Unix.listen sock 10;
         let (s, caller) = Unix.accept sock in
         let inchan = Unix.in_channel_of_descr s
         and outchan = Unix.out_channel_of_descr s in
         let pseudo = input_line inchan in
         let clients = Pyliste.new_pyliste [|inchan; outchan; pseudo|] in
         begin
            while clients.taille < nb do
               let (s, caller) = Unix.accept sock in
               let inchan = Unix.in_channel_of_descr s
               and outchan = Unix.out_channel_of_descr s in
               let pseudo = input_line inchan in
               begin
                  Pyliste.add clients [|inchan; outchan; pseudo|] ;
                  let i = ref 0 and msg = "NB " ^ string_of_int clients.taille in
                  while !i < clients.taille do
                     try answer (Pyliste.get client !i).(1) msg
                     with _ -> (Pyliste.remove clients !i; i := !i - 1)
                     i := !i + 1 ;
                  done;
                  if clients.taille = 0 then exit 0 ;
               end ;
            done ;
            init_game clients ;
         end

      let init_game clients =
         begin
            let msg = "START 0" in
            for i=0 to clients.taille - 1 do
               try answer (Pyliste.get client i).(1) msg
               with _ -> (Pyliste.remove clients i)
            done
            (* Envoyer les textures, les informations de placement... *)
            (* input_line est bloquante -> comment gérer les clients ? Une thread par client ? *)
            (* https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora166.html -> Unix.select*)
         end

   let find_port oc nb =
      let c = ref 0 in
      begin
         while !c < 400 &&  c >= 0 do
            try
               !c = -1
            with _ -> c := !c + 1;
         done ;
         if !c = -1 then
            begin
               answer oc "LAUNCHED " ^ (string_of_int (port + !c)) ;
               match Unix.fork () with
               | 0 -> game_server !c nb
               | pid -> exit 0 ;
            end
         else answer oc "ECHEC 0" ;
      end

   let answer_back ic oc =
      try
         while true do
            let s = input_line ic in
            let command, nb = Scanf.sscanf s "%s %d" (fun x y -> x, y) in
               begin
                  if String.equal command "LAUNCH" then  (find_port oc nb; exit 0;)
               end
         done
      with _ -> (Printf.printf "Fin du traitement\n" ; exit 0)
      
   let launch () = Unix.handle_unix_error main_server answer_back port

end ;;

Server.launch () ;;