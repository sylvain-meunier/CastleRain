#use "topfind";;
#load "unix.cma" ;;
#directory "+threads";;
#load "threads.cma";;

module Chat_server =
struct
  (* Port de connexion *)
  let port = 2400

  (* Permet de créer le serveur *)
  let get_my_addr () = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

  (* Permet d'accepter une connexion *)
  let accept_con sock =
    let (s, caller) = Unix.accept sock in
    let inchan = Unix.in_channel_of_descr s and outchan = Unix.out_channel_of_descr s in
    (inchan, outchan, s)
  
  (* Ajoute une connexion à la liste globale *)
  let add_to_list args =
    let connex_list, sock = args in
    let ic, oc, s = accept_con sock in
    connex_list := (ic, oc, s)::!connex_list
  
  (* Ajoute des connexions à la liste globale *)
  let listen_client args =
    let cl, sock = args in
    try
      while true do
        add_to_list args ;
      done
    with _ -> (Unix.shutdown sock Unix.SHUTDOWN_ALL; Unix.close sock)
  
  (* Met fin à une connexion *)
  let shutdown_con conn =
    let ic, oc, s = conn in
    begin
      close_in ic ;
      close_out oc ;
      Unix.close s ;
    end
  
  (* Détermine si un message a été envoyé et le renvoie dans ce cas *)
  let rec is_send conlist = match conlist with
  | [] -> ""
  | (ic, oc, s)::q ->
    try
      let ics, garb1, garb2 = Unix.select [Unix.descr_of_in_channel ic] [] [] 0.01 in
      if List.length ics <> 0 then input_line ic
      else is_send q
    with _ -> is_send q

  (* Envoie un message à un client *)
  let answer oc text =
    begin
      Printf.printf "SEND : %s\n%!" text ;
      output_string oc text ;
      flush oc ;
    end

  (* Transmet un message à tous les clients *)
  let rec transmet msg conlist mem = match conlist with
  | [] -> mem
  | (ic, oc, s)::q ->
    try
      answer oc msg ;
      transmet msg q ((ic, oc, s)::mem)
    with _ ->
      try
        shutdown_con (ic, oc, s) ;
        transmet msg q mem
      with _ -> transmet msg q mem 

  (* Lie les deux fonctions précédentes *)
  let main_function connexions =
    while true do
      let msg = is_send !connexions in
      if msg <> "" then (Printf.printf "%!RECEIVED : %s\n" msg ; connexions := (transmet msg !connexions [])) ;
      Unix.sleepf 0.1 ;
    done

  (* Crée le serveur principal *)
  let establish_server sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    let connexions = ref [] in
    begin
      Unix.bind sock sockaddr ;
      Unix.listen sock 20 ;
      Printf.printf "Server Ready\n%!" ;
      ignore (Thread.create (listen_client) (connexions, sock)) ;
      main_function connexions ;
    end
      
  (* Lance le serveur *)
  let launch () =
      let adresse = get_my_addr() in establish_server (Unix.ADDR_INET(adresse, port))

end ;;

Chat_server.launch () ;;