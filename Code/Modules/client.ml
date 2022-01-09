(* Ce module contient les fonctions utiles pour établir une connexion avec le serveur et lui envoyer des messages *)

#use "topfind";;

#load "unix.cma";;

module Client =
struct
  (* Cette variable peut être modifiée en cas de besoin *)
  let serverip = "127.0.1.1"
  (* Cette variable doit correspondre à celle indiquée dans le fichier Code/Modules/server.ml *)
  let serverport = 2400

  exception SERVER_ERROR
  exception Notmsg

  (* Envoi un message au serveur *)
  let sendtoserver oc text =
    begin
      output_string oc (text^"\n") ;
      flush oc ;
    end
  
  (* Renvoie la commande et l'information basique contenue dans un message *)
  let decode answer = Scanf.sscanf answer "%s %d" (fun x y -> x, y)

  (* Essaie de se connecter au serveur, renvoie alors le couple in_channel, out_channel de connexion *)
  let open_connection sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 
    in
      try Unix.connect sock sockaddr ;
          (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)
      with exn -> (Unix.close sock ; raise exn)
  
  (* utilise la fonction précédente pour se connecter au serveur à partir du port de connexion *)
  let connect_to_server port =
      let serveur_adr = Unix.inet_addr_of_string serverip in
      let sockadr = Unix.ADDR_INET(serveur_adr, port) in 
      open_connection sockadr

  (* Ferme la connexion courante avec le serveur, et renvoie un nouveau couple de in/out channel *)
  let ask_launch ic oc nb_player =
    try
      begin
        sendtoserver oc ("LAUNCH "^(string_of_int nb_player)) ;
        let ans = input_line ic in
        let com, port = decode ans in
          begin
            Printf.printf "OKAY : %s%!" com ;
            if not (String.equal com "LAUNCHED") then raise SERVER_ERROR ;
            close_out oc ;
            Printf.printf " ON PORT :  %d\n%!" port ;
            connect_to_server port, port ;
          end
      end
    with 
      | Exit -> exit 0
      | exn -> (close_out oc ; raise exn)

  (* Décode un message provenant du chat *)
  let get_text_msg msg = if String.equal "CHAT " (String.sub msg 0 5) then String.sub msg 5 (String.length msg - 5) else raise Notmsg
    
  (* Attends que le serveur démarre la partie *)
  let wait_players ic oc pseudo room =
    begin
      sendtoserver oc ("PSEUDO " ^ (String.trim pseudo)) ;
      sendtoserver oc ("ROOM " ^ room) ;
      let ans = input_line ic in
      let command, nb = decode ans in
      Printf.printf "Current player : %d\n%!" nb ;
      let nb_ = ref nb in
      while (let com, nb = decode (input_line ic) in nb_ := nb; not (String.equal com "START")) do
        (* Affiche le nombre de joueurs *)
        Printf.printf "Current player : %d\n%!" !nb_ ;
        Unix.sleepf 0.01 ;
      done ;
      Printf.printf "STARTING !\n%!" ;
      (ic, oc) ;
    end
  
  (* Permet de rejoindre une partie *)
  let join dport pseudo room =
    let ic, oc = connect_to_server (serverport + dport) in wait_players ic oc pseudo room, (serverport + dport)
 
  (* Renvoie les in/out channel connectés au bon serveur *)
  let launch nb_player pseudo room =
    let ic1, oc1 = connect_to_server serverport in
    let (ic, oc), port = ask_launch ic1 oc1 nb_player in wait_players ic oc pseudo room, port

  (* Teste si le serveur est accessible *)
  let ping port =
    try
      let ic, oc = connect_to_server (serverport + port) in
      let _ = sendtoserver oc "PING 0" in
      String.equal (input_line ic) "PONG"
    with _ -> false ;

 end ;;

(* Table d'envoi des messages *)

(*QUAND JOIN :
PSEUDO <pseudo>\n
ROOM <room>

QUAND LAUNCH :
LAUNCH <nb>

QUAND MOUVEMENT :
PLAYER\n
PSEUDO <pseudo> ROOM <room> HEAD <dir> <anim> POS <x> <y>

QUAND CHAT:
CHAT <msg>

QUAND DEND :
DEND
*)

(* Table de réception

NB <nombre> -> Indique le nombre de joueur actuellement connecté au serveur
RM 0 -> Indique qu'un joueur vient de se déconnecter
A termes, ces deux messages contiendront également le pseudo du joueur

Ajouter également tous les messages de la tablea d'envoi
*)