#use "topfind";;

#load "unix.cma";;

module Client =
struct
  let serverip = "127.0.1.1"
  let serverport = 2400

  let chat_in = open_in "./chat.tsout" and chat_out = open_out "./chat.tsin"

  let open_connection sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 
    in try Unix.connect sock sockaddr ;
           (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)
      with exn -> (Unix.close sock ; raise exn)
 
  let shutdown_connection inchan =
    Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND
  
  let main_client client_fun =
      let serveur_adr = Unix.inet_addr_of_string serverip in
      let sockadr = Unix.ADDR_INET(serveur_adr, serverport) in 
      let ic,oc = open_connection sockadr in
      (client_fun ic oc ; shutdown_connection ic)
 
  let client_fun ic oc = 
    try
      begin
        flush oc ;
        output_string oc "LAUNCH 1" ;
        flush oc ;
        Printf.printf "WAITING %!" ;
        let ans = input_line ic in
          begin
            Printf.printf "%s\n %!" ans ;
          end
      end
    with 
      | Exit -> exit 0
      | exn -> (shutdown_connection ic ; raise exn)
 
  let launch () = main_client client_fun

 end ;;

(* Lancer une partie -> soit nouvelle (crée les fichiers) soit ancienne (charge) *)
(* Rejoindre une partie -> soit nouvelle (crée), soit ancienne (charge) *)
(* Tutoriel -> les tutos claires et précis -> crédits *)
(* Quitter le jeu *)

(*QUAND JOIN :
PSEUDO\n
<pseudo>
ROOM\n
<room>

QUAND LAUNCH :
LAUNCH <nb>

QUAND MOUVEMENT :
PLAYER\n
PSEUDO <pseudo>\n
ROOM <room>\n
HEAD <dir> <anim>\n
POS <x> <y>\n

QUAND CHAT:
CHAT\n
<msg>

QUAND DEND :
DEND*)