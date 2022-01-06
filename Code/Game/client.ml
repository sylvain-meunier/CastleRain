#use "topfind";;
#load "unix.cma";;

#use "../Modules/pyliste.ml" ;;
#use "../Modules/castlerain.ml" ;;
#use "../Modules/client.ml" ;;

(* Ping au lancement *)

Client.ping () ;;

Client.launch 1 "AlfredAl" "village:1" ;;

(* Client.join 1 "Alfred" "village:2" *)

(* Lancer une partie -> demander : nb de joueur, pseudo (sans espace, String.trim) *)
(* Demander le nombre de joueur *)
(* Chat.connect NB *)