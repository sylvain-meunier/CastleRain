(* Fichier de test pour la connexion au serveur *)

#use "topfind";;
#load "unix.cma";;

#use "../Modules/pyliste.ml" ;;
#use "../Modules/castlerain.ml" ;;
#use "../Modules/client.ml" ;;

(* Ping au lancement *)

Client.ping 0 ;;

let i = 1 in
if i = 0 then (Client.launch 1 "AlfredAl" "village:1"; ()) else let ic, oc = Client.join i "Alfred" "village:2" in (Unix.sleepf 1.; Client.sendtoserver oc "Coucou", 0) ;;

Unix.sleepf 10. ;;

(* Lancer une partie -> demander : nb de joueur, pseudo (sans espace, String.trim) *)
(* Demander le nombre de joueur *)
(* Chat.connect NB *)