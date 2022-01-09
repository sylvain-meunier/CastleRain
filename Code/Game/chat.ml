(* Module de chat (placé dans ce dossier car nécessite de nombreux autres modules) *)

#use "topfind";;
#load "unix.cma" ;;
#directory "+threads";;
#load "threads.cma";;
#require "graphics";;

#use "../Modules/castlerain.ml" ;;
#use "../Modules/pyliste.ml" ;;
#use "../Modules/client.ml" ;;

module Chat =
struct
	exception End

	(* Envoie dans le canal d'entrée du chat tous les messages précédemment envoyés, pourrait être optimisé *)
	let reset_mem () = Unix.system "cat ./memchat.out >chat.tsin"

	(* Voir le directeur : Director/chat.txt *)
	let file_in_path = "./chat.tsin" and file_out_path = "./chat.tsout" and memchat_path = "./memchat.out"

	let file_in = open_in file_in_path and file_out = open_out file_out_path and memchat = open_out memchat_path

	(* Auteur par défaut des messages *)
	let author = "J1"

	let largeur_fenetre = int_of_string Init.chat_x
	let hauteur_fenetre = int_of_string Init.chat_y

	(* Taille du texte lors de l'affichage à l'écran (ne semble pas fonctionner, du moins pas sous WSL 2) *)
	let taille_txt = 20

	(* Permet une marge d'écriture par rapport aux bords de la fenêtre *)
	let x_min_txt = 15
	let y_min_txt = 25

	(* Séparation entre l'espace d'écriture d'un message et l'espace d'affichage des message *)
	let y_separation = 50

	(* Taille maximale (en caractères) d'un message *)
	let taille_max_message = 210

	(* Retient le message écrit sous la forme d'une liste *)
	let message_ecrit = Pyliste.new_pyliste ' '
	(* Curseur *)
	let carac_en_cours = ref 0

	type message = {author : string; message : string}
	let liste_message = ref [{author = "System" ; message = "Bienvenue sur le chat !"}]

	(* Convertit un tableau de caractères en chaine de caractères *)
	let charArray_to_String tableau = 
		String.init (Pyliste.taille tableau) (Pyliste.get tableau)

	(* Fait un retour à la ligne *)
	let retour_ligne word = let dx, dy = Graphics.text_size word in Graphics.moveto x_min_txt (Graphics.current_y () - dy)

	(* Permet d'envoyer un messagr *)
	let send_msg oc = let msg = ((author^"|"^(charArray_to_String message_ecrit))^"\n") in
		begin
			Client.sendtoserver oc ("CHAT " ^ msg) ;
			output_string file_out msg ;
			flush file_out ;
			carac_en_cours := 0 ;
			Pyliste.empty message_ecrit ;
		end

	(* Reconstitue un message si celui-ci contenant le caractères '|' *)
	let full_msg msg liste =
		let rec aux liste mem = match liste with
			| [] -> mem
			| a::b -> aux b (mem^"|"^a)
		in
		aux liste msg

	(* Affichage les messages lus dans le canal d'entrée *)
	let client_fun ic = 
			while true do
				try 
					let msg = (input_line ic) in
					let r = (if String.starts_with ~prefix:"NB" msg then "System|Un nouveau joueur a rejoint le chat" else if String.starts_with ~prefix:"RM" msg then "System|Un joueur est parti du chat" else Client.get_text_msg msg) in
					try
						output_string memchat r;
						flush memchat;
						let a::m::r = String.split_on_char '|' r in liste_message := ({author = a; message = full_msg m r} :: !liste_message);
					with _ -> Printf.printf "Erreur : %s %!" r

				with
				| End_of_file -> () ;
				
				Unix.sleepf 0.3 ;
			done

	(* Renvoie le premier mot d'un texte, ainsi que les suivants sous la forme d'une liste *)
	let cut_word text =
		try
			let w::garb = String.split_on_char ' ' text in w, garb
		with _ -> text, []

	(* Affiche un message caractère par caractère *)
	let draw_by_char word = 
		for i = 0 to (String.length word - 1) do
			if (largeur_fenetre - (Graphics.current_x())) <= x_min_txt then retour_ligne word ;
			Graphics.draw_char word.[i];
		done

	(* Affiche un message à l'écran *)
	let show_msg_on_screen word = let dx, dy = Graphics.text_size word in
			begin
				if Graphics.current_x() <> x_min_txt && (Graphics.current_x()) + dx + x_min_txt >= largeur_fenetre then retour_ligne word ;
				(* Teste si le message ne tient pas sur une ligne *)
				if dx + x_min_txt >= largeur_fenetre then draw_by_char word
				else Graphics.draw_string word
			end

	(* Affiche une liste de messages à l'écran (l'argument l2 est en fait une sous-liste contenant les mots renvoyés par la fonction cut_word) *)
	let rec show_msg l1 l2 = match l1, l2 with
	| [], [] -> ()
	| liste, a::b -> (show_msg_on_screen (a^" "); show_msg liste b)
	| a::b, [] -> let word, gar = cut_word a in (show_msg_on_screen (word^" "); show_msg b gar)

	(* Indique qu'il n'y a plus la place d'afficher de nouveaux messages *)
	exception Restart

	(* Affiche les messages à l'écran en permanence *)
	let rec display_msgs img =
		try 
			while true do
				if List.length !liste_message = 0 then raise Restart ;
				Graphics.draw_image img 0 y_separation ;
				Graphics.draw_segments [|(0, y_separation, largeur_fenetre, y_separation)|] ;
				let dx, dy = Graphics.text_size ((List.hd(!liste_message)).message) in Graphics.moveto x_min_txt (hauteur_fenetre - y_min_txt - dy) ;
				let rec aux list_msg = match list_msg with
					| [] -> ()
					| x::q when Graphics.current_y() <= y_separation + dy -> (liste_message := (List.rev (x::q)); raise Restart)
					| x::q ->
						begin
							show_msg [x.author^": "^x.message] [] ;
							if q <> [] then let dx, dy = Graphics.text_size ((List.hd(q)).message) in Graphics.rmoveto (x_min_txt - (Graphics.current_x ())) (- int_of_float (float_of_int(dy)*. 1.5));
							aux q;
						end
				in aux (List.rev !liste_message) ;
				Unix.sleepf 0.3 ;
			done
		with Restart -> (Unix.sleepf 0.3 ; display_msgs img)

	(* Affiche le message que le joueur est en train d'écrire *)
	let display_written img =
		let _ = Graphics.draw_image img 0 0, Graphics.moveto x_min_txt (y_separation - y_min_txt) in
			begin
				show_msg [charArray_to_String message_ecrit] [] ;
			end

	(* Fonction principale *)
	let skel f_init f_end f_key f_mouse =
		let oc = f_init () in
		let img = Graphics.make_image (Array.make_matrix (y_separation-2) largeur_fenetre Graphics.white) in
		while true do
			try
				display_written img ;
				let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
				if s.Graphics.keypressed then f_key s.Graphics.key oc
				else if s.Graphics.button then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
			with
				_ -> f_end () ;
		done
	
	let pids = ref [||]

	(* Fonction d'initialisation *)
	let f_init () =
		begin
			ignore (reset_mem ()) ;
			Graphics.set_text_size taille_txt ;
			Graphics.set_color Graphics.black ;
			Graphics.moveto x_min_txt (hauteur_fenetre - y_min_txt) ;
			if not (Client.ping 0) then (show_msg ["System : Unable to reach server, please try again later..."] []; retour_ligne "S";  retour_ligne "S";  show_msg ["System : Closing in 10 seconds"] []; raise Client.SERVER_ERROR)
			else let c = ref 1 in
			let (ic, oc), servnumb =
					try

						(* Cherche sous-serveur déjà ouvert *)
						while not (Client.ping !c) && !c <= 10 do
							c := !c + 1 ;
						done ;
						Client.join !c "Alfred" "village:2" ;

					(* Sinon, en crée un nouveau *)
					with _ -> Client.launch 1 "Alfred" "village:1"
			in
			let _ = liste_message := {author = "System"; message = "Connected to subserver : "^(string_of_int servnumb)}::(!liste_message) in
			let clientid = Thread.id (Thread.create (client_fun) ic) in
			let img = Graphics.make_image (Array.make_matrix (hauteur_fenetre - y_separation - 1) largeur_fenetre Graphics.white) in
			let displayid = Thread.id (Thread.create (display_msgs) img) in
			(pids := [|clientid; displayid; Unix.getpid ()|] ; oc) ;
		end

	(* Ferme les fichiers et la fenêtre *)
	let f_end () =
		begin
			Graphics.close_graph () ;
			close_in file_in ;
			close_out file_out ;
			close_out memchat ;
			Close.killall !pids ;
		end

	let f_mouse x y = ()

	(* Gère les touches sur lesquelles appuie le jouer *)
	let f_key k oc = match k with
		| '\027' -> raise End
		| '\008' -> (Pyliste.remove message_ecrit (!carac_en_cours-1); carac_en_cours := max 0 (!carac_en_cours - 1))
		| '\127' -> (Pyliste.remove message_ecrit (!carac_en_cours); if !carac_en_cours >= Pyliste.taille message_ecrit then carac_en_cours := max 0 (!carac_en_cours - 1))
		| '\013' -> send_msg oc
		| x when x = Fleche.left -> carac_en_cours := max 0 (!carac_en_cours - 1)
		| x when x = Fleche.right -> carac_en_cours := min (message_ecrit.taille) (!carac_en_cours+1)
		| x -> if !carac_en_cours < taille_max_message then
			begin
				Pyliste.insert_lock message_ecrit !carac_en_cours x taille_max_message ;
				carac_en_cours := (!carac_en_cours + 1) ;
			end

	(* Lance les fonctions précédentes *)
	let start_chat () =
		try
			skel f_init f_end f_key f_mouse ;
		with _ -> Unix.sleepf 10.; exit 0

	(* Crée la fenêtre et lance le chat *)
	let func () =
    begin
      let window_id = Init.create_win Init.chat_x Init.chat_y "chat_A12Z46F_Cirno" "[CastleRain - Chat]" "chat.id" in
      let xpos = string_of_int ((Init.screen_x + (int_of_string Init.game_x) - (int_of_string Init.chat_x))/2) and ypos = string_of_int ((Init.screen_y - (int_of_string Init.chat_y))/2) in ignore (Unix.system("xdotool windowmove " ^ window_id ^ " " ^ xpos ^ " " ^ ypos)) ;
      start_chat () ;
    end
end ;;