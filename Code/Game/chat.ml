#use "topfind";;
#load "unix.cma" ;;
#directory "+threads";;
#load "threads.cma";;
#require "graphics";;

#use "../Modules/pyliste.ml" ;;
#use "../Modules/castlerain.ml" ;;

Init.init () ;;

Unix.system "cat ./memchat.out >chat.tsin" ;;

let file_in = open_in "./chat.tsin" and file_out = open_out "./chat.tsout" and memchat = open_out "./memchat.out";;

let author = "J1";;

let largeur_fenetre = 400;;
let hauteur_fenetre = 400;;

let taille_txt = 20;;
let x_min_txt = 15;;
let y_min_txt = 25;;
let y_separation = 50;;

let nb_carac_longueur = 50;;
let taille_max_message = 210;;

let message_ecrit = Pyliste.new_pyliste ' ' ;;
let carac_en_cours = ref 0;;

type message = {author : string; message : string} ;;
let liste_message = ref [{author = "System" ; message = "Bienvenue sur le chat !"}] ;;

let nb_carac_message msg = (String.length msg.author) + 1 + (String.length msg.message);;

let charArray_to_String tableau = 
	String.init (Pyliste.taille tableau) (Pyliste.get tableau) ;;

let retour_ligne word = let dx, dy = Graphics.text_size word in Graphics.moveto x_min_txt (Graphics.current_y () - dy);;

let open_chat () =
	Graphics.open_graph (" "^(string_of_int largeur_fenetre)^"x"^(string_of_int hauteur_fenetre));
	Graphics.set_text_size taille_txt;;

let send_msg () =
	begin
		output_string file_out ((author^"|"^(charArray_to_String message_ecrit))^"\n");
		flush file_out;
		carac_en_cours := 0;
		Pyliste.empty message_ecrit;
	end ;;

let full_msg msg liste =
	let rec aux liste mem = match liste with
		| [] -> mem
		| a::b -> aux b (mem^"|"^a)
	in
	aux liste msg ;;

let clien_fun ic = 
		while true do 
			try 
				let r = input_line ic in
				try
					output_string memchat r;
					flush memchat;
					let a::m::r = String.split_on_char '|' r in liste_message := ({author = a; message = full_msg m r} :: !liste_message);
				with _ -> Printf.printf "Erreur : %s %!" r
			with End_of_file -> () ;
			Unix.sleepf 0.3 ;
		done ;;

let cut_word text =
	try
		let w::garb = String.split_on_char ' ' text in w, garb
	with _ -> text, [] ;;

let draw_by_char word = 
	for i = 0 to (String.length word - 1) do
		if (largeur_fenetre - (Graphics.current_x())) <= 20 then retour_ligne word ;
		Graphics.draw_char word.[i];
	done ;;

let show_msg_on_screen word = let dx, dy = Graphics.text_size word in
		begin
			if (Graphics.current_x()) + dx + x_min_txt >= largeur_fenetre then retour_ligne word ;
			if (Graphics.current_x()) + dx + x_min_txt >= largeur_fenetre then draw_by_char word
			else Graphics.draw_string word
		end ;;

let rec show_msg l1 l2 = match l1, l2 with
| [], [] -> ()
| liste, a::b -> (show_msg_on_screen (a^" "); show_msg liste b)
| a::b, [] -> let word, gar = cut_word a in (show_msg_on_screen (word^" "); show_msg b gar) ;;

let display_msgs img =
	while true do
		Graphics.draw_image img 0 y_separation ;
		Graphics.draw_segments [|(0, y_separation, largeur_fenetre, y_separation)|] ;
		let dx, dy = Graphics.text_size ((List.hd(!liste_message)).message) in Graphics.moveto x_min_txt (hauteur_fenetre - y_min_txt - dy) ;
		let rec aux list_msg = match list_msg with
			| [] -> ()
			| x::q when Graphics.current_y() <= 10 -> aux q
			| x::q ->
				begin
					show_msg [x.author^": "^x.message] [] ;
					if q <> [] then let dx, dy = Graphics.text_size ((List.hd(q)).message) in Graphics.rmoveto (x_min_txt - (Graphics.current_x ())) ( -dy - y_min_txt);
					aux q;
				end
		in aux (List.rev !liste_message) ;
		Unix.sleepf 0.8 ;
	done ;;

let	display_msgs_fun img = 
	while true do
		Unix.sleepf 0.2 ;
		display_msgs img ;
	done ;;

let display_written img =
	let _ = Graphics.draw_image img 0 0, Graphics.moveto x_min_txt (y_separation - y_min_txt) in
		begin
			show_msg [charArray_to_String message_ecrit] [] ;
		end ;;


exception End;;

let skel f_init f_end f_key f_mouse =
	f_init ();
	let img = Graphics.make_image (Array.make_matrix (y_separation-2) largeur_fenetre Graphics.white) in
	try
		while true do
			try
				display_written img ;
				let s = Graphics.wait_next_event
					[Graphics.Button_down; Graphics.Key_pressed]
				in if s.Graphics.keypressed then f_key s.Graphics.key
					else if s.Graphics.button
						then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
			with
				End -> raise End
			done
	with
		End -> f_end ();;

let f_init () =
	begin
		open_chat () ;
		Graphics.set_color Graphics.black ;
		ignore (Thread.create (clien_fun) file_in) ;
		let img = Graphics.make_image (Array.make_matrix (hauteur_fenetre - y_separation - 1) largeur_fenetre Graphics.white) in
		ignore (Thread.create (display_msgs_fun) img) ;
	end ;;

let f_end () =
	begin
		Graphics.close_graph () ;
		close_in file_in ;
		close_out file_out ;
		close_out memchat ;
		Close.close () ;
	end ;;

let f_mouse x y = ();;
let f_key k = match k with
	| '\027' -> raise End
	| '\008' -> (Pyliste.remove message_ecrit !carac_en_cours; carac_en_cours := max 0 (!carac_en_cours - 1))
	| '\013' -> send_msg ()
	| x when x = Fleche.left -> carac_en_cours := max 0 (!carac_en_cours - 1)
	| x when x = Fleche.right -> carac_en_cours := min (message_ecrit.taille) (!carac_en_cours+1)
	| x -> if !carac_en_cours < taille_max_message then
		begin
			Pyliste.insert_lock message_ecrit !carac_en_cours x taille_max_message ;
			carac_en_cours := (!carac_en_cours + 1) ;
		end ;;

skel f_init f_end f_key f_mouse;;