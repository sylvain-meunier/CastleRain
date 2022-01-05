(* Ce module contient des fonctions utiles pour manipuler des tableaux dynamiques se rapprochant des liste Python *)

module Pyliste =
struct
	type 'a pyliste = {mutable mem : 'a array; mutable taille : int; ele : 'a}
	(* Ajoute un élément *)
	let add p ele =
		begin
			(if p.taille >= Array.length p.mem then
					let newarray = Array.make (p.taille*2) p.ele in
						(for i=0 to p.taille - 1 do
							newarray.(i) <- p.mem.(i);
						done;
						p.mem <- newarray));
			p.mem.(p.taille) <- ele;
			p.taille <- p.taille + 1;
		end ;
	(* Supprime un élément *)
	and remove p ind =
		if ind >= 0 && p.taille > 0 then
			begin
				for i=ind+1 to p.taille-1 do
					p.mem.(i-1) <- p.mem.(i)
				done;
				p.taille <- p.taille - 1;
			end
	(* Affecte un élément à un indice *)
	and set p ind ele = p.mem.(ind) <- ele
	(* Renvoie un élément *)
	and get p ind = p.mem.(ind)
	(* Renvoie la taille de la liste *)
	and taille p = p.taille
	(* Crée une liste *)
	and new_pyliste ele = {mem = (Array.make 1 ele); taille = 0; ele = ele}
	(* Insère un élément *)
	let insert p ind ele =
		if ind >= 0 then
			begin
				if p.taille > 0 then add p (p.mem.(p.taille-1))
				else add p ele;
				for i=p.taille-1 downto ind+1 do
					p.mem.(i) <- p.mem.(i-1)
				done;
				set p ind ele;
			end ;
	(* Ajoute un élément, sauf si le tableau dépasse une certaine taille *)
	and add_lock p ele taille =
		if p.taille <= taille then add p ele
		else ()
	(* Vide le tableau *)
	and empty p =
		while p.taille > 0 do
			remove p 0 ;
		done
	(* Insère un élément, sauf si le tableau dépasse une certaine taille *)
	let insert_lock p ind ele taille_max =
		if ind >= 0 then
			begin
				if p.taille > 0 then add_lock p (p.mem.(p.taille-1)) taille_max
				else add_lock p ele taille_max;
				for i = min (p.taille-1) taille_max downto ind+1 do
					p.mem.(i) <- p.mem.(i-1)
				done;
				set p ind ele;
			end
end ;;