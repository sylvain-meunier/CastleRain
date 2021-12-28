module Pyliste =
struct
	type 'a pyliste = {mutable mem : 'a array; mutable taille : int; ele : 'a}
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
	and remove p ind =
		if ind >= 0 && p.taille > 0 then
			begin
				for i=ind+1 to p.taille-1 do
					p.mem.(i-1) <- p.mem.(i)
				done;
				p.taille <- p.taille - 1;
			end ;
	and set p ind ele = p.mem.(ind) <- ele
	and get p ind = p.mem.(ind)
	and new_pyliste ele = {mem = (Array.make 1 ele); taille = 0; ele = ele}
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
end ;;