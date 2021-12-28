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
	and set p ele ind = p.mem.(ind) <- ele
	and get p ind = p.mem.(ind)
	and new_pyliste ele = {mem = (Array.make 1 ele); taille = 0; ele = ele}
end ;;