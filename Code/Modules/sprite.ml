module Sprite =
struct
  let read path = let file = open_in path and tab = ref (Array.make_matrix 0 0 0) in
	  	try
		  	let ligne = ref (input_line file) and i = ref 0 and j = ref 0 in
			  let x, y = Scanf.sscanf (!ligne) "%d %d" (fun x y -> x, y) in
				  begin
					  tab := Array.make_matrix x y 0;
					  while true do
						  ligne := input_line file;
						  !tab.(!i).(!j) <- (int_of_string (List.hd (String.split_on_char '\r' !ligne)));
  						j := !j + 1;
  						if !j >= y then
  							begin
  								j := 0;
  								i := !i + 1;
  							end
  					done;
  					!tab
  				end
  		with End_of_file -> !tab

end ;;