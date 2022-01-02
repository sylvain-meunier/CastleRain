#load "unix.cma" ;;
#require "graphics" ;;

module Init =
struct
	let allow_arrow_press () =
		begin
		ignore (Unix.system "xmodmap -pke > ./touchmem.xmm");
		ignore (Unix.system "xmodmap -e \"keysym  Left = 0xf0\" ");
		ignore (Unix.system "xmodmap -e \"keysym  Right = 0xf1\"");
		ignore (Unix.system "xmodmap -e \"keysym  Up = 0xf2\"");
		ignore (Unix.system "xmodmap -e \"keysym  Down = 0xf3\"");
		end
	
	let empty_file () = ignore (Unix.system ">chat.tsin >game.tsin >dial.tsin >chat.tsout >game.tsout >dial.tsout")
	let init () =
		begin
			allow_arrow_press () ;
			empty_file () ;
		end
end ;;

module Fleche =
struct
	let left = Char.chr 240 and right = Char.chr 241 and up = Char.chr 242 and down = Char.chr 243
	let left_maj = Char.uppercase_ascii left and rigth_maj = Char.uppercase_ascii right and down_maj = Char.uppercase_ascii down and up_maj = Char.uppercase_ascii up
end ;;

module Close =
struct
	let close () =
		begin
			ignore (Unix.system "xmodmap ./touchmem.xmm");
			exit 0;
		end
end ;;