#load "unix.cma";;

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
	let init () = allow_arrow_press ()
end ;;

module Fleche =
struct
	let left = Char.chr 240 and right = Char.chr 241 and up = Char.chr 242 and down = Char.chr 243
end ;;

module Close =
struct
	let close () =
		begin
			ignore (Unix.system "xmodmap ./touchmem.xmm");
			exit 0;
		end
end ;;