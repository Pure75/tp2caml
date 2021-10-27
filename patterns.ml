(*Exercice 1*)

(*1.1.1*)
let rec build_line x str =
  if x = 0 then "" else
    build_line (x-1) str ^ str ;;

build_line 5 "*" ;;

(*1.1.2*)

let rec square x str =
  if x = 0 then print_string "" else
    let rec square2 y = if y = 0 then print_string "" else
        begin
          print_string(build_line x str) ; print_string "\n" ; square2 (y-1)
        end
    in square2 x     ;;

square 5 "*" ;;

(*1.1.3*)


let rec build_line2 x (y,z) =
  if x = 0 then "" else
    build_line2 (x-1) (y,z) ^ y ^ z ;;

let rec build_line3 x (z,y) =
  if x = 0 then "" else
    build_line3 (x-1) (z,y) ^ z ^ y ;;

let square2 x (y,z) =
  if x = 0 then print_string""
  else let rec square3 x w =
         if x = 0 then print_string"" else
         if x mod 2 = 0 then
           begin
             print_string(build_line2 w (y,z)) ; print_string "\n" ;
             square3 (x-1) w
           end
         else
           begin
             print_string(build_line3 w (z,y)) ; print_string "\n" ;
             square3 (x-1) w
           end
    in square3 x x ;;

square2 5 ("*",".") ;;


(*1.1.4*)

let rec triangle x str =
  if x = 0 then print_string "" else
    let rec triangle2 y = if y = x+1 then print_string "" else
        begin
          print_string(build_line y str) ; print_string "\n" ; triangle2 (y+1)
        end
    in triangle2 1  ;;


triangle 5 "*" ;;



(*1.2.1*)


let pyramid x (y , z) =
  if x = 0 then print_string ""
  else
    let rec pyramid2 w v =
      if w+1 = 0 then print_string ""
      else
        begin
          print_string(build_line w y) ; print_string(build_line v z) ;
          print_string(build_line w y) ; print_string("\n") ;
          pyramid2 (w-1) (v+2)
        end
    in (pyramid2 (x-1) 2) ;;

pyramid 5 (".","*") ;;

(*1.2.2*)

let cross x (y , z) =
  if x = 0 then print_string""
  else
    let rec crossbis a b c =
      if c = -1 then print_string ""
      else
	begin
	  print_string(build_line a y) ; print_string(build_line b z) ;
	  print_string(build_line c y) ; print_string(build_line b z) ;
	  print_string(build_line a y) ; print_string("\n") ;
	  crossbis (a+1) b (c-2)
	end
    in  crossbis 0 1 ((2*x)-3);
    print_string(build_line (x-1) y) ; print_string(build_line 1 z) ;
    print_string(build_line (x-1) y) ; print_string("\n") ;
    let rec crossbis2 a b c d e =
      if c = (2*x)-1 then print_string ""
      else
	begin
	  print_string(build_line a y) ; print_string(build_line b z) ;
	  print_string(build_line c y) ; print_string(build_line d z) ;
	  print_string(build_line e y) ; print_string("\n") ;
	  crossbis2 (a-1) b (c+2) d (e-1)
	end
    in crossbis2 (x-2) 1 1 1 (x-2) ;;

cross 5 ("." , "&") ;;
cross 18("." , "&") ;;
