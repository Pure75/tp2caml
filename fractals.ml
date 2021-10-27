#load "graphics.cma" ;;
open  Graphics  ;;
open Random ;;
open_graph " 1200 x800" ;;
clear_graph () ;;

(*Exercice 2.1.1*)

let draw_line (x,y) (z,t) =moveto x y ;lineto z t ;;

let rec montain n (x,y) (z,t) =
if n = 0 then
  begin
 moveto x  y ; lineto z t
  end
else
  begin
let a = ((x+z)/2) and b = ((y+t)/2) + Random.int(10 * (n+1))
in montain (n-1) (x , y) (a , b) ;
montain (n-1) (a , b) (z , t)
  end ;;

montain 5 (100,100) (150,100) ;;

(*Exercice 2.1.2*)

clear_graph () ;;

let rec dragon n (x,y) (z,t) =
if n = 0 then
  begin
moveto x y ; lineto z t
  end
else
  begin
 let u = ((x+z)/2 + (t-y)/2) and
 v = ((y+t)/2 - (z-x)/2)
  in dragon (n-1) (x,y) (u,v);
dragon (n-1) (z,t) (u,v) ;
  end ;;

dragon 19 (150,150) (350,350) ;;


(*Exercie 2.2.1*)

clear_graph ();;

let sponge (x,y) n =
  let rec sponge2 (x,y) n =
    let z = n/3 in
    if n = 0 then fill_rect x y z z
    else
      begin
        sponge2 (x,y) z;
        sponge2 ((x+n),y) z;
        sponge2 ((x+(2*n)),y) z;
        sponge2 ((x+(2*n)),(y+n)) z;
        sponge2 ((x+(2*n)),(y+(2*n))) z;
        sponge2 ((x+n),(y+(2*n))) z;
        sponge2 (x,(y+(2*n))) z;
        sponge2 (x,(y+n)) z;
      end
  in sponge2 (x,y) n ;;



sponge (10,10) 243;;



(*Exercice 2.2.2*)

clear_graph ();;

let triangle (x,y) n =
 let rec triangle2 (x,y) n =
 let z = n/2 in
if n = 0 then fill_rect x y z z
else
  begin
triangle2 (x,y) z ;
triangle2 ((x+(n)),y) z ;
triangle2 ((x+(n/2)),(y+n)) z;
  end
  in triangle2 (x,y) n ;;

triangle (10,10) 150;;


(*Exercice 2.3.1*)

clear_graph () ;;

let cercle (x,y) r =
  let rec cercle2 (x,y) r =
    let z = r/2 in
if z = 0 then ()
else
    begin
draw_circle x y r ;
cercle2 ((x+z),y) z ;
cercle2 ((x-z),y) z ;
    end
in cercle2 (x,y) r ;;


cercle (600,550) 200 ;;


(*Exercice 2.3.2*)

clear_graph () ;;

let fleche (x,y) r =
  let rec fleche2 (x,y) r z =
    let a = r/2 in
if a = 0 then ()
else
match z with
  |1 -> fill_circle x y r ; fleche2 ((x+r+a),y) a 3 ;
 fleche2 ((x-r-a),y) a 4 ; fleche2 (x,(y+r+a)) a 1 ;
  |2 -> fill_circle x y r ; fleche2 ((x+r+a),y) a 3 ;
 fleche2 ((x-r-a),y) a 4 ;fleche2 (x,(y-r-a)) a 2 ;
  |3 -> fill_circle x y r ; fleche2 (x,(y-r-a)) a 2 ;
 fleche2 (x,(y+r+a)) a 1 ; fleche2 ((x+r+a),y) a 3 ;
  |_ -> fill_circle x y r ; fleche2 (x,(y-r-a)) a 2 ;
 fleche2 (x,(y+r+a)) a 1 ; fleche2 ((x-r-a),y) a 4 ;
  in fleche2 (x,y) r 1 ;;

fleche (300,200) 75 ;;

clear_graph () ;;

(*Exercice 2.3.4*)

(*Version croix*)

clear_graph () ;;

let vicsekcross (x,y) n =
  let rec vicsek2 (x,y) n =
    let z = n/3 in
    if n = 0 then fill_rect x y z z
    else
      begin
        vicsek2 (x,y) z;
        vicsek2 ((x+n),(y+n)) z;
        vicsek2 ((x+(2*n)),y) z;
        vicsek2 ((x+(2*n)),(y+(2*n))) z;
        vicsek2 (x,y+(2*n)) z ;
      end
  in vicsek2 (x,y) n ;;

vicsekcross (100,100) 100 ;;

(*Version etoile*)


clear_graph () ;;

let vicsekstar (x,y) n =
  let rec vicsek2 (x,y) n =
    let z = n/3 in
    if n = 0 then fill_rect x y z z
    else
      begin
        vicsek2 (x+n,y) z;
        vicsek2 ((x+n),(y+n)) z;
        vicsek2 ((x+(2*n)),y+n) z;
        vicsek2 ((x+n),(y+(2*n))) z;
        vicsek2 (x,y+n) z ;
      end
  in vicsek2 (x,y) n ;;

vicsekstar (100,100) 100 ;;

