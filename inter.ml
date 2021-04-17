#load "graphics.cma";;
open Graphics;;

(**@author Hugo Bernedo*)



type relief = Top | Bot | Flat;;

type box_config =
  {x : int; y:int; w:int; h:int; bw : int; mutable r :relief;
   b1_col : Graphics.color;
   b2_col : Graphics.color;
   b_col  : Graphics.color};;

let draw_rect x0 y0 w h =
  let (a,b) = Graphics.current_point ()
  and x1 = x0+w and y1 = y0+h
  in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1; Graphics.lineto x1 y1;
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  Graphics.moveto a b;;

let draw_box_outline bcf col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h;;

let draw_box bcf =
  let x1= bcf.x and y1 = bcf.y in
  let x2 = x1 + bcf.w and y2 = y1 + bcf.h in
  let ix1 = x1 + bcf.bw and ix2 = x2-bcf.bw
      and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [| (x1,y1);(ix1,iy1);(ix2,iy2);(ix2,iy2);(x2,y2);(x2,y1)|]
  in
  let border2 g =
    Graphics.set_color g;
    Graphics.fill_poly
      [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2)|]
  in
  Graphics.set_color bcf.b_col;
  (match bcf.r with
     Top ->
      Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
      border1 bcf.b1_col;
      border2 bcf.b2_col
   | Bot ->
        Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
      border1 bcf.b1_col;
      border2 bcf.b2_col
   | Flat ->
      Graphics.fill_rect x1 y1 bcf.w bcf.h);
  draw_box_outline bcf Graphics.black;;


let erase_box bcf =
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect (bcf.x + bcf.bw) (bcf.y + bcf.bw)
    (bcf.w - (2*bcf.bw)) (bcf.h - (2* bcf.bw));;

type position = Left | Center | Right;;

let draw_string_in_box pos str bcf col =
  let (w,h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h-h)/2 in
  (match pos with
   | Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty
   | Right  -> let tx = bcf.x + bcf.w -w - bcf.bw -1 in Graphics.moveto tx ty
   | Left -> let tx = bcf.x + bcf.bw +1 in Graphics.moveto tx ty );
  Graphics.set_color col ;
  Graphics.draw_string str;;


(* Exemple de jeu : *) 

let set_gray x = (Graphics.rgb x x x) ;;

let gray1 = set_gray 100 and gray2 = set_gray 170 and gray3 = set_gray 240;;


let rec create_grid nb_col n sep b =
  if n < 0 then []
  else
    let px = n mod nb_col and py = n /nb_col in
    let nx = b.x + sep + px*(b.w+sep)
    and ny = b.y + sep + py*(b.h+sep) in (
    let b1 = {b with x = nx; y = ny} in
    b1 :: (create_grid nb_col (n-1) sep b ));;


let vb =
  let b = { x=0 ; y=0 ; w = 40 ; h = 40 ; bw = 4;
            b1_col = gray1 ; b2_col = gray3 ; b_col = gray2 ; r = Top} in
  Array.of_list (create_grid 5 24 2 b ) ;;



let draw () =
  Graphics.moveto 0 600;
  Graphics.lineto 600 700 ;;

let () =
  Graphics.open_graph "";
  Graphics.resize_window 400 400;
 
Array.iter draw_box vb ;
draw_string_in_box Center "X" vb.(5) Graphics.black;
draw_string_in_box Center "X" vb.(8) Graphics.black;
draw_string_in_box Center "0" vb.(12) Graphics.yellow;
draw_string_in_box Center "0" vb.(11) Graphics.yellow;

(*  ignore (Graphics.read_key())*) ;;
