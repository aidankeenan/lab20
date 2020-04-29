type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;
open List ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  map  (fun row -> map (fun pixel -> if pixel <= threshold then 0. else 1.)
                                 row) img


       
(* show the image *)
let depict img =
  open_graph ""; clear_graph ();
  let width, height = length (hd img), length img in resize_window width height;
  let depict_pix intensity row color = let lvl = int_of_float (255. *. (1. -. intensity)) in set_color (rgb lvl lvl lvl);
  plot color (height - row) in
  iteri (fun r row -> iteri (fun color pix -> depict_pix pix r color) row) img;
  Unix.sleep 2; close_graph () ;;

(* dither max image -- dithered image *)
let dither img =
  map
    (fun row ->
     map
       (fun pixel -> if pixel > Random.float 1.
                 then 1.
                 else 0.) row)
    img
    
let mona = Monalisa.image ;;
let mona_threshold = threshold mona 0.75 ;;   
let mona_dither = dither mona ;;

depict mona ;;
depict mona_threshold ;;
depict mona_dither ;;
           

           
