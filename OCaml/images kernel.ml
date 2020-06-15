let foi = float_of_int and
	iof = int_of_float;;

#load "graphics.cma";;
#load "Str.cma";;

#load "Unix.cma" ;;

let time_test time callback =
	let start = Unix.gettimeofday () and
		turn = ref 0 in
	while Unix.gettimeofday () < start +. time do
		callback ();
		incr turn
	done;
	print_string "\npendant ";
	print_int (iof time);
	print_string "s :\n °";
	print_int !turn;
	print_string " tours\nen moyenne : ";
	print_float (foi !turn /. time);; 

let dure_test callback = 
	let start = Unix.gettimeofday () in
	callback ();
	let dure = Unix.gettimeofday () -. start in
	print_string "le programe a tourné pendant ";
	print_float dure;
	print_string " s";;

let switch a b = 
	(*echange 2 referance*)
	let temp = !a in
	a := !b;
	b := temp;;

let read_file file = 
	(*renvoit le contenu d'un ficher donner en argument (chemin) sous forme de 'string'*)
	let txt = ref "" in
	let chanel = open_in file in
	try
		while true do
    		txt := !txt^"\n"^input_line chanel
  		done;
  		!txt
	with End_of_file ->
		close_in chanel;
		!txt ;;

let get_writing_function_file file = 
	(*renvoit une fonction permettant d'écrire dans le ficher sont le chemin est donner en argument ainsi qu'une fonction permetant de fermer le stream ouvert pour l'occasion*)
	let chanel = open_out file in
	let write txt =
		output_string chanel (txt^"\n");
	and
	end_writing () =
		close_out chanel;
	in write, end_writing;;

let read_ppm file = 
	let ppm = read_file file and
		sepLines = Str.regexp "\n" and
		sepChara = Str.regexp " " in
	let linesFile = ref (Str.split sepLines ppm) in
	let h::t = !linesFile in
	linesFile := t;
	let [_; lines; columns; ma] = Str.split sepChara h in
	Graphics.open_graph (lines^"x"^columns);
	Graphics.auto_synchronize false;
	let l = int_of_string lines and
		c = int_of_string columns in
	for i = 0 to l - 1 do
		let l::t = !linesFile in
		linesFile := t;
		let line = ref (Str.split sepChara l) in
		for j = 0 to c - 1 do
			let r::g::b::t = !line in
			line := t;
			Graphics.set_color (Graphics.rgb (int_of_string r) (int_of_string g) (int_of_string b));
			Graphics.plot i j;
		done
	done;
	Graphics.synchronize ();;


(*gestion des couleur*)
type color = {r : float; g : float; b : float};;
let copie_color c = {r = c.r; g = c.g; b = c.b};;
let create_color r g b = {r = foi (r*r); g = foi (g*g); b = foi (b*b)};; (*On stoque les couleur au carré pour que les melange de couleur soit plus agréable*)
let string_of_color c = (string_of_int (iof (c.r**0.5)))^" "^(string_of_int (iof (c.g**0.5)))^" "^(string_of_int (iof (c.b**0.5)))^" ";;
let get_Graphics_color c = Graphics.rgb (iof (c.r**0.5)) (iof (c.g**0.5)) (iof (c.b**0.5));;
let black = create_color 0 0 0 and
	white = create_color 255 255 255 and
	green = create_color 0 255 0 and
	red = create_color 255 0 0 and
	blue = create_color 0 0 255 and
	gray = create_color 128 128 128;;


let mix_color c1 c2 s = 
	(*permet de créé une nouvellle couleur en faisant un melange pondéré de deux coleurs*)
	let r = c1.r *. s +. (1. -. s) *. c2.r and
		g = c1.g *. s +. (1. -. s) *. c2.g and
		b = c1.b *. s +. (1. -. s) *. c2.b in
	{r = r; g = g; b = b};;

(* let plot a b = Graphics.fill_rect (10*a) (10*b) (10*a + 10) (10*b + 10);;
 *)
class imageBuffer line column background = object (self)
	(*buffer d'image, permetant de créé pas à pas une image et de l'exporter au format ppm*)
	val buffer = Array.make_matrix line column background
	val mutable background = background
	val line = line
	val column = column

	method clear () =
		for i = 0 to line - 1 do
			for j = 0 to column - 1 do
			buffer.(i).(j) <- background
		done
	done
	method draw_pixel x y color = 
		if 0<=x && x<line && 0<=y && y<column then buffer.(x).(y) <- color

	method private middle_point_algorithm x1 y1 x2 y2 =
		(*algoritme du point milieu*)
		let x1 = ref x1 and	y1 = ref y1 and	x2 = ref x2 and	y2 = ref y2 in
		if !y1 > !y2 then begin
			(*afin de réduire les diferent cas a traiter, onn  se place toujours dans le cas ou le segment "monte"*)
			switch y1 y2; switch x1 x2
		end;
		let dx = abs (!x2 - !x1) and
			dy = !y2 - !y1 in

		let dp = ref (2 * dy - dx) and
			sE = ref (2 * dy) and
			sNE = ref (2 * (dy - dx)) and
			x = ref !x1 and
			y = ref !y1 in
		let notOver = ref (!x != !x2 || !y != !y2) and
			incrx = if !x1 <= !x2 then incr else decr in (*definit la croissance ou la decroissance des x*)

		let incrE  = ref (fun () -> dp := !dp + !sE; incrx x) and
			incrNE = ref (fun () -> dp := !dp + !sNE; incrx x; incr y) in

		if dx < dy then begin (*si la valeur absolue de la pente est superieur a 1, on modifie les constante pour faire fonctionner l'algoritme*)
			dp := 2 * dx - dy;
			sE := 2 * dx;
			sNE := 2 * (dx - dy);
			incrE := (fun () -> dp := !dp + !sE; incr y)
		end;

		(*On renvoit le prechain pixel sur le segment*)
		let next () = 
			if !dp < 0 then !incrE ()
			else !incrNE ();
			notOver := !x != !x2 || !y != !y2
		in next, x, y, notOver

	method draw_line x1 y1 x2 y2 color = 
		let next, x, y, notOver = self#middle_point_algorithm x1 y1 x2 y2 in
		self#draw_pixel !x !y color;
		while !notOver do
			next ();
			self#draw_pixel !x !y color
		done;

	method draw_line_gradient x1 y1 x2 y2 c1 c2 = 
		let next, x, y, notOver = self#middle_point_algorithm x1 y1 x2 y2 in
		let c1 = ref c1 and c2 = ref c2 in
		if !x != x1 || !y != y1 then switch c1 c2;
		let dx = abs (x2 - x1) and
			dy = abs (y2 - y1) in
		let s = ref 1. and
			ds = 1. /. (foi (max dx dy)) in

		self#draw_pixel !x !y (mix_color !c1 !c2 !s);
		while !notOver do
			next ();
			s := !s -. ds;
			self#draw_pixel !x !y (mix_color !c1 !c2 !s)
		done

	method draw_triangle x1 y1 x2 y2 x3 y3 color = 
		self#draw_line x1 y1 x2 y2 color;
		self#draw_line x2 y2 x3 y3 color;
		self#draw_line x3 y3 x1 y1 color

	method fill_triangle x1 y1 x2 y2 x3 y3 color = 
		let x1 = ref x1 and y1 = ref y1 and x2 = ref x2 and y2 = ref y2 and x3 = ref x3 and y3 = ref y3 in
		if !y2 >= !y3 then begin switch y3 y2; switch x3 x2 end;
		if !y1 >= !y3 then begin switch y1 y3; switch x1 x3 end;
		if !y1 >= !y2 then begin switch y1 y2; switch x1 x2 end;
		(*on a alors y1 <= y2 <= y3*)
		let nextLong, xL, yL, notOverL = self#middle_point_algorithm !x1 !y1 !x3 !y3 and
			n1S, x1S, y1S, notOver1S   = self#middle_point_algorithm !x1 !y1 !x2 !y2 and
			n2S, x2S, y2S, notOver2S   = self#middle_point_algorithm !x2 !y2 !x3 !y3 in
		let next = [|n1S; n2S|] and x = [|x1S; x2S|] and y = [|y1S; y2S|] and notOver = [|notOver1S; notOver2S|] and
			mainY = ref (!y1 + 1) and i = ref 0 in
		
		self#draw_pixel !xL !yL color;
		while !notOverL do
			(*On calcule les point successif des deux segement jusqu'a ce que le point afficher soit sur la ligne juste au dessus *)
			while !yL != !mainY && !notOverL do
				nextLong ();
				self#draw_pixel !xL !yL color
			done;
			while !(y.(!i)) != !mainY && !(notOver.(1)) do
				if not !(notOver.(!i)) then i := 1;
				next.(!i) ();
				self#draw_pixel !(x.(!i)) !(y.(!i)) color;
			done;

			for z = min !xL !(x.(!i)) + 1 to max !xL !(x.(!i)) - 1 do
				self#draw_pixel z !mainY color
			done;
			incr mainY
		done

	method fill_triangle_gradient x1 y1 x2 y2 x3 y3 c1 c2 c3 = 
		let x1 = ref x1 and y1 = ref y1 and x2 = ref x2 and y2 = ref y2 and x3 = ref x3 and y3 = ref y3 and c1 = ref c1 and c2 = ref c2 and c3 = ref c3 in
		if !y2 > !y3 then begin switch y3 y2; switch x3 x2; switch c3 c2 end;
		if !y1 > !y3 then begin switch y1 y3; switch x1 x3; switch c1 c3 end;
		if !y1 > !y2 then begin switch y1 y2; switch x1 x2; switch c1 c2 end;
		(*on a alors y1 <= y2 <= y3*)

		let nextLong, xL, yL, notOverL = self#middle_point_algorithm !x1 !y1 !x3 !y3 and
			n1S, x1S, y1S, notOver1S   = self#middle_point_algorithm !x1 !y1 !x2 !y2 and
			n2S, x2S, y2S, notOver2S   = self#middle_point_algorithm !x2 !y2 !x3 !y3 in
		let next = [|n1S; n2S|] and x = [|x1S; x2S|] and y = [|y1S; y2S|] and notOver = [|notOver1S; notOver2S|] and c = [|[|c1; c2|]; [|c2; c3|]|] and
			mainY = ref (!y1 + 1) and i = ref 0 in

		(*On calcul les distances pour effectuer les interpolation lineaire*)
		let dx12 = abs (!x2 - !x1) and dy12 = abs (!y2 - !y1) in
		let ds12 = 1. /. (foi ( max dx12 dy12)) in
		let dx23 = abs (!x2 - !x3) and dy23 = abs (!y2 - !y3) in
		let ds23 = 1. /. (foi (max dx23 dy23)) in
		let dx31 = abs (!x3 - !x1) and dy31 = abs (!y3 - !y1) in
		let s31 = ref 1. and ds31 = 1. /. (foi (max dx31 dy31)) in

		let longColor = ref !c1 and littleColor = ref !c1 and s = ref 1. and ds = [|ds12; ds23|] in
		(*On initialise les couleur a gauche et a droite du segment horizontal que l'on vas tracer*)

		self#draw_pixel !xL !yL !longColor;
		while !notOverL do

			while !yL != !mainY && !notOverL do
				nextLong ();
				s31 := !s31 -. ds31;
				longColor := mix_color !c1 !c3 !s31;
				self#draw_pixel !xL !yL !longColor
			done;
			while !(y.(!i)) != !mainY && !(notOver.(1)) do
				if not !(notOver.(!i)) && !i != 1 then begin 
					i := 1;
					s := 1.
				end;
				next.(!i) ();
				s := !s -. ds.(!i);
				littleColor := mix_color !(c.(!i).(0)) !(c.(!i).(1)) !s;
				self#draw_pixel !(x.(!i)) !(y.(!i)) !littleColor;
			done;

			let tempS = ref 1. and tempDs = 1. /. foi (abs (!xL - !(x.(!i)))) in
			let tempC = if !xL < !(x.(!i)) then [|!longColor; !littleColor|] else [|!littleColor; !longColor|] in
			
			for z = min !xL !(x.(!i)) + 1 to max !xL !(x.(!i)) - 1 do
				tempS := !tempS -. tempDs;
				self#draw_pixel z !mainY (mix_color tempC.(0) tempC.(1) !tempS)
			done;
			incr mainY
		done

	method draw_circle x y r color = 
		let x_ = ref 0 and y_ = ref r and d = ref r in
		(* self#draw_pixel (x + r) y color;
		self#draw_pixel (x - r) y color; *)

		while !y_ != 0 do
		if !d = r then begin
			self#draw_pixel (x + !x_) (y + !y_) color;
			self#draw_pixel (x + !y_) (y - !x_) color;
			self#draw_pixel (x - !x_) (y - !y_) color;
			self#draw_pixel (x - !y_) (y + !x_) color
		end;
		if !d = r then incr x_;
		if !d < r then incr x_;
		if !d > r then begin decr y_; decr x_ end;
		d := iof ((foi (!x_ * !x_  +  !y_ * !y_))**0.5);
		done

	method fill_circle x y r color = 
		let x_ = ref 0 and y_ = ref r and d = ref r in

		self#draw_pixel x y color;
		while !y_ != 0 do
		if !d = r then begin
			for z = 0 to !x_ do
			self#draw_pixel (x + z) (y + !y_) color;
			self#draw_pixel (x + !y_) (y - z) color;
			self#draw_pixel (x - z) (y - !y_) color;
			self#draw_pixel (x - !y_) (y + z) color
		done;
		end;
		if !d = r then incr x_;
		if !d < r then incr x_;
		if !d > r then begin decr y_; decr x_ end;
		d := iof ((foi (!x_ * !x_  +  !y_ * !y_))**0.5);
		done


	method set_background color = background <- color

	method show_image () = 
		Graphics.open_graph ((string_of_int line)^"x"^(string_of_int column));
		Graphics.auto_synchronize false;
		for i = 0 to line - 1 do
			for j = 0 to column - 1 do
				Graphics.set_color (get_Graphics_color buffer.(i).(j));
				Graphics.plot i j
			done
		done;
		Graphics.synchronize ()

	method export_to_ppm file = 
		let write, end_writing = get_writing_function_file file in
		write ("P3 "^(string_of_int line)^" "^(string_of_int column)^" 255");
		let txt = ref "" in
		for j = column - 1 downto 0 do
			txt := "";
			for i = 0 to line - 1 do
				txt := !txt ^ string_of_color buffer.(i).(j)
			done;
			write !txt
		done;
		end_writing ()
end;;

let test = new imageBuffer 1000 1000 black;;
(* test#draw_line_gradient 50 50 70 60 white green;;
test#draw_line_gradient 50 50 70 80 white red;;
test#draw_line_gradient 50 50 30 80 white blue;;
test#draw_line_gradient 50 50 30 60 white gray;;
test#draw_line_gradient 50 50 70 40 white green;;
test#draw_line_gradient 50 50 70 20 white red;;
test#draw_line_gradient 50 50 30 20 white blue;;
test#draw_line_gradient 50 50 30 40 white gray;; *)
test#fill_triangle_gradient 1000 1000 0 1000 0 0 green blue green;;
test#fill_triangle_gradient 1000 1000 1000 0 0 0 green red green;;
(* test#fill_triangle_gradient 800 600 500 1000 10 50 blue red green;;
 *)(* test#draw_triangle 499 499 298 10 1 1 white;; *)
(* test#draw_line 499 499 1 1 green; *)
(* test#fill_triangle 400 200 300 200 100 200 red;; *)
(* test#draw_line 0 0 10 298 white;;
test#draw_line 499 499 0 0 white;;
test#draw_line 10 298 499 499 white;; *)
(* test#draw_line 0 0 10 10 red;; *)
(* test#fill_circle 250 250 50 green;;
test#draw_circle 250 250 50 red;; *)

(* test#show_image ();; *)
test#export_to_ppm "C:/Users/pierre/Desktop/programation/Caml/TIPE/square.ppm";;

(* test#export_to_ppm "C:/Users/pierre/Desktop/programation/Caml/TIPE/triangle.ppm";; *)


(*(*  test#export_to_ppm "C:/Users/pierre/Desktop/programation/Caml/TIPE/line.ppm";;
read_ppm "C:/Users/pierre/Desktop/programation/Caml/TIPE/line.ppm";; *) *)
(* 
Graphics.open_graph "1000x500";;
Graphics.auto_synchronize false;;
Graphics.moveto 0 0;;
Graphics.set_line_width 0;;
let temp = new imageBuffer 1000 500 black;;

let paint () =
	for i = 0 to 999 do
		for j = 0 to 499 do
			Graphics.plot i j
		done
	done;;

let draw () =
	Graphics.fill_rect 0 0 999 499;;

let line_Graphics () = 
	Graphics.draw_segments [|(0, 0, 999, 499)|];;
let line_iamgeBuffer () = 
	temp#draw_line 0 0 999 499;; *)

(* dure_test paint;;
dure_test (fun () -> temp#clear ());; *)

(* time_test 5. line_Graphics;;
time_test 5. line_iamgeBuffer;;
 *)