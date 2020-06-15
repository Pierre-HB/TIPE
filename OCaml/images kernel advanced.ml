let foi = float_of_int and
	iof = int_of_float;;

#load "graphics.cma";;
#load "Str.cma";;
#load "Unix.cma" ;;
#load "Threads.cma";;


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

type record = {startTime : float; fileDirectory : string; fileName : string; nextName : unit -> string; currentNumberFiles : int ref; x : int; y : int};;

let create_ppm_names root fileNames x y = 
	(*On crée l'integeur permettant d'avoir tout les droit sur le fichier :*)
	(*voir doc (partie 2.3): http://gallium.inria.fr/~remy/camlunix/cours.html *)
	let d = ref 0 and file = root^"/"^fileNames in
	for i = 0 to 3 do
		for j = 0 to 2 do
			d := !d + (1 lsl (8*i + j))
			(*lsl permet de decaler 1 de 8*i+j bit vers la gauche*)
		done
	done;
	Unix.mkdir (file) !d;
	let i = ref (-1) and fileName = file^"/"^fileNames^"_" in
	let nextName () = incr i; fileName^string_of_int(!i)^".ppm"
	in {startTime = Unix.gettimeofday (); fileDirectory = file; fileName = fileNames; nextName = nextName; currentNumberFiles = i; x = x; y = y};;

let read_record record = 
	let nbOctet = ref 0 
	and file = record.fileDirectory^"/"^record.fileName^"_" in
	for i = 0 to !(record.currentNumberFiles) do
		nbOctet := !nbOctet + (Unix.lstat (file^string_of_int i^".ppm")).st_size
	done;
	let txt = ref "name : " in
	txt := !txt^record.fileName^"\n";
	txt := !txt^"résolution : "^string_of_int record.x^" x "^string_of_int record.y;
	txt := !txt^"\ntime : ";
	let dure = Unix.gettimeofday () -. record.startTime in
	let hour = iof (dure/.3600.) in
	let nbHour = 3600.*.foi hour in
	let minute = iof ((dure -. nbHour)/.60.) in
	let nbMinute = 60. *. foi minute in
	let second = iof (dure -. nbHour -. nbMinute) in
	let nbSecond = foi second in
	let ms = iof ((dure -. nbSecond -. nbMinute -. nbHour)*.1000.) in
	txt := !txt^(string_of_int hour)^"h "^(string_of_int minute)^"' "^(string_of_int second)^"'' "^(string_of_int ms)^"ms\n";
	txt := !txt^"frames : "^string_of_int(!(record.currentNumberFiles)+1)^"\n";
	txt := !txt^"size : ";
	if !nbOctet >= 1000000000 then txt := !txt^string_of_float ((foi(iof((foi !nbOctet)/.1000000.)))/.1000.)^"Go\n";
	if !nbOctet < 1000000000 && !nbOctet >= 1000000 then txt := !txt^string_of_float ((foi(iof((foi !nbOctet)/.1000.)))/.1000.)^"Mo\n";
	if !nbOctet < 1000000 && !nbOctet >= 1000 then txt := !txt^string_of_float (foi !nbOctet /.1000.)^"Ko\n";
	if !nbOctet < 1000 then txt := !txt^string_of_int !nbOctet^"o\n";
	!txt;;


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
	let write_endLine txt =
		output_string chanel (txt^"\n");
	and
		write txt = output_string chanel txt
	and
		end_writing () =
		close_out chanel;
	in write_endLine, write, end_writing;;

let save_record record = 
	let file = record.fileDirectory^"/"^record.fileName^" record.txt" in
	let write_endLine, write, end_writing = get_writing_function_file file in
	let stats = read_record record in
	write stats;
	print_string stats;
	end_writing ();;

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
type color = {r : float; g : float; b : float; a : float};;
type pixel = {c : color; z : float; pid : int};;
(*les pixel on une dimension de profondeur affin d'appliquer un Z-buffering*)
let get_id = let i = ref 0 in fun () -> incr i; !i;;
let print_color c = 
	print_string "\nr : ";
	print_int (iof ((c.r/.c.a)**0.5));
	print_string "\ng : ";
	print_int (iof ((c.g/.c.a)**0.5));
	print_string "\nb : ";
	print_int (iof ((c.b/.c.a)**0.5));
	print_string "\na : ";
	print_float c.a;;
let copie_color c = {r = c.r; g = c.g; b = c.b; a = c.a};;
let create_color r g b a = {r = a *. foi (r*r); g = a *. foi (g*g); b = a *. foi (b*b); a = a};; (*On stoque les couleur au carré pour que les melange de couleur soit plus agréable*)
let string_of_color c = (string_of_int (iof ((c.r/.c.a)**0.5)))^" "^(string_of_int (iof ((c.g/.c.a)**0.5)))^" "^(string_of_int (iof ((c.b/.c.a)**0.5)))^" ";;
let export_color write c = 
	write (string_of_int (iof ((c.r/.c.a)**0.5)));
	write  " ";
	write (string_of_int (iof ((c.g/.c.a)**0.5)));
	write  " ";
	write (string_of_int (iof ((c.b/.c.a)**0.5)));
	write  " ";;

let get_Graphics_color c = Graphics.rgb (iof ((c.r/.c.a)**0.5)) (iof ((c.g/.c.a)**0.5)) (iof ((c.b/.c.a)**0.5));;
let black = create_color 0 0 0 1. and
	white = create_color 255 255 255 1. and
	green = create_color 0 255 0 1. and
	red = create_color 255 0 0 1. and
	blue = create_color 0 0 255 1. and
	gray = create_color 128 128 128 1.;;

let add_color c1 c2 = (*la couleur c1 est "au dessus" de la couleur c2*)
	let alphaTemp = 1. -. c1.a in
	{r = c1.r +. c2.r *. alphaTemp; g = c1.g +. c2.g *. alphaTemp; b = c1.b +. c2.b *. alphaTemp; a = c1.a +. c2.a *. alphaTemp};;

let scale_color c a =
	{r = c.r *. a; g = c.g *. a; b = c.b *. a; a = c.a};;

let mix_color c1 c2 s = 
	(*permet de créé une nouvelle couleur en faisant un melange pondéré de deux coleurs*)
	let temp = 1. -. s in
	let r = c1.r *. s +. temp *. c2.r and
		g = c1.g *. s +. temp *. c2.g and
		b = c1.b *. s +. temp *. c2.b and
		a = c1.a *. s +. temp *. c2.a in
	{r = r; g = g; b = b; a = a};;

let mix_pixel p1 p2 s =
	{c = mix_color p1.c p2.c s; z = p1.z *. s +. p2.z *. (1. -. s); pid = p1.pid};;

(* let plot a b = Graphics.fill_rect (10*a) (10*b) (10*a + 10) (10*b + 10);;
 *)
let rec insere_pixel liste pix = match liste with
	| [] -> [pix]
	(* | h::t when pix.pid = -2 -> liste *)
	| h::t when h.pid = pix.pid && h.z < pix.z -> h::t
	| h::t when h.pid = pix.pid -> pix::t
	| h::t when h.z > pix.z -> if pix.c.a = 1. then [pix] else pix::liste
	| h::t -> if h.c.a != 1. then h::insere_pixel t pix else [h];;

let rec fusionne_pixels liste = match liste with
	| [] -> create_color 0 0 0 0.00001 (*se cas n'est pas cencé arriver, le résultat permet de ne pas avoir d'erreur*)
	| [p] -> p.c
	| h::t -> add_color h.c (fusionne_pixels t);;

class imageBuffer line column background = object (self)
	(*line est la taille d'une ligne, donc le nombre de colloçnne et column est la taille d'une collonne donc le nombre de ligne*)
	(*buffer d'image, permetant de créé pas à pas une image et de l'exporter au format ppm*)
	val buffer = Array.make_matrix line column [{c = background; z = max_float; pid = 0}]
	val mutable image = Array.make_matrix line column background
	val mutable haveImage = false
	val mutable backgroundPixel = {c = background; z = max_float; pid = 0}
	val line = line
	val column = column

	method clear () =
		haveImage <- false;
		(* buffer <- Array.make_matrix line column [backgroundPixel]; *)
		image <- Array.make_matrix line column background;

		for i = 0 to line - 1 do
			for j = 0 to column - 1 do
			buffer.(i).(j) <- [backgroundPixel]
		done
	done
	method draw_pixel x y pix = 
		if 0<=x && x<line && 0<=y && y<column then begin
			buffer.(x).(y) <- insere_pixel buffer.(x).(y) pix;
			haveImage <- false;
		end

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

	method draw_line x1 y1 x2 y2 pix = 
		let next, x, y, notOver = self#middle_point_algorithm x1 y1 x2 y2 in
		self#draw_pixel !x !y pix;
		while !notOver do
			next ();
			self#draw_pixel !x !y pix
		done;

	method draw_line_gradient x1 y1 x2 y2 p1 p2 = 
		let next, x, y, notOver = self#middle_point_algorithm x1 y1 x2 y2 in
		let p1 = ref p1 and p2 = ref p2 in
		if !x != x1 || !y != y1 then switch p1 p2;
		let dx = abs (x2 - x1) and
			dy = abs (y2 - y1) in
		let s = ref 1. and
			ds = 1. /. (foi (max dx dy)) in

		self#draw_pixel !x !y (mix_pixel !p1 !p2 !s);
		while !notOver do
			next ();
			s := !s -. ds;
			self#draw_pixel !x !y (mix_pixel !p1 !p2 !s)
		done

	method draw_triangle x1 y1 x2 y2 x3 y3 pix = 
		self#draw_line x1 y1 x2 y2 pix;
		self#draw_line x2 y2 x3 y3 pix;
		self#draw_line x3 y3 x1 y1 pix

	method draw_triangle_gradiant x1 y1 x2 y2 x3 y3 p1 p2 p3 = 
		self#draw_line_gradient x1 y1 x2 y2 p1 p2;
		self#draw_line_gradient x2 y2 x3 y3 p2 p3;
		self#draw_line_gradient x3 y3 x1 y1 p3 p1;


	method fill_triangle x1 y1 x2 y2 x3 y3 pix = 
		match x1 with
			|_ when x1 = x2 && y1 = y2 -> self#draw_line x1 y1 x3 y3 pix
			|_ when x2 = x3 && y2 = y3 -> self#draw_line x2 y2 x1 y1 pix
			|_ when x3 = x1 && y3 = y1 -> self#draw_line x3 y3 x2 y2 pix
			|_ -> begin

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
				
				self#draw_pixel !xL !yL pix;
				while !notOverL do
					(*On calcule les point successif des deux segement jusqu'a ce que le point afficher soit sur la ligne juste au dessus *)
					while !yL != !mainY && !notOverL do
						nextLong ();
						self#draw_pixel !xL !yL pix
					done;
					while !(y.(!i)) != !mainY && !(notOver.(1)) do
						if not !(notOver.(!i)) then i := 1;
						next.(!i) ();
						self#draw_pixel !(x.(!i)) !(y.(!i)) pix;
					done;

					for z = min !xL !(x.(!i)) + 1 to max !xL !(x.(!i)) - 1 do
						self#draw_pixel z !mainY pix
					done;
					incr mainY
				done
			end

	method fill_triangle_gradient x1 y1 x2 y2 x3 y3 p1 p2 p3 = 
		match x1 with
			|_ when x1 = x2 && y1 = y2 -> self#draw_line_gradient x1 y1 x3 y3 (mix_pixel p1 p2 0.5) p3
			|_ when x2 = x3 && y2 = y3 -> self#draw_line_gradient x2 y2 x1 y1 (mix_pixel p2 p3 0.5) p1
			|_ when x3 = x1 && y3 = y1 -> self#draw_line_gradient x3 y3 x2 y2 (mix_pixel p3 p1 0.5) p2
			|_ -> begin

				let x1 = ref x1 and y1 = ref y1 and x2 = ref x2 and y2 = ref y2 and x3 = ref x3 and y3 = ref y3 and p1 = ref p1 and p2 = ref p2 and p3 = ref p3 in
				if !y2 > !y3 then begin switch y3 y2; switch x3 x2; switch p3 p2 end;
				if !y1 > !y3 then begin switch y1 y3; switch x1 x3; switch p1 p3 end;
				if !y1 > !y2 then begin switch y1 y2; switch x1 x2; switch p1 p2 end;
				(*on a alors y1 <= y2 <= y3*)

				let nextLong, xL, yL, notOverL = self#middle_point_algorithm !x1 !y1 !x3 !y3 and
					n1S, x1S, y1S, notOver1S   = self#middle_point_algorithm !x1 !y1 !x2 !y2 and
					n2S, x2S, y2S, notOver2S   = self#middle_point_algorithm !x2 !y2 !x3 !y3 in
				let next = [|n1S; n2S|] and x = [|x1S; x2S|] and y = [|y1S; y2S|] and notOver = [|notOver1S; notOver2S|] and c = [|[|p1; p2|]; [|p2; p3|]|] and
					mainY = ref (!y1 + 1) and i = ref 0 in

				(*On calcul les distances pour effectuer les interpolation lineaire*)
				let dx12 = abs (!x2 - !x1) and dy12 = abs (!y2 - !y1) in
				let ds12 = 1. /. (foi ( max dx12 dy12)) in
				let dx23 = abs (!x2 - !x3) and dy23 = abs (!y2 - !y3) in
				let ds23 = 1. /. (foi (max dx23 dy23)) in
				let dx31 = abs (!x3 - !x1) and dy31 = abs (!y3 - !y1) in
				let s31 = ref 1. and ds31 = 1. /. (foi (max dx31 dy31)) in

				let longColor = ref !p1 and littleColor = ref !p1 and s = ref 1. and ds = [|ds12; ds23|] in
				(*On initialise les couleur a gauche et a droite du segment horizontal que l'on vas tracer*)

				self#draw_pixel !xL !yL !longColor;
				while !notOverL do

					while !yL != !mainY && !notOverL do
						nextLong ();
						s31 := !s31 -. ds31;
						longColor := mix_pixel !p1 !p3 !s31;
						self#draw_pixel !xL !yL !longColor
					done;
					while !(y.(!i)) != !mainY && !(notOver.(1)) do
						if not !(notOver.(!i)) && !i != 1 then begin 
							i := 1;
							s := 1.
						end;
						next.(!i) ();
						s := !s -. ds.(!i);
						littleColor := mix_pixel !(c.(!i).(0)) !(c.(!i).(1)) !s;
						self#draw_pixel !(x.(!i)) !(y.(!i)) !littleColor;
					done;

					let tempS = ref 1. and tempDs = 1. /. foi (abs (!xL - !(x.(!i)))) in
					let tempC = if !xL < !(x.(!i)) then [|!longColor; !littleColor|] else [|!littleColor; !longColor|] in
					
					for z = min !xL !(x.(!i)) + 1 to max !xL !(x.(!i)) - 1 do
						tempS := !tempS -. tempDs;
						self#draw_pixel z !mainY (mix_pixel tempC.(0) tempC.(1) !tempS)
					done;
					incr mainY
				done
			end

	method draw_circle x y r pix = 
		let x_ = ref 0 and y_ = ref r and d = ref r in

		while !y_ != 0 do
		if !d = r then begin
			self#draw_pixel (x + !x_) (y + !y_) pix;
			self#draw_pixel (x + !y_) (y - !x_) pix;
			self#draw_pixel (x - !x_) (y - !y_) pix;
			self#draw_pixel (x - !y_) (y + !x_) pix
		end;
		if !d = r then incr x_;
		if !d < r then incr x_;
		if !d > r then begin decr y_; decr x_ end;
		d := iof ((foi (!x_ * !x_  +  !y_ * !y_))**0.5);
		done

	method fill_circle x y r pix = 
		let x_ = ref 0 and y_ = ref r and d = ref r in

		self#draw_pixel x y pix;
		while !y_ != 0 do
		if !d = r then begin
			for z = 0 to !x_ do
			self#draw_pixel (x + z) (y + !y_) pix;
			self#draw_pixel (x + !y_) (y - z) pix;
			self#draw_pixel (x - z) (y - !y_) pix;
			self#draw_pixel (x - !y_) (y + z) pix
		done;
		end;
		if !d = r then incr x_;
		if !d < r then incr x_;
		if !d > r then begin decr y_; decr x_ end;
		d := iof ((foi (!x_ * !x_  +  !y_ * !y_))**0.5);
		done


	method set_background color = backgroundPixel <- {c = color; z = max_float; pid = 0}

	method get_image () =
		if not haveImage then begin
			haveImage <- true;
			for i = 0 to line - 1 do
				for j = 0 to column - 1 do
					image.(i).(j) <- fusionne_pixels buffer.(i).(j)
				done
			done
		end;
		image

	method show_image () = 
		self#get_image (); (*on convertie les liste de pixels en une matrice de couleur*)
		Graphics.open_graph ((string_of_int line)^"x"^(string_of_int column));
		Graphics.auto_synchronize false;
		for i = 0 to line - 1 do
			for j = 0 to column - 1 do
				Graphics.set_color (get_Graphics_color image.(i).(j));
				Graphics.plot i j
			done
		done;
		Graphics.synchronize ()

	method export_to_ppm file withThreads = 
		let image_ = self#get_image () in (*on convertie les liste de pixels en une matrice de couleur*)
		
		let save () =
			let write_endLine, write, end_writing = get_writing_function_file file in
			write_endLine ("P3 "^(string_of_int line)^" "^(string_of_int column)^" 255");
			(* let txt = ref "" in *)
			for j = column - 1 downto 0 do
				(* txt := ""; *)
				for i = 0 to line - 1 do
					export_color write image_.(i).(j)
					(* write (string_of_color image_.(i).(j)) *)
					(* txt := !txt ^ string_of_color image_.(i).(j) *)
				done;
				(* write !txt *)
				write "\n"
			done;
			end_writing ()
		in
		if withThreads then begin
			let start = Thread.create save in
			start ();
			()
		end else save ();
		()
end;;
(* 
let test = new imageBuffer 300 300 black;;
(* test#draw_line_gradient 50 50 70 60 white green;;
test#draw_line_gradient 50 50 70 80 white red;;
test#draw_line_gradient 50 50 30 80 white blue;;
test#draw_line_gradient 50 50 30 60 white gray;;
test#draw_line_gradient 50 50 70 40 white green;;
test#draw_line_gradient 50 50 70 20 white red;;
test#draw_line_gradient 50 50 30 20 white blue;;
test#draw_line_gradient 50 50 30 40 white gray;; *)
let green_ = create_color 0 255 0 0.1 and blue_ = create_color 0 0 255 0.1;;

test#fill_triangle_gradient 80 65  20 20  80 65 {c = red; z = 1.; pid = 1} {c = green; z = 1.; pid = 1} {c = green_; z = 1.; pid = 1};;

test#fill_triangle_gradient 200 800 800 650 300 100 {c = red; z = 1.; pid = 1} {c = green; z = 1.; pid = 1} {c = green_; z = 1.; pid = 1};;
test#fill_triangle_gradient 400 500 500 100 850 600 {c = blue; z = 0.8; pid = 1} {c = blue_; z = 2.5; pid = 1} {c = red; z = 2.5; pid = 1};;
(* test#fill_triangle_gradient 800 600 500 1000 10 50 blue red green;; *)
 *)(* test#draw_triangle 499 499 298 10 1 1 white;;
(* test#draw_line 499 499 1 1 green; *)
(* test#fill_triangle 400 200 300 200 100 200 red;; *)
(* test#draw_line 0 0 10 298 white;;
test#draw_line 499 499 0 0 white;;
test#draw_line 10 298 499 499 white;; *)
(* test#draw_line 0 0 10 10 red;; *)
(* test#fill_circle 250 250 50 green;;
test#draw_circle 250 250 50 red;; *)

test#show_image ();; *)
(* test#export_to_ppm "C:/Users/pierre/Desktop/programation/Caml/TIPE/triangleAdvanced.ppm";; *)


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