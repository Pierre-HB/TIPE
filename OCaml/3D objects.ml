#use "D:/documents/devoires/prepa/TIPE/final/OCaml/3D kernel.ml";;

let pi = acos (-.1.);;

let modulo x n = let m = x mod n in if m >= 0 then m else m+n;;
let arondi x = if x < 0. then iof x - 1 else iof x;;


class randomGenerator startSeed = object (self)
	val mutable seed = startSeed
	val m = 2147483648 (*2**31*)
	val a = 1103515245
	val c = 12345
	method random = seed <- (seed*a+c) mod m; seed
	method randint n = self#random mod n
	method randrange a b = self#randint (b-a) + a
	method random_float = (float_of_int (self#random)) /. 2147483648.
end;;

let liste_aleatoire randGen n = 
	let liste = Array.make n 0 in
	for i = 0 to n-1 do
		liste.(i) <- i 
	done;
	for i = n-1 downto 1 do
		let r = randGen#randint i in
		let c = liste.(i) in
		liste.(i) <- liste.(r);
		liste.(r) <- c 
	done;
	liste;;

class virtual noise = object method virtual get_noise : float->float->float end;;		

class perlin seed resolution= object (self)
	inherit noise
	val mutable entiers = liste_aleatoire (new randomGenerator seed) 256
	val res = resolution
	val vecteurs = [|(1.,0.);(0.,1.);(-1.,0.);(0.,-1.);(0.70710678118654757,0.70710678118654757);
	(-.0.70710678118654757,0.70710678118654757);(0.70710678118654757,-.0.70710678118654757);(-.0.70710678118654757,-.0.70710678118654757)|]
	
	method private get_vector x y = vecteurs.(modulo entiers.(modulo (y+entiers.(modulo x 256)) 256) 8)
	method private scalaire v1 v2 = let x1,y1 = v1 and x2,y2 = v2 in x1*.x2 +. y1*.y2
	method private compute_scalar x0 y0 x y = 
		let u = self#get_vector x0 y0 and
			v = (x-.foi x0, y-.foi y0) in self#scalaire u v
	method private polynome x = 3.*.x*.x -. 2.*.x*.x*.x
	method get_noise x y =
		let x = x/.res and y = y/.res in
		let x0 = arondi x and y0 = arondi y in
		let x1 = x0 + 1 and y1 = y0 + 1 in

		let dx = self#polynome (x-.foi x0) and
			dy = self#polynome (y-.foi y0) in

		let s_x0y0 = self#compute_scalar x0 y0 x y and
			s_x1y0 = self#compute_scalar x1 y0 x y and
			s_x0y1 = self#compute_scalar x0 y1 x y and
			s_x1y1 = self#compute_scalar x1 y1 x y in

		let l1 = s_x1y0*.dx +. s_x0y0*.(1.-.dx) and
			l2 = s_x1y1*.dx +. s_x0y1*.(1.-.dx) in
		l2*.dy +. l1*.(1.-.dy)
end;;

class advancedPerlin seed resolutions coefs forces = object (self)
	inherit noise
	val rg = new randomGenerator seed
	val n = Array.length coefs
	val mutable total = 0.
	val mutable perlins = Array.make (Array.length coefs) (new perlin 0 0.)
	
	method get_noise x y = 
		let l = ref 0. in 
		for i = 0 to n-1 do
			l := !l +. (perlins.(i)#get_noise x y)*.coefs.(i)*.forces.(i) 
		done;
		!l/.total
	
	initializer 
		for i = 0 to n-1 do 
			perlins.(i) <- new perlin rg#random resolutions.(i);
			total <- total+.coefs.(i) 
		done
end;;


class sunLight orientation = object (self)
	inherit light () as super
	val color = create_color 250 220 200 1.
	val reflexionColor = create_color 250 250 250 1.
	val orientation = orientation
	val seuilReflexion = 0.95


	method apply_light_face material p1 p2 p3 normal camera = 
		let scal = scalar orientation normal in
		material#add_light color (scal *. 0.5 +. 0.5)
	method apply_light_point material p normal camera = 
		self#apply_light_face material p p p normal camera;
		let v = vector_sub3D p (camera.repere3D#get_origine ())
		and n = normal#copy_vector3D () in
		let s_ = 2. *. scalar v normal in
		n#dot s_;
		let v_ = vector_sub3D n v in
		v_#normalize ();

		let s = scalar v_ orientation in
		material#add_reflexion reflexionColor (exp (-.5.*.((-.s *. s*.s(* abs_float s *))+.1.)))

	initializer
		orientation#normalize ()
end;;


class water () = object (self)
	inherit material () as super
	val originalColor = create_color 10 50 150 0.7
	val light_effect = 0.5
	val light_reflexion_effect = 0.8
	val mutable tempColor = blue
	val meshColor = create_color 0 10 200 1.
	val rAbsorbance = 0.85
	val gAbsorbance = 0.5
	val bAbsorbance = 0.
end;;
class water_ () = object (self)
	inherit material () as super
	val originalColor = create_color 10 50 150 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.8
	val mutable tempColor = blue
	val meshColor = create_color 0 10 200 1.
	val rAbsorbance = 0.85
	val gAbsorbance = 0.7
	val bAbsorbance = 0.
end;;
let eau = new water ();;

class fire () = object (self)
	inherit material () as super
	val originalColor = create_color 50 5 5 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.8
	val mutable tempColor = blue
	val meshColor = create_color 220 100 100 1.
	val rAbsorbance = 0.05
	val gAbsorbance = 0.8
	val bAbsorbance = 0.9
end;;
let feu = new fire ();;


class orange () = object (self)
	inherit material () as super
	val originalColor = create_color 200 80 0 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.8
	val mutable tempColor = blue
	val meshColor = create_color 255 150 150 1.
	val rAbsorbance = 0.0
	val gAbsorbance = 0.7
	val bAbsorbance = 1.
end;;

class greenery () = object (self)
	inherit material () as super
	val originalColor = create_color 80 200 100 1.
	val light_effect = 0.4
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 100 200 100 1.
	val rAbsorbance = 0.8
	val gAbsorbance = 0.0
	val bAbsorbance = 0.8
end;;

class dark_greenery () = object (self)
	inherit material () as super
	val originalColor = create_color 20 100 50 1.
	val light_effect = 0.6
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 100 180 100 1.
	val rAbsorbance = 0.8
	val gAbsorbance = 0.0
	val bAbsorbance = 0.7
end;;

class white () = object (self)
	inherit material () as super
	val originalColor = create_color 200 200 200 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 255 255 255 1.
	val rAbsorbance = 0.
	val gAbsorbance = 0.
	val bAbsorbance = 0.
end;;
class grey () = object (self)
	inherit material () as super
	val originalColor = create_color 128 128 128 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 200 200 200 1.
	val rAbsorbance = 0.2
	val gAbsorbance = 0.2
	val bAbsorbance = 0.2
end;;
class grey_ () = object (self)
	inherit material () as super
	val originalColor = create_color 128 128 128 0.5
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 200 200 200 0.5
	val rAbsorbance = 0.2
	val gAbsorbance = 0.2
	val bAbsorbance = 0.2
end;;
class black () = object (self)
	inherit material () as super
	val originalColor = create_color 50 50 50 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 150 150 150 1.
	val rAbsorbance = 0.5
	val gAbsorbance = 0.5
	val bAbsorbance = 0.5
end;;
class green () = object (self)
	inherit material () as super
	val originalColor = create_color 0 200 0 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 0 255 0 1.
	val rAbsorbance = 0.9
	val gAbsorbance = 0.
	val bAbsorbance = 0.9
end;;
class red () = object (self)
	inherit material () as super
	val originalColor = create_color 200 0 0 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 255 0 0 1.
	val rAbsorbance = 0.
	val gAbsorbance = 0.9
	val bAbsorbance = 0.9
end;;
class blue () = object (self)
	inherit material () as super
	val originalColor = create_color 0 0 200 1.
	val light_effect = 0.5
	val light_reflexion_effect = 0.9
	val mutable tempColor = blue
	val meshColor = create_color 0 0 255 1.
	val rAbsorbance = 0.9
	val gAbsorbance = 0.9
	val bAbsorbance = 0.
end;;



let p = new point (new vector3D 0. 0. 0.);;
let f = (new face p p p eau);;


class square center n radius_ material_ hightRender = object (self)
	inherit object3D () as super
	val radius = radius_
	val mutable position = center
	val faces = Array.make ((n-1)*(n-1)*2) f
	val points = Array.make (n*n) p
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		let renderSetup_ = {renderMesh = renderSetup.renderMesh; renderMaterial = renderSetup.renderMaterial; renderHidenFaces = true; saveImageUsingThreads = renderSetup.saveImageUsingThreads} in
		if renderType then super#render_light_point imageBuffer camera light renderSetup_ else super#render_light_face imageBuffer camera light renderSetup_

	initializer
	let n_ = foi (n-1) in
	for x = 0 to n - 1 do
		for y = 0 to n - 1 do
			let v = new vector3D  (radius*.(((foi x) /. n_) -. 0.5)) 0. (radius*.(((foi y) /. n_) -. 0.5)) in
			points.(x*n + y) <- new point (vector_add3D v position)
		done;
	done;
	for x = 0 to n - 2 do
		for y = 0 to n - 2 do
			faces.(2*(x*(n-1) + y)) <- new face points.(x*n + y) points.((x+1)*n + y) points.((x+1)*n + y+1) material;
			faces.(2*(x*(n-1) + y) + 1) <- new face points.(x*n + y) points.((x+1)*n + y+1) points.(x*n + y+1) material
		done;
	done;

end;;


class polygone center n radius_ material_ hightRender = object (self)
	inherit object3D () as super
	val radius = radius_
	val mutable position = center
	val faces = Array.make n f
	val points = Array.make (n+1) p
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		let renderSetup_ = {renderMesh = renderSetup.renderMesh; renderMaterial = renderSetup.renderMaterial; renderHidenFaces = true; saveImageUsingThreads = renderSetup.saveImageUsingThreads} in
		if renderType then super#render_light_point imageBuffer camera light renderSetup_ else super#render_light_face imageBuffer camera light renderSetup_

	initializer
	(* let n_ = foi n in *)
	points.(0) <- new point (position#copy_vector3D ());
	for i = 0 to n-1 do
		let v = new vector3D (cos (2.*.pi*.(foi i)/.(foi (n)))) 0. (sin (2.*.pi*.(foi i)/.(foi (n)))) in
		v#dot radius;
		points.(i+1) <- new point (vector_add3D v position)
	done;
	for j = 1 to n do
		faces.(j-1) <- new face points.(0) points.(j) points.(if j = n then 1 else (j+1)) material
	done;
end;;

class cone center n radius_ hight material_ hightRender = object (self)
	inherit object3D () as super
	val radius = max radius_ hight
	val base = radius_
	val mutable position = center
	val faces = Array.make (2*n) f
	val points = Array.make (2*n+2) p
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup

	initializer
	(* let n_ = foi n in *)
	points.(0) <- new point (position#copy_vector3D ());
	points.(1) <- new point (vector_add3D (new vector3D 0. hight 0.) position);

	for i = 0 to n-1 do
		let v = new vector3D (cos (2.*.pi*.(foi i)/.(foi (n)))) 0. (sin (2.*.pi*.(foi i)/.(foi (n)))) in
		v#dot base;
		points.(i+2) <- new point (vector_add3D v position);
		points.(i+2+n) <- new point (vector_add3D v position)
	done;
	for j = 2 to (n+1) do
		faces.(j-2) <- new face points.(0) points.(if j = n+1 then 2 else (j+1)) points.(j) material;
		faces.(j-2 + n) <- new face points.(1) points.(j+n) points.(if j = n+1 then 2+n else (j+1+n))  material
	done;
end;;

class cylinder center n radius_ hight material_ hightRender = object (self)
	inherit object3D () as super
	val radius = max radius_ hight
	val base = radius_
	val mutable position = center
	val faces = Array.make (4*n) f
	val points = Array.make (4*n+2) p
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup

	initializer
	(* let n_ = foi n in *)
	points.(0) <- new point (position#copy_vector3D ());
	points.(1) <- new point (vector_add3D (new vector3D 0. hight 0.) position);

	for i = 0 to n-1 do
		let v = new vector3D (cos (2.*.pi*.(foi i)/.(foi (n)))) 0. (sin (2.*.pi*.(foi i)/.(foi (n)))) in
		v#dot base;
		points.(i+2) <- new point (vector_add3D v position);
		points.(i+2+n) <- new point (vector_add3D v position);
		v#set_y hight;
		points.(i+2+2*n) <- new point (vector_add3D v position);
		points.(i+2+3*n) <- new point (vector_add3D v position)
	done;
	for j = 2 to (n+1) do
		faces.(j-2) <- new face points.(0) points.(if j = n+1 then 2 else (j+1)) points.(j) material;
		faces.(j-2 + n) <- new face points.(1) points.(j+3*n) points.(if j = n+1 then 2 + 3*n else (j+1+3*n))  material;
		faces.(j-2 + 2*n) <- new face points.(j+n) points.(if j = n+1 then j+1 else j+1+n) points.(if j = n+1 then j+1+n else j+2*n+1) material;
		faces.(j-2 + 3*n) <- new face points.(j+n+n) points.(if j = n+1 then j else j+n) points.(if j = n+1 then j+1+n else j+2*n+1) material;
	done;
end;;

(* let fleche_head_size = 0.5
and fleche_head_radius = 0.2
and fleceh_body_radius = 0.1;; *)

class fleche center direction n fleche_head_size fleche_head_radius fleceh_body_radius material_ hightRender = object (self)
	inherit object3D () as super
	val faces = Array.make (6*n) f
	val points = Array.make (6*n+4) p
	val mutable position = center
	val radius = direction#get_norme ()
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup

	initializer
		let hBody = max 0. (radius-.fleche_head_size) in
		let hHead = radius -. hBody in
		let head = new cone (new vector3D 0. hBody 0.) n fleche_head_radius hHead material hightRender
		and body = new cylinder (new vector3D 0. 0. 0.) n fleceh_body_radius hBody material hightRender in

		for i = 0 to 6*n - 1 do
			if i < 2*n then faces.(i) <- (head#get_faces ()).(i)
			else faces.(i) <- (body#get_faces ()).(i-2*n)
		done;
		for i = 0 to 6*n + 3 do
			if i < 2*n + 2 then points.(i) <- (head#get_points ()).(i)
			else points.(i) <- (body#get_points ()).(i-2*n - 2)
		done;

		let v = direction#copy_vector3D () in
		v#normalize ();
		let x = v#get_x ()
		and y = v#get_y ()
		and z = v#get_z () in
		let e1 = new vector3D (y-.z) (z-.x) (x-.y) in
		e1#normalize ();
		let e3 = cross3D e1 v in
		let x1 = e1#get_x ()
		and y1 = e1#get_y ()
		and z1 = e1#get_z ()
		and x3 = e3#get_x ()
		and y3 = e3#get_y ()
		and z3 = e3#get_z () in
		let rot = create_matrix    [|x1; x; x3; y1; y; y3; z1; z; z3|] (3,3) in


		let o = center#copy_vector3D () in
		o#dot (-.1.);

		let move1 = {rotation = rot; translation = new vector3D 0. 0. 0.} 
		and move2 = {rotation = get_In 3; translation = o} in
		self#move_original_position move1;
		self#move_original_position move2

end;;

class cube o n size material_ hightRender = object (self)
	inherit object3D () as super
	val radius = size
	val mutable position = o
	val faces = Array.make ((n-1)*(n-1)*12) (new face p p p eau)
	val points = Array.make (n*n*6) p
	val material = material_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup


	initializer
	let n_ = foi (n-1) 
	and n2 = n*n 
	and n2_ = (n-1)*(n-1) in
	for x = 0 to n - 1 do
		for y = 0 to n - 1 do
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius) (radius*.((2.*.(foi y) /. n_) -. 1.)) in
			points.(x*n + y) <- new point (vector_add3D v position);
			let v = new vector3D (radius*.((2.*.(foi y) /. n_) -. 1.)) (-.radius) (radius*.((2.*.(foi x) /. n_) -. 1.)) in
			points.(x*n + y + n2) <- new point (vector_add3D v position);
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi y) /. n_) -. 1.)) (-.radius) in
			points.(x*n + y + 2*n2) <- new point (vector_add3D v position);
			let v = new vector3D (radius*.((2.*.(foi y) /. n_) -. 1.)) (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius) in
			points.(x*n + y + 3*n2) <- new point (vector_add3D v position);
			let v = new vector3D (radius) (radius*.((2.*.(foi y) /. n_) -. 1.)) (radius*.((2.*.(foi x) /. n_) -. 1.)) in
			points.(x*n + y + 4*n2) <- new point (vector_add3D v position);
			let v = new vector3D (-.radius) (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi y) /. n_) -. 1.)) in
			points.(x*n + y + 5*n2) <- new point (vector_add3D v position)
		done;
	done;
	for x = 0 to n - 2 do
		for y = 0 to n - 2 do
				for i = 0 to 5 do
				faces.(2*(x*(n-1) + y + i*n2_)) <- new face points.(x*n + y + i*n2) points.((x+1)*n + y + i*n2) points.((x+1)*n + y+1 + i*n2) material;
				faces.(2*(x*(n-1) + y + i*n2_) + 1) <- new face points.(x*n + y + i*n2) points.((x+1)*n + y+1 + i*n2) points.(x*n + y+1 + i*n2) material
			done;
		done;
	done;
end;;


class sphere o n size material_ hightRender = object (self)
	inherit object3D () as super
	val radius = size
	val mutable position = o
	val faces = Array.make ((n-1)*(n-1)*12) (new face p p p eau)
 	val points = Array.make (6*n*n - 12*n + 8) p
 	val material = material_
 	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup


	initializer
	let n_ = foi (n-1) 
	and n2 = n*n 
	and n2_ = (n-1)*(n-1) 
	and tempPoints = Array.make (n*n*6) (0,0,0) 
	and pointLocation = Array.make (n*n*6) (-1) in
	
	for x = 0 to n - 1 do
		for y = 0 to n - 1 do
			tempPoints.(x*n + y) <- (x, n-1, y);
			tempPoints.(x*n + y + n2) <- (y, 0, x);
			tempPoints.(x*n + y + 2*n2) <- (x, y, 0);
			tempPoints.(x*n + y + 3*n2) <- (y, x, n-1);
			tempPoints.(x*n + y + 4*n2) <- (n-1, y, x);
			tempPoints.(x*n + y + 5*n2) <- (0, x, y)
		done;
	done;
(*On fusionne les point qui sont les même*)
	for i = 0 to n*n*6 - 1 do
		if pointLocation.(i) = -1 then begin
			pointLocation.(i) <- i;
			for j = i + 1 to n*n*6 - 1 do
				if tempPoints.(j) = tempPoints.(i) then pointLocation.(j) <- i
			done
		end
	done;
	(*on créé les points, sans doublons*)
	let i = ref 0 in
	for j = 0 to n*n*6 - 1 do
		if pointLocation.(j) = j then begin
			let x, y, z = tempPoints.(j) in
			(*On met les coordonné a la racine pour avoir des triangle avec une surface plus reguliere*)
			let v = new vector3D (((2.*.(foi x) /. n_) -. 1.)) (((2.*.(foi y) /. n_) -. 1.)) (((2.*.(foi z) /. n_) -. 1.)) in
			v#normalize ();
			v#dot radius;
			points.(!i) <- new point (vector_add3D v position);
			pointLocation.(j) <- ( - !i);
			incr i;
		end

	done;
	let get_point p = if pointLocation.(p) < 0 
		then points.(-pointLocation.(p)) 
		else points.(-pointLocation.(pointLocation.(p))) in
	for x = 0 to n - 2 do
		for y = 0 to n - 2 do
				for i = 0 to 5 do
				faces.(2*(x*(n-1) + y + i*n2_)) <- new face (get_point (x*n + y + i*n2)) (get_point ((x+1)*n + y + i*n2)) (get_point ((x+1)*n + y+1 + i*n2)) material;
				faces.(2*(x*(n-1) + y + i*n2_) + 1) <- new face (get_point (x*n + y + i*n2)) (get_point ((x+1)*n + y+1 + i*n2)) (get_point (x*n + y+1 + i*n2)) material
			done;
		done;
	done;
end;;


class ocean_psysics n noise dt_ = object (self)
	val meshCinetique = Array.make (n*n) (0., 0.)
	val temp = Array.make (n*n) (0., 0.)
	val mutable points = Array.make 1 p
	val dt = dt_

	method update () =
		for x = 0 to n-1 do
 			for y = 0 to n-1 do
 				let d = ref 0.
 				and z, v = meshCinetique.(x*n + y) in
 				if x != 0 then 
 					d := !d +. (fst meshCinetique.((x-1)*n + y) -. z)
 					else d := !d +. (fst meshCinetique.((x+1)*n + y) -. z);
 				if x != n-1 then 
 					d := !d +. (fst meshCinetique.((x+1)*n + y) -. z)
 					else d := !d +. (fst meshCinetique.((x-1)*n + y) -. z);
 				if y != 0 then 
 					d := !d +. (fst meshCinetique.(x*n + y-1) -. z)
 					else d := !d +. (fst meshCinetique.(x*n + y+1) -. z);
 				if y != n-1 then 
 					d := !d +. (fst meshCinetique.(x*n + y+1) -. z)
 					else d := !d +. (fst meshCinetique.(x*n + y-1) -. z);

				let a = !d /. 16. in
		        let v_ = v +. a*.dt in
		        let z_ = z+. v_*.dt in
		        temp.(x*n + y) <- (z_, v_)
		    done
		done;
		for i = 0 to n*n-1 do
			meshCinetique.(i) <- temp.(i)
		done;
		self#update_point ()

	method set_points points_ = points <- points_

	method update_point () =
		for x = 0 to n-1 do
 			for y = 0 to n-1 do
 			let z, v = meshCinetique.(x*n + y) in
 				(points.(x*n + y)#get_fixed_position ())#set_y z
 			done
 		done

	method get_cinetique () = meshCinetique

	initializer
		for x = 0 to n - 1 do
			for y = 0 to n - 1 do
				meshCinetique.(x*n + y) <- (noise#get_noise (foi x) (foi y), 0.)
			done
		done
end;;
class ocean center n radius_ meshCinetique_ mat hightRender = object (self)
	inherit object3D () as super
	val radius = radius_
	val mutable position = center
	val faces = Array.make ((n-1)*(n-1)*2) (new face p p p eau)
	val points = Array.make (n*n) p
	val material = mat 
	val meshCinetique = meshCinetique_
	val renderType = hightRender

	method render imageBuffer camera light renderSetup = 
		if renderType then super#render_light_point imageBuffer camera light renderSetup else super#render_light_face imageBuffer camera light renderSetup
	
	initializer
	let n_ = foi n in
	for x = 0 to n - 1 do
		for y = 0 to n - 1 do
			let i = (radius*.(((foi x) /. n_) -. 0.5))
			and j = (radius*.(((foi y) /. n_) -. 0.5)) 
			and k = fst meshCinetique.(x*n + y) in
			let v = new vector3D i k j in
			points.(x*n + y) <- new point (vector_add3D v position);
		done;
	done;
	for x = 0 to n - 2 do
		for y = 0 to n - 2 do
			faces.(2*(x*(n-1) + y)) <- new face points.(x*n + y) points.((x+1)*n + y) points.((x+1)*n + y+1) material;
			faces.(2*(x*(n-1) + y) + 1) <- new face points.(x*n + y) points.((x+1)*n + y+1) points.(x*n + y+1) material
		done;
	done;

end;;

let generate_ocean n o radius material hightRender dt_ seed resolutions weights forces = 
	for i = 0 to Array.length resolutions -1 do
		resolutions.(i) <- resolutions.(i) *.1.123*.foi n /.(2.*.radius)
	done;	
	let perl = new advancedPerlin seed resolutions weights forces in
	let oce_physic = new ocean_psysics n perl dt_ in
	let oce = new ocean o n radius (oce_physic#get_cinetique ()) material hightRender in
	oce_physic#set_points (oce#get_points ());
	oce_physic, oce;;