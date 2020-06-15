let iof = int_of_float and
	foi = float_of_int;;


let foreach_list liste callback = 
	(*applique la une fonction à tout les element d'une list*)
	let temp = ref liste in
	while !temp <> [] do
		let h::t = !temp in
		temp := t;
		callback h
	done;;

let foreach_array tableau callback = 
	(*applique une fonction à chaque element d'une array*)
	for i = 0 to Array.length tableau - 1 do
		callback tableau.(i)
	done;;

let foi = float_of_int and
	iof = int_of_float;;

class matrix coefficient = object (self)
	val coefficient = coefficient
	val mutable determinant = 0.
	val mutable haveDeterminant = false
	val mutable shape = (0,0)
	val mutable n = 0

	method private foreach_coefficient callback = 
		for i = 0 to fst shape - 1 do
			for j = 0 to snd shape - 1 do
				callback i j
			done
		done
	method get_coef line columne = coefficient.(line).(columne)
	method set_coef line columne coef = 
		coefficient.(line).(columne) <- coef;
		haveDeterminant <- false
	method add_coef line columne coef = 
		coefficient.(line).(columne) <- coef +. coefficient.(line).(columne);
		haveDeterminant <- false
	method private self_sum opp (m : matrix) = 
		if shape != m#get_shape () then failwith "les matrice n'ont pas les même dimensions - ERROR self_sum (class matrice)";
		self#foreach_coefficient (fun i j -> coefficient.(i).(j) <- opp coefficient.(i).(j) (m#get_coef i j))
	method add_matrix m = self#self_sum (+.) m
	method sub_matrix m = self#self_sum (-.) m 
	method get_shape () = shape
	method dot alpha = 
		if haveDeterminant then determinant <- determinant *. alpha *. foi n;
		(*le determinant est une fonction n-lineaire*)
		self#foreach_coefficient (fun i j -> coefficient.(i).(j) <- coefficient.(i).(j) *. alpha)
	method get_under_matrix line columne =
		if 0 > line || line >= fst shape || 0 > columne || columne >= snd shape then failwith "Il n'estiste pas de tel sous matrice - ERROR get_under_matrix (class matrice)";
		let get_line l = if l < line then l else l-1 and
			get_columne c = if c < columne then c else c-1 in
		let m = Array.make_matrix (n-1) (n-1) 0. in
		self#foreach_coefficient (fun i j ->
			if i != line && j != columne then
				m.(get_line i).(get_columne j) <- coefficient.(i).(j));
		new matrix m
	method get_determinant () = 
	(* determine le determinant d'une matrice en dévelopant suivant la premiere ligne*)
		if fst shape != snd shape then failwith "la matrice n'est pas carré - ERROR get_determinant (class matrice)";
		if not haveDeterminant then begin
			haveDeterminant <- true;
			if n = 0 then determinant <- 1.
			else begin  
				determinant <- 0.;
				let coef = ref 1. in
				for i = 0 to n-1 do
					if coefficient.(i).(0) != 0. then determinant <- determinant  +. !coef *. coefficient.(i).(0) *. (self#get_under_matrix i 0)#get_determinant ();
					coef := -.1. *. !coef
				done
			end
		end;
		determinant
	method copy_matrix () = 
		let m = Array.make_matrix (fst shape) (snd shape) 0. in
		self#foreach_coefficient (fun i j -> m.(i).(j) <- coefficient.(i).(j));
		new matrix m
	initializer
		if Array.length coefficient != 0 then shape <- (Array.length coefficient, Array.length coefficient.(0));
		n <- max (fst shape) (snd shape)
end;;

let print_matrix m =
	let (line, column) = m#get_shape () in
	for i = 0 to line - 1 do
		print_string "\n| ";
		for j = 0 to column - 1 do
			print_float ((foi (iof (1000. *. m#get_coef i j)))/.1000.);
			print_string " | "
		done
	done;;


let matrix_product m1 m2 = 
(*Produit matriciel*)
	let l1, c1 = m1#get_shape () and
		l2, c2 = m2#get_shape () in
	if c1 <> l2 then failwith "Les matrice ne corresponde pas - ERROR matrix_product";
	let m = Array.make_matrix l1 c2 0. in
	for i = 0 to l1-1 do
		for j = 0 to c2-1 do
			for k = 0 to c1-1 do
				m.(i).(j) <- m1#get_coef i k *. m2#get_coef k j +. m.(i).(j)
			done
		done
	done;
	new matrix m;;

let transpose_matrix mat = 
(*Donne la transposé d'une matrice*)
	let l, c = mat#get_shape () in 
	let transpose = Array.make_matrix c l 0. in
	for x = 0 to l - 1 do
		for y = 0 to c - 1 do
			transpose.(y).(x) <- mat#get_coef x y
		done
	done;
	new matrix transpose;;
	
let matrix_sum opp m1 m2 = 
(*permet de faire un addition ou une soustraction dans Mn(R)*)
	let l1, c1 = m1#get_shape () and
		l2, c2 = m2#get_shape () in
	if (l1, c1) <> (l2, c2) then failwith "les matrices n'ont pas les même dimension - ERROR matrix_sum";
	let m = Array.make_matrix l1 c1 0. in
	for i = 0 to l1-1 do
		for j = 0 to c2-1 do
			m.(i).(j) <- opp (m1#get_coef i j) (m2#get_coef i j)
		done
	done;
	new matrix m;;
let matrix_add m1 m2 = matrix_sum (+.) m1 m2;;
let matrix_sub m1 m2 = matrix_sum (-.) m1 m2;;

let matrix_dot alpha m = 
(*renvoie une copy de la matrice, multiplier par un facteur alpha*)
	let mat = m#copy_matrix () in
	mat#dot alpha;
	mat;;

let create_matrix coefs shape = 
(*les coeffs sont donnés ligne par ligne dans une array à une dimension*)
	let l, c = shape in
	let m = Array.make_matrix l c 0. in
	for i = 0 to l-1 do
		for j = 0 to c-1 do
			m.(i).(j) <- coefs.(i*c+j)
		done
	done;
	new matrix m;;

let get_In n = 
(*renvoie l'élément neutre de Mn(R)*)
	let m = Array.make_matrix n n 0. in
	for i = 0 to n - 1 do
		m.(i).(i) <-1.
	done;
	new matrix m;;

let get_On n = 
(*renvoit l'élément nul de Mn(R)*)
	new matrix (Array.make_matrix n n 0.);;

let get_moveMatrix ax ay az sx sy sz = 
	(*ax ay az les angles d'Euler, sx sy sz les facteur de grossisement suivant les 3 axes*)
	let scale = create_matrix [|sx;0.;0.; 0.;sy;0.; 0.;0.;sz;|] (3,3) and
		rotationX = create_matrix [|1.;0.;0.; 0.;cos ax;-.sin ax; 0.;sin ax;cos ax;|] (3,3) and
		rotationY = create_matrix [|cos ay;0.;sin ay; 0.;1.;0.; -.sin ay;0.;cos ay;|] (3,3) and
		rotationZ = create_matrix [|cos az;-.sin az;0.; sin az;cos az;0.; 0.;0.;1.;|] (3,3) in
	matrix_product (matrix_product (matrix_product rotationZ rotationX) rotationY) scale;;


let reshape_array shape tableau = 
	if Array.length tableau != (fst shape) * (snd shape) then failwith "les dimensions ne sont pas compatible, nombre de coefficient incoherents - ERROR reshape_array";
	let m = Array.make_matrix (fst shape) (snd shape) 0. in
	for i = 0 to fst shape - 1 do
		for j = 0 to snd shape - 1 do
			m.(i).(j) <- tableau.(i * (snd shape) + j)
		done
	done;
	m;;

class vector coefficient = object (self)
	val dimension = Array.length coefficient
	inherit matrix (reshape_array ((Array.length coefficient), 1) coefficient) as super
	val mutable haveNorme = false
	val mutable norme = 0.

	method get_dimension () = dimension
	method get_coord i = super#get_coef i 0
	method set_coord i x = 
		super#set_coef i 0 x;
		haveNorme <- false
	method add_coord i x = 
		super#add_coef i 0 x;
		haveNorme <- false 
	method get_norme () = 
		if not haveNorme then begin
			haveNorme <- true;
			norme <- 0.;
			for i = 0 to n - 1 do
				norme <- self#get_coord i *. self#get_coord i +. norme
			done;
			norme <- norme**0.5
		end;
		norme
	method normalize () = (*rend le vecteur unitaire*)
			super#dot (1./.(self#get_norme ()));
			norme <- 1.;
			haveNorme <- true
	method copy_vector () = 
		let m = Array.make n 0. in
		for i = 0 to n - 1 do
			m.(i) <- self#get_coord i
		done;
		new vector m
end;;

	let get_vect_null n = let m = Array.make n 0. in new vector m;;

	let scalar v1 v2 =
		(*produit scalaire*)
		if v1#get_dimension () != v2#get_dimension () then failwith "les vecteurs n'ont pas la même dimension - ERROR scalar";
		let s = ref 0. in
		for i = 0 to 2 do
			s := !s +. (v1#get_coord i) *. (v2#get_coord i)
		done;
		!s;;

	let vector_sum opp v1 v2 = 
		(*permet d'ajouter ou soustraire deux vecteurs*)
		if v1#get_dimension () != v2#get_dimension () then failwith "Les vecteurs n'ont pas a même direction - ERROR vector_sum";
		let m = Array.make (v1#get_dimension ()) 0. in
		for i = 0 to v1#get_dimension () - 1 do
			m.(i) <- opp (v1#get_coef i) (v2#get_coef i)
		done;
		new vector m;;

	let vector_add v1 v2 = vector_sum (+.) v1 v2;;
	let vector_sub v1 v2 = vector_sum (-.) v1 v2;;

class vector3D x y z = object (self)
	inherit vector [|x; y; z|] as super

	method get_x () = super#get_coef 0 0
	method get_y () = super#get_coef 1 0
	method get_z () = super#get_coef 2 0
	method set_x x = super#set_coef 0 0 x
	method set_y y = super#set_coef 1 0 y
	method set_z z = super#set_coef 2 0 z
	method add_x dx = super#add_coef 0 0 dx
	method add_y dy = super#add_coef 1 0 dy
	method add_z dz = super#add_coef 2 0 dz
	method copy_vector3D () = new vector3D (self#get_x ()) (self#get_y ()) (self#get_z ())
end;;

let is_vector3D v = fst (v#get_shape ()) = 3 && snd (v#get_shape ()) = 1;;
let are_vector3D v1 v2 = fst (v1#get_shape ()) = fst (v2#get_shape ()) && snd (v1#get_shape ()) = snd (v2#get_shape ()) && is_vector3D v1;;

let vector3D_of_matrix v =
	if not (is_vector3D v) then failwith "Cette matrice n'est pas un vecteur 3D - ERROR vector3D_of_matrix";
	new vector3D (v#get_coef 0 0) (v#get_coef 1 0) (v#get_coef 2 0);;

let cross3D v1 v2 = 
	(*produit vectoriel*)
	if not (are_vector3D v1 v2) then failwith "Les vecteurs n'ont pas la bonne direction ou ne sont pas des vecteur 3D - ERROR cross3D";
	let x1 = v1#get_coef 0 0 and
		y1 = v1#get_coef 1 0 and
		z1 = v1#get_coef 2 0 and
		x2 = v2#get_coef 0 0 and
		y2 = v2#get_coef 1 0 and
		z2 = v2#get_coef 2 0 in
	let x = y1*.z2 -. z1*.y2 and
		y = z1*.x2 -. x1*.z2 and
		z = x1*.y2 -. y1*.x2 in
	new vector3D x y z;;

let vector_sum3D opp v1 v2 = 
	(*permet d'ajouter ou soustraire deux vecteurs*)
 	if not (are_vector3D v1 v2) then failwith "Les vecteurs n'ont pas a même direction ou ne sont pas des vecteur 3D - ERROR vector_sum3D";
	let x = opp (v1#get_coef 0 0) (v2#get_coef 0 0) and
		y = opp (v1#get_coef 1 0) (v2#get_coef 1 0) and
		z = opp (v1#get_coef 2 0) (v2#get_coef 2 0) in
	new vector3D x y z;;

let vector_add3D v1 v2 = vector_sum3D (+.) v1 v2;;
let vector_sub3D v1 v2 = vector_sum3D (-.) v1 v2;;

let vector_combi_linear3D a1 v1 a2 v2 = 
	(*permet d'ajouter ou soustraire deux vecteurs*)
 	if not (are_vector3D v1 v2) then failwith "Les vecteurs n'ont pas a même direction ou ne sont pas des vecteur 3D - ERROR vector_sum3D";
	let x = a1*.(v1#get_coef 0 0) +. a2*.(v2#get_coef 0 0) and
		y = a1*.(v1#get_coef 1 0) +. a2*.(v2#get_coef 1 0) and
		z = a1*.(v1#get_coef 2 0) +. a2*.(v2#get_coef 2 0) in
	new vector3D x y z;;

type move_matrixs3D = {rotation : matrix; translation : vector3D};;

let get_columne matrice = 
	let e1 = new vector3D (matrice#get_coef 0 0) (matrice#get_coef 1 0) (matrice#get_coef 2 0)
	and e2 = new vector3D (matrice#get_coef 0 1) (matrice#get_coef 1 1) (matrice#get_coef 2 1)
	and e3 = new vector3D (matrice#get_coef 0 2) (matrice#get_coef 1 2) (matrice#get_coef 2 2)
	in e1, e2, e3;;

class base_orthonorme3D (e1: vector3D) (e2: vector3D) (e3: vector3D) = object (self)
	val e1 = e1	
	val e2 = e2	
	val e3 = e3

	method get_e1 () = e1
	method get_e2 () = e2
	method get_e3 () = e3
	method get_return_matrix () = 
		let m = [|[|e1#get_x (); e2#get_x (); e3#get_x ()|];
				  [|e1#get_y (); e2#get_y (); e3#get_y ()|];
				  [|e1#get_z (); e2#get_z (); e3#get_z ()|]|] in
		new matrix m
	method get_passage_matrix () = 
		transpose_matrix (self#get_return_matrix ())
end;;

class repere3D (e1: vector3D) (e2: vector3D) (e3: vector3D) (origine: vector3D) = object
	inherit base_orthonorme3D e1 e2 e3 as super
	val mutable moveMatrix = {rotation = get_On 3; translation = origine}
	val origine = origine
	val mutable haveMoveMatrix = false

	method reset () = haveMoveMatrix <- false
	method get_origine () = origine
	method get_move_matrixs () = 
		if not haveMoveMatrix then begin
			haveMoveMatrix <- true;
			moveMatrix <- {rotation = super#get_passage_matrix (); translation = origine}
		end;
		moveMatrix
end;;

let move_vector3D vect move_matrixs = 
	let v = matrix_sub vect move_matrixs.translation in
	vector3D_of_matrix (matrix_product move_matrixs.rotation v);;