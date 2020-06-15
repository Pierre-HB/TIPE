#use "D:/documents/devoires/prepa/TIPE/final/OCaml/3D objects.ml";;

let dt = 0.5;;


class molecule position = object (self)
	val mutable p = position
	val mutable v = new vector3D 0. 0. 0.
	val mutable a = new vector3D 0. 0. 0.

	method set_acceleration a_ = a <- a_
	method set_vitesse v_ = v <- v_
	method set_position p_ = p <- p_
	method update dt = 
		v <- vector_combi_linear3D 1. v dt a;
		p <- vector_combi_linear3D 1. p dt v
	method get_position () = p
end;;



let pi = acos (-.1.);;

 let rot = get_moveMatrix 0. (2.*.pi/.4.) 0. 1. 1. 1.;;


let move = {rotation = rot; translation = new vector3D 0. 0. 0.};;


let projection_exmple_scene scene =
	let x = 3.
	and y = 1.5
	and z = 2. in
	let d = 1.5 in
	let epsilon = 0.01 in
	let y_ = (-.epsilon+.d)*.y/.x
	and z_ = (-.epsilon+.d)*.z/.x in

	let v1 = new fleche (new vector3D 0. 0. 0.) (new vector3D x 0. 0.) 10 0.3 0.15 0.05 (new red ()) true
	and v2 = new fleche (new vector3D 0. 0. 0.) (new vector3D 0. y 0.) 10 0.3 0.15 0.05 (new red ()) true
	and v3 = new fleche (new vector3D 0. 0. 0.) (new vector3D 0. 0. z) 10 0.3 0.15 0.05 (new red ()) true
	and e1 = new fleche (new vector3D 0. 0. 0.) (new vector3D 1. 0. 0.) 10 0.25 0.16 0.08(new black ()) true
	and e2 = new fleche (new vector3D 0. 0. 0.) (new vector3D 0. 1. 0.) 10 0.25 0.16 0.08(new black ()) true
	and e3 = new fleche (new vector3D 0. 0. 0.) (new vector3D 0. 0. 1.) 10 0.25 0.16 0.08(new black ()) true
	and v = new fleche (new vector3D 0. 0. 0.) (new vector3D x y z) 10 0.5 0.15 0.05(new green ()) true
	and obj = new sphere (new vector3D x y z) 10 0.2 (new dark_greenery ()) true
	and camera = new sphere (new vector3D 0. 0. 0.) 10 0.2 (new white ()) true
	and screen = new square (new vector3D 0. (-.d) 0.) 20 4. (new grey_ ()) true 
	and proj = new polygone (new vector3D (y_-.0.) (-.d+.epsilon) (z_)) 10 0.1 (new black ()) true in
	let rot = get_moveMatrix 0. 0. (pi/.2.) 1. 1. 1. in
	let move = {rotation = rot; translation = new vector3D 0. 0. 0.} in
	screen#move_original_position move;
	proj#move_original_position move;
	scene#add_object3D v1;
	scene#add_object3D v2;
	scene#add_object3D v3;
	scene#add_object3D e1;
	scene#add_object3D e2;
	scene#add_object3D e3;
	scene#add_object3D obj;
	scene#add_object3D v;
	scene#add_object3D screen;
	scene#add_object3D proj;
	scene#add_object3D camera;;

let perlin_exmple_scene scene =
	let vectors = [|new vector3D 1. 0. 0.; new vector3D (-.1.) 0. 0.; new vector3D 0. 0. 1.; new vector3D 0. 0. (-.1.); new vector3D 0.70710678118654757 0. 0.70710678118654757; new vector3D (-.0.70710678118654757) 0. 0.70710678118654757; new vector3D 0.70710678118654757 0. (-.0.70710678118654757); new vector3D (-.0.70710678118654757) 0. (-.0.70710678118654757)|] in
	let vectors_ = [|new vector3D 0.5 0. 0.; new vector3D (-.0.5) 0. 0.; new vector3D 0. 0. 0.5; new vector3D 0. 0. (-.0.5); new vector3D (0.70710678118654757/.2.) 0. (0.70710678118654757/.2.); new vector3D (-.0.70710678118654757/.2.) 0. (0.70710678118654757/.2.); new vector3D (0.70710678118654757/.2.) 0. (-.0.70710678118654757/.2.); new vector3D (-.0.70710678118654757/.2.) 0. (-.0.70710678118654757/.2.)|] in

	let x_ = 0.3
	and y_ = -.0.5 in
	let n = 5 
	and offset = 1 in
	for x = -n to n do
		for y = -n to n do
			let c = match x,y with
				|0, 0 -> new red ()
				|0, -1 -> new red ()
				|1, 0 -> new red ()
				|1, -1 -> new red ()
				|_ -> new white ()
			in
			let position = new vector3D (foi x) 0. (foi y) in

			let s = new sphere position 5 0.1 c true in
			scene#add_object3D s;
			
		done
	done;
	let s = new sphere (new vector3D x_ 0. y_) 5 0.05 (new blue ()) true 
	and v = new fleche (new vector3D 0. 0. 0.) (new vector3D x_ 0. y_) 5 0.15 0.05 0.02(new dark_greenery ()) true
	and screen = new square (new vector3D 0. 0. 0.) 5 (foi (2*(n+1))) (new grey ()) true in
	scene#add_object3D screen;
	scene#add_object3D s;;



let th () = 
	let start = Unix.gettimeofday () in
		
	let c = new cube (new vector3D (-.0.5) (-.1.) 0.) 10 1. (new green ()) true in

	let sph2 = new sphere (new vector3D (0.) (-.1.) (1.5)) 7 0.7 (new red ()) true in

 	let oce_physic, oce = generate_ocean 70 (new vector3D (0.) (-.0.8) 2.) 5. (new water ()) false dt 1234567890 [|3.5; 0.5|] [|1.; 0.2|] [|0.7; 0.35|] in

	let tet = -.0. in
	let e1 = new vector3D (cos tet) 0. (-.sin tet) in
	let e2 = new vector3D 0. 1. 0. in
	let e3 = new vector3D (sin tet) 0. (cos tet) in



	let ax = pi/.4. (*vers le bas*)
	and ay = 0. (*vers la droite*)
	and az = 0. in (*sens horaire*)
	let lookingMatrix = get_moveMatrix ax ay az 1. 1. 1. in
	let e1, e2, e3 = get_columne lookingMatrix in
	print_matrix lookingMatrix;
	print_float (lookingMatrix#get_determinant ());



	let o = new vector3D (0.) 2. (-.2.) in
	let camReper = new repere3D e1 e2 e3 o in
	let xSize = 1920/2
	and ySize = 1080/2 in
	let cam = create_camera xSize ySize 1. camReper in



	let s = new scene cam {renderMesh = false; renderMaterial = true; renderHidenFaces = true; saveImageUsingThreads = false} in
	let image = s#get_image () in
	image#set_background (create_color 10 50 150 1.);
	image#clear ();

	s#add_object3D oce;

	s#add_object3D sph2;




	s#add_light (new sunLight (new vector3D (-.1.) (-.1.) (-.1.)));

	let get_name () = 
		let i = ref 1 in
		while Sys.file_exists ("C:/Users/pierr/Desktop/graphic engine/test/"^string_of_int(!i)^".ppm") do
			incr i
		done;
		"C:/Users/pierr/Desktop/graphic engine/test/"^string_of_int(!i)^".ppm" in

	let longRecorded = false in

	if longRecorded then begin
		let record = create_ppm_names "C:/Users/pierr/Desktop/graphic engine/record" "wave_6" xSize ySize in
		let next = record.nextName in


		for i = 0 to 1200 do
			s#clear_image ();
			s#render ();

			s#export_to_ppm (next ());

		done;
	save_record record;
		
	end
	else begin
		print_string "\ninitialisation : ";
		print_float (Unix.gettimeofday () -. start);
		let startbis = Unix.gettimeofday () in

		s#render ();
		

		print_string "\nprojection : ";
		print_float (Unix.gettimeofday () -. startbis);
		print_string " s";
		let start2 = Unix.gettimeofday () in

		image#show_image ();

		print_string "\naffichage : ";
		print_float (Unix.gettimeofday () -. start2);
		print_string " s";
		let start3 = Unix.gettimeofday () in

		let name = get_name () in
		print_string "\n export to : ";
		print_string name;
		s#export_to_ppm name ;

		print_string "\nsauvegarde : ";
		print_float (Unix.gettimeofday () -. start3);
		print_string " s\n";
	end;;

th ();;
