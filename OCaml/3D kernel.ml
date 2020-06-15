#use "D:/documents/devoires/prepa/TIPE/final/OCaml/linear algebra kernel.ml";;
#use "D:/documents/devoires/prepa/TIPE/final/OCaml/images kernel advanced.ml";;


type camera = {mutable size_x : int; mutable size_y : int; ouverture : float; a : float; d : float; ad : float; repere3D : repere3D; cosO : float};;
type renderSetup = {renderMesh : bool; renderMaterial : bool; saveImageUsingThreads : bool; renderHidenFaces : bool};;

let create_camera size_x size_y ouverture repere3D = 
	(*size_x et size_y sont ne nombre de pixel horizontal et vertical de l'écran, ouverture est l'angle d'ouverture horizontal de la camera*)
	let a = (foi size_y) /. foi size_x and
		d = 1. /. tan(ouverture /. 2.) in
	(*a est le rapport entre la hauteur et la largeur de l'écran, d est la distance entre le point de projection et le plan de projection de la camera virtuelle*)
	let ad = a*.d 
	and ouvertureY = 2. *. asin (a *. sin (ouverture/.2.)) in
	(*la composante ad permet a la camera virtuelle devoir une surface carré et de l'afficher sur un ecran rectangulaire*)
	{size_x = size_x; size_y = size_y; ouverture = ouverture; a = a; d = d; ad = ad; repere3D = repere3D; cosO = max (cos ouverture) (cos ouvertureY)};;

let projection_on_screen vector camera = 
	let x = vector#get_x () and
		y = vector#get_y () and
		z = vector#get_z () in
	(*les coordoné x y z sont dans la base relative a la camera!*)
	(*On aplique Thales, si le point est sur l'écran on obtien alors une valeur entre -1 et 1*)

	(*ON EFFECTUE LA PROJECTION DANS UN REPERT QUI N'EST PAS DIRECT, MAIS CELA N'AFFECTE QUE LA PROJECTION*)
	let proj_x = iof ((x*.camera.ad/.z +. 1.) *. foi camera.size_x /. 2.) and
		proj_y = iof ((y*.camera.d/.z +. 1.) *. foi camera.size_y /. 2.) in
	proj_x, proj_y;;


let intersection_plan_droite_optimiser d directionDroite ptDroite =
	(*pour le plan perpendiculaire à z (dans la base (x, y, z)) contenant le point (0,0,d)*)
	let nv = directionDroite#get_z () in
	if nv = 0. then failwith "le plan et la droite ne se coupe pas. - ERROR intersection_plan_droite_optimiser";
	let v_ = directionDroite#copy_vector3D () and
		z = (d -. ptDroite#get_z ()) /. nv in
	v_#dot z;
	vector_add3D ptDroite v_;;

class virtual material () = object (self)
	val virtual originalColor : color
	val virtual meshColor : color
	val virtual light_effect : float
	val virtual light_reflexion_effect : float
	val virtual mutable tempColor : color
	val pid = get_id ()
	val virtual rAbsorbance : float
	val virtual gAbsorbance : float
	val virtual bAbsorbance : float

	method reset () = tempColor <- originalColor
	method mix_light lightColor lightProportion =
		let light = {r = lightColor.r *. (1.-.rAbsorbance); g = lightColor.g *. (1.-.gAbsorbance); b = lightColor.b *. (1.-.bAbsorbance); a = originalColor.a} in
		tempColor <- mix_color tempColor light lightProportion
	method add_light lightColor illumination = 
		self#mix_light lightColor  (1. -. illumination *. light_effect)

	method add_reflexion lightColor illumination = 
		self#mix_light lightColor  (1. -. illumination *. light_reflexion_effect);

	method get_color () = tempColor 
	method get_meshColor () = meshColor
	method get_original_color () = originalColor 
	method get_id () = pid

end;;

class point position = object (self)
	val mutable originalPosition = position
	val mutable faceListe = []
	val mutable currentPosition = position#copy_vector3D ()
	val mutable haveMoved = false
	val mutable haveNormal = false
	val mutable normal = new vector3D 0. 0. 0.
	val mutable proj_x = 0
	val mutable proj_y = 0
	val mutable isProjected = false

	method move mat = if not haveMoved then begin 
			haveMoved <- true;
			currentPosition <- move_vector3D originalPosition mat
		end
	method get_projection camera = 
		if not isProjected then begin
			isProjected <- true;
			let (a, b) = projection_on_screen (self#get_position ()) camera in
			proj_x <- a;
			proj_y <- b
		end;
		proj_x, proj_y
	method reset () = 
		haveMoved <- false;
		haveNormal <- false;
		isProjected <- false
	method get_normal () = 
		if not haveNormal then begin
			haveNormal <- true;
			normal <- new vector3D 0. 0. 0.;
			foreach_list faceListe (fun face -> normal <- vector_add3D normal (face#get_normal ()));
			normal#normalize ()
		end;
		normal
	method get_fixed_position () = originalPosition 
	method get_position () = if haveMoved then currentPosition else originalPosition
	method add_face (face : face) = faceListe <- face::faceListe
	method move_original_position movesMatrix = 
		originalPosition <- move_vector3D originalPosition movesMatrix;
		self#reset ()

end
and	face p1 p2 p3 material = object (self)
	val p1 = p1
	val p2 = p2
	val p3 = p3
	val material = material
	val mutable haveNormal = false
	val mutable normal = new vector3D 0. 0. 0.
	val mutable speedDistance = 0.

	method get_normal () = 
		(*les normal sont calculer dans l'environnement canonique, PAS dans la base de la camera*)
		if not haveNormal then begin
			haveNormal <- true;
			let v1 = vector_sub3D (p2#get_fixed_position ()) (p1#get_fixed_position ())
			and v2 = vector_sub3D (p3#get_fixed_position ()) (p1#get_fixed_position ()) in
			normal <- cross3D v1 v2;
			normal#normalize ()
		end;
		normal
	method draw_face_on_screen_no_interpolation (imageBuffer : imageBuffer) camera (light : material -> vector3D -> vector3D -> vector3D -> vector3D -> camera -> unit ) renderSetup =
		if renderSetup.renderHidenFaces || scalar (vector_sub3D (camera.repere3D#get_origine ()) (p1#get_fixed_position ())) (self#get_normal ()) <= 0. then begin
			if renderSetup.renderMaterial then begin
				material#reset ();
				light material (p1#get_fixed_position ()) (p2#get_fixed_position ()) (p3#get_fixed_position ()) (self#get_normal ()) camera; (*applique la lumiere au materiel de la face*)
			end;
			let moveMatrix = camera.repere3D#get_move_matrixs () in
			p1#move moveMatrix;
			p2#move moveMatrix;
			p3#move moveMatrix;

			let outOfFieldPoint = ref 0 
			and z1 = (p1#get_position ())#get_z () and z2 = (p2#get_position ())#get_z () and z3 = (p3#get_position ())#get_z () in
			if z1 < camera.d then incr outOfFieldPoint;
			if z2 < camera.d then incr outOfFieldPoint;
			if z3 < camera.d then incr outOfFieldPoint;

			let epsilon = 0.01 in

			match !outOfFieldPoint with
				|0 -> let x1, y1 = p1#get_projection camera
					and x2, y2 = p2#get_projection camera
					and x3, y3 = p3#get_projection camera in

					if renderSetup.renderMesh then imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z1-.epsilon; pid = -2} {c = material#get_meshColor (); z = z2-.epsilon; pid = -2} {c = material#get_meshColor (); z = z3-.epsilon; pid = -2};

					if renderSetup.renderMaterial then imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = material#get_color (); z = z1; pid = material#get_id ()} {c = material#get_color (); z = z2; pid = material#get_id ()} {c = material#get_color (); z = z3; pid = material#get_id ()};
					(* if renderSetup.renderMesh then imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}; *)


				|1 -> let p1_ = ref p1 and p2_ = ref p2 and p3_ = ref p3 in
					if z1 < camera.d then switch p1_ p3_;
					if z2 < camera.d then switch p2_ p3_;
					(*c'est alors le point p3_ qui est deriere la camera*)
					let i1 = intersection_plan_droite_optimiser camera.d (vector_sub3D (!p1_#get_position ()) (!p3_#get_position ()) ) (!p1_#get_position ())
					and i2 = intersection_plan_droite_optimiser camera.d (vector_sub3D (!p2_#get_position ()) (!p3_#get_position ()) ) (!p2_#get_position ()) in
					let x1, y1 = (!p1_)#get_projection camera
					and x2, y2 = (!p2_)#get_projection camera
					and x3, y3 = projection_on_screen i1 camera
					and x4, y4 = projection_on_screen i2 camera in

					if renderSetup.renderMaterial then begin
						imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = material#get_color (); z = z1; pid = material#get_id ()} {c = material#get_color (); z = z2; pid = material#get_id ()} {c = material#get_color (); z = camera.d; pid = material#get_id ()};
						imageBuffer#fill_triangle_gradient x2 y2 x3 y3 x4 y4 {c = material#get_color (); z = z2; pid = material#get_id ()} {c = material#get_color (); z = camera.d; pid = material#get_id ()} {c = material#get_color (); z = camera.d; pid = material#get_id ()}
					end;
					if renderSetup.renderMesh then begin
						imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z1-.epsilon; pid = -2} {c = material#get_meshColor (); z = z2-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2};
						imageBuffer#draw_triangle_gradiant x2 y2 x3 y3 x4 y4 {c = material#get_meshColor (); z = z1-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2}
						(* imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()};
						imageBuffer#draw_triangle x2 y2 x3 y3 x4 y4 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}
					 *)end

				|2 -> let p1_ = ref p1 and p2_ = ref p2 and p3_ = ref p3 in
					if z1 >= camera.d then switch p1_ p3_;
					if z2 >= camera.d then switch p2_ p3_;
					(*Alors p3_ est le seul point face a la camera*)
					let i1 = intersection_plan_droite_optimiser camera.d (vector_sub3D (!p3_#get_position ()) (!p1_#get_position ()) ) (!p3_#get_position ())
					and i2 = intersection_plan_droite_optimiser camera.d (vector_sub3D (!p3_#get_position ()) (!p2_#get_position ()) ) (!p3_#get_position ()) in
					let x1, y1 = (!p3_)#get_projection camera
					and x2, y2 = projection_on_screen i1 camera
					and x3, y3 = projection_on_screen i2 camera in

					if renderSetup.renderMaterial then imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = material#get_color (); z = z3; pid = material#get_id ()} {c = material#get_color (); z = camera.d; pid = material#get_id ()} {c = material#get_color (); z = camera.d; pid = material#get_id ()};
					(* if renderSetup.renderMesh then imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}; *)
					if renderSetup.renderMesh then imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z3-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2};
  
				|_ -> ()
		end

	method draw_face_on_screen_with_interpolation (imageBuffer : imageBuffer) camera (light : material -> vector3D -> vector3D -> camera -> unit ) renderSetup =
		if renderSetup.renderHidenFaces || scalar (vector_sub3D (camera.repere3D#get_origine ()) (p1#get_fixed_position ())) (self#get_normal ()) <= 0. then begin
			let c1 = ref green and c2 = ref green and c3 = ref green in
			(*on initialise les couleur au trois angles*)
			if renderSetup.renderMaterial then begin
				material#reset ();
				light material (p1#get_fixed_position ()) (p1#get_normal ()) camera;
				c1 := material#get_color ();
				material#reset ();
				light material (p2#get_fixed_position ()) (p2#get_normal ()) camera;
				c2 := material#get_color ();
				material#reset ();
				light material (p3#get_fixed_position ()) (p3#get_normal ()) camera;
				c3 := material#get_color ();

			end;
			let moveMatrix = camera.repere3D#get_move_matrixs () in
			p1#move moveMatrix;
			p2#move moveMatrix;
			p3#move moveMatrix;

			let outOfFieldPoint = ref 0 
			and z1 = (p1#get_position ())#get_z () and z2 = (p2#get_position ())#get_z () and z3 = (p3#get_position ())#get_z () in
			if z1 < camera.d then incr outOfFieldPoint;
			if z2 < camera.d then incr outOfFieldPoint;
			if z3 < camera.d then incr outOfFieldPoint;

			let epsilon = 0.01 in

			match !outOfFieldPoint with
				|0 -> let x1, y1 = p1#get_projection camera
					and x2, y2 = p2#get_projection camera
					and x3, y3 = p3#get_projection camera in
					if renderSetup.renderMesh then imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z1-.epsilon; pid = -2} {c = material#get_meshColor (); z = z2-.epsilon; pid = -2} {c = material#get_meshColor (); z = z3-.epsilon; pid = -2};

					if renderSetup.renderMaterial then imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = !c1; z = z1; pid = material#get_id ()} {c = !c2; z = z2; pid = material#get_id ()} {c = !c3; z = z3; pid = material#get_id ()};
					(* if renderSetup.renderMesh then imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}; *)


				|1 -> let p1_ = ref p1 and p2_ = ref p2 and p3_ = ref p3 in
					if z1 < camera.d then switch p1_ p3_;
					if z2 < camera.d then switch p2_ p3_;
					(*c'est alors le point p3_ qui est deriere la camera*)
					let v31 = vector_sub3D (!p1_#get_position ()) (!p3_#get_position ())
					and v32 = vector_sub3D (!p2_#get_position ()) (!p3_#get_position ()) in
					let i1 = intersection_plan_droite_optimiser camera.d v31 (!p1_#get_position ())
					and i2 = intersection_plan_droite_optimiser camera.d v32 (!p2_#get_position ()) in
					let x1, y1 = (!p1_)#get_projection camera
					and x2, y2 = (!p2_)#get_projection camera
					and x3, y3 = projection_on_screen i1 camera
					and x4, y4 = projection_on_screen i2 camera in

					let v31_ = vector_sub3D (!p3_#get_position ()) i1
					and v32_ = vector_sub3D (!p3_#get_position ()) i2 in
					let s1 =  (v31_#get_norme ()) /. (v31#get_norme ())
					and s2 =  (v32_#get_norme ()) /. (v32#get_norme ()) in
					let c31 = mix_color !c3 !c1 s1
					and c32 = mix_color !c3 !c2 s2 in 

					if renderSetup.renderMaterial then begin
						imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = !c1; z = z1; pid = material#get_id ()} {c = !c2; z = z2; pid = material#get_id ()} {c = c31; z = camera.d; pid = material#get_id ()};
						imageBuffer#fill_triangle_gradient x2 y2 x3 y3 x4 y4 {c = !c2; z = z2; pid = material#get_id ()} {c = c31; z = camera.d; pid = material#get_id ()} {c = c32; z = camera.d; pid = material#get_id ()}
					end;
					if renderSetup.renderMesh then begin
						imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z1-.epsilon; pid = -2} {c = material#get_meshColor (); z = z2-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2};
						imageBuffer#draw_triangle_gradiant x2 y2 x3 y3 x4 y4 {c = material#get_meshColor (); z = z2-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2}
					
						(* imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()};
						imageBuffer#draw_triangle x2 y2 x3 y3 x4 y4 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}
					 *)end

				|2 -> let p1_ = ref p1 and p2_ = ref p2 and p3_ = ref p3 in
					if z1 >= camera.d then switch p1_ p3_;
					if z2 >= camera.d then switch p2_ p3_;
					(*Alors p3_ est le seul point face a la camera*)
					let v31 = vector_sub3D (!p3_#get_position ()) (!p1_#get_position ())
					and v32 = vector_sub3D (!p3_#get_position ()) (!p2_#get_position ()) in
					let i1 = intersection_plan_droite_optimiser camera.d v31 (!p3_#get_position ())
					and i2 = intersection_plan_droite_optimiser camera.d v32 (!p3_#get_position ()) in
					let x1, y1 = (!p3_)#get_projection camera
					and x2, y2 = projection_on_screen i1 camera
					and x3, y3 = projection_on_screen i2 camera in

					let v31_ = vector_sub3D (!p3_#get_position ()) i1
					and v32_ = vector_sub3D (!p3_#get_position ()) i2 in
					let s1 =  (v31_#get_norme ()) /. (v31#get_norme ())
					and s2 =  (v32_#get_norme ()) /. (v32#get_norme ()) in
					let c31 = mix_color !c3 !c1 s1
					and c32 = mix_color !c3 !c2 s2 in

					if renderSetup.renderMaterial then imageBuffer#fill_triangle_gradient x1 y1 x2 y2 x3 y3 {c = !c1; z = z3; pid = material#get_id ()} {c = c31; z = camera.d; pid = material#get_id ()} {c = c32; z = camera.d; pid = material#get_id ()};
					(* if renderSetup.renderMesh then imageBuffer#draw_triangle x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = 0.; pid = material#get_id ()}; *)
					if renderSetup.renderMesh then imageBuffer#draw_triangle_gradiant x1 y1 x2 y2 x3 y3 {c = material#get_meshColor (); z = z3-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2} {c = material#get_meshColor (); z = camera.d-.epsilon; pid = -2};
  
				|_ -> ()
		end

	method get_average_position () = vector_add3D (vector_add3D (p1#get_fixed_position ()) (p2#get_fixed_position ())) (p3#get_fixed_position ())
	method comput_speedDistance (origine : vector3D) = 
		let v = self#get_average_position () in
		let dx = v#get_x () -. origine#get_x ()
		and dy = v#get_y () -. origine#get_y ()
		and dz = v#get_z () -. origine#get_z () in
		speedDistance <- dx*.dx +. dy*.dy +. dz*.dz
	method get_speedDistance () = speedDistance

	method reset () =
		material#reset;
		p1#reset ();
		p2#reset ();
		p3#reset ();
		haveNormal <- false
	initializer
		p1#add_face self;
		p2#add_face self;
		p3#add_face self
end;;

class virtual light () = object (self)

	method virtual apply_light_face : material -> vector3D -> vector3D -> vector3D -> vector3D -> camera -> unit
	(*material p1 p2 p3 normal = () *)	
	method virtual apply_light_point : material -> vector3D -> vector3D -> camera -> unit
	(*material p normal*)
	
	(*applique la lumiere sur le materielle de la face en prennant en conte toutes les donné disponible sur la face*)
end;;

let rec quick_sort liste tri debut fin =
	if fin - debut >=1 then begin
    	let pivot = liste.(debut) in
    	let sup = ref (fin-1) and
    	inf = ref debut in
    	let temp = ref liste.(!sup) in
    	for i = 0 to fin-debut-2 do
    	    if tri !temp pivot then begin
    	        liste.(!inf) <- !temp;
    	        incr inf;
    	        temp := liste.(!inf)
    	   	 	end
    	    else begin
    	        liste.(!sup) <- !temp;
    	        decr sup;
    	        temp := liste.(!sup)
    	    	end
    	    done;
    	liste.(!inf) <- pivot;
    	if !inf - debut > 1 then quick_sort liste tri debut !inf;
    	if fin - !sup > 1 then quick_sort liste tri (!sup+1) fin
	end;;

class virtual object3D () = object (self)
	val virtual faces : face array
	val virtual points : point array
	val virtual mutable position : vector3D
	val virtual radius : float
	val virtual material : material

	method get_faces () = faces
	method get_points () = points
	method is_in_front_camera camera =
		let v = vector_sub3D position (camera.repere3D#get_origine ()) in
		let distance = v#get_norme () in
		v#normalize ();
		let s = scalar v (camera.repere3D#get_e3 ()) in
		if distance != 0. then s > camera.cosO -. radius/. distance && s > 0. else true;


	method apply_light_face (lights : light list) material p1 p2 p3 normal camera =
		foreach_list lights (fun light -> light#apply_light_face material p1 p2 p3 normal camera)
	method apply_light_point (lights : light list) material p normal camera =
		foreach_list lights (fun light -> light#apply_light_point material p normal camera)	


	method render_light_face (imageBuffer : imageBuffer) camera (lights : light list) renderSetup =
		if self#is_in_front_camera camera || renderSetup.renderHidenFaces then begin
			let lightComputer = self#apply_light_face lights in
			foreach_array faces (fun face -> face#draw_face_on_screen_no_interpolation imageBuffer camera lightComputer renderSetup)
		end;
	

	method render_light_point (imageBuffer : imageBuffer) camera (lights : light list) renderSetup =
		if self#is_in_front_camera camera || renderSetup.renderHidenFaces then begin
			let lightComputer = self#apply_light_point lights in
			foreach_array faces (fun face -> face#draw_face_on_screen_with_interpolation imageBuffer camera lightComputer renderSetup)
		end;

	method reset () =
		foreach_array faces (fun face -> face#reset ())

	method move_original_position movesMatrix =
		foreach_array points (fun point -> point#move_original_position movesMatrix);
		position <- move_vector3D position movesMatrix

	method virtual render : imageBuffer -> camera -> light list -> renderSetup -> unit
end;;

class scene camera renderSetup = object (self)
	val virtual mutable object3DListe : object3D list
	val mutable object3DListe = []
	val mutable lightListe = []
	val image = new imageBuffer camera.size_x camera.size_y (create_color 0 10 0 1.)
	val camera = camera
	val renderSetup = renderSetup


	method render () =
		foreach_list object3DListe (fun object3D -> object3D#render image camera lightListe renderSetup);
		foreach_list object3DListe (fun object3D -> object3D#reset ())
	method add_object3D obj = object3DListe <- obj::object3DListe
	method add_light light = lightListe <- light::lightListe
	method get_image () = image
	method export_to_ppm file = image#export_to_ppm file renderSetup.saveImageUsingThreads
	method clear_image () = image#clear ()
end;;