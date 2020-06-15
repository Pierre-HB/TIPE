(* class cube_bizote o n size material_ = object (self)
	inherit object3D () as super
	val radius = size
	val mutable position = o
	val faces = Array.make ((n-1)*(n-1)*12) (new face p p p eau)
	val points = Array.make (n*n*6) p
	val material = material_

	method render imageBuffer camera light renderSetup = super#render_light_point imageBuffer camera light renderSetup


	initializer
	let n_ = foi (n-1) 
	and n2 = n*n 
	and n2_ = (n-1)*(n-1) in
	let p = new point (new vector3D 0. 0. 0.) in
	(*les point  a l'interieur des faces*)
	let f0 = Array.make n2_ p 
	and f1 = Array.make n2_ p
	and f2 = Array.make n2_ p
	and f3 = Array.make n2_ p
	and f4 = Array.make n2_ p
	and f5 = Array.make n2_ p
	and a0 = Array.make (n-2) p
	and a1 = Array.make (n-2) p
	and a2 = Array.make (n-2) p
	and a3 = Array.make (n-2) p
	and a4 = Array.make (n-2) p
	and a5 = Array.make (n-2) p
	and a6 = Array.make (n-2) p
	and a7 = Array.make (n-2) p
	and a8 = Array.make (n-2) p
	and a9 = Array.make (n-2) p
	and a10 = Array.make (n-2) p
	and a11 = Array.make (n-2) p
	and e0 = ref p
	and e1 = ref p
	and e2 = ref p
	and e3 = ref p
	and e4 = ref p
	and e5 = ref p
	and e6 = ref p
	and e7 = ref p in
	(*
		e0	a0  e1   ^ z
		a3	f0  a1   |
		e3	a2  e2  y.-> x

		e3	a2  e1	 ^ y
		a11	f1  a10  |
		e7	a6  e6  zo->x

		e7	a6  e6  yo-> x
		a7	f2  a5   |
		e4	a4  e5   v z

		e4	a4  e5	z.->x
		a8	f3  a9 	 |
		e0	a0  e1	 v y

		e0	a3  e3
		a8	f4  a11 ->
		e4	a7  e7

		e0	a0  e1
		a3	f5  a1 ->
		e3	a2  e2










	*)

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
	
end;; *)


(* class cube_bizote o n size material_ = object (self)
	inherit object3D () as super
	val radius = size
	val mutable position = o
	val faces = Array.make ((n-1)*(n-1)*12) (new face p p p eau)
	val points = Array.make (6*n*n - 12*n + 8) p
	val material = material_

	method render imageBuffer camera light renderSetup = super#render_light_point imageBuffer camera light renderSetup


	initializer
	let n_ = foi (n-1) 
	and n2 = n*n 
	and n2_ = (n-1)*(n-1) in

	for x = 0 to n - 1 do
		for y = 0 to n - 2 do
			(*Face 1, facile*)
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius) (radius*.((2.*.(foi y) /. n_) -. 1.)) in
			points.(y*n + x) <- new point (vector_add3D v position);

			(*Face 2, moyen...*)
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi (n-1-y)) /. n_) -. 1.)) (radius) in
			points.(y*n + x + n*(n-1)) <- new point (vector_add3D v position);

			(*Face 3, moyen...*)
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (-.radius) (radius*.((2.*.(foi (n-1-y)) /. n_) -. 1.)) in
			points.(y*n + x + 2*n*(n-1)) <- new point (vector_add3D v position);

			(*Face 4, moyen...*)
			let v = new vector3D (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi y) /. n_) -. 1.)) (-.radius) in
			points.(y*n + x + 3*n*(n-1)) <- new point (vector_add3D v position);

		done;
	done;

	for x = 1 to n-2 do
		for y = 1 to n-2 do
			(*Face 5, Dur...*)
			let v = new vector3D (radius) (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi y) /. n_) -. 1.)) (-.radius) in
			points.((y-1)*n + x-1 + 4*n*(n-1)) <- new point (vector_add3D v position);

			(*Face 6, Dur...*)
			let v = new vector3D (-.radius) (radius*.((2.*.(foi x) /. n_) -. 1.)) (radius*.((2.*.(foi y) /. n_) -. 1.)) (-.radius) in
			points.((y-1)*n + x-1 + 4*n*(n-1) + (n-2)*(n-2)) <- new point (vector_add3D v position);
		done;
	done;
	let p face x y = match face with
		|0 -> y*n + x
		|1 -> y*n + x + n*(n-1)
		|2 -> y*n + x + n*(n-1)*2
		|3 -> y*n + x + n*(n-1)*3
		|4 -> begin
				match x with
				|0   -> y*n + x + n*(n-1)*2
				|n-1 ->
				|_   ->
		end



		|_ ->
	(*Face 5 et 6 a traiter separement*)


	(* for x = 0 to n - 1 do
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
	done; *)
	
end;; *)