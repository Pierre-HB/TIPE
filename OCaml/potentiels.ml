#use "C:/Users/pierr/Desktop/graphic engine/3D objects.ml";;

let width = 700
and height = 500;;

let create_sources n = 
	let temp = Array.make_matrix n 2 0 in
	for i = 0 to n-1 do
		temp.(i).(0) <- Random.int width ;
		temp.(i).(1) <- Random.int height 
	done;
	temp;;

let a = 0.4
and r = 50.
and t = 0.9
and distance x y = sqrt(x*.x +. y*.y);;

let sources = create_sources 150;;

	
let potentiel x y = 
	let s = ref 0. in
	let n = Array.length sources in
	for i = 0 to n-1 do
		let temp = distance (float_of_int (x-sources.(i).(0))) (float_of_int (y-sources.(i).(1))) in
		if temp < r then begin
			let t = temp*.temp/.(r*.r) in
			s := !s +. 1. -. 3.*.t*.t +.2.*.t*.t*.t
			end
	done;
	!s -. t;;
	
let potentiel x y = 
	let s = ref 0. in
	let n = Array.length sources in
	for i = 0 to n-1 do
		let temp = distance (float_of_int (x-sources.(i).(0))) (float_of_int (y-sources.(i).(1))) in
		if temp < r then begin
			let t = 1. -. temp*.temp/.(r*.r) in
			s := !s +. t*.t*.t*.t*.t
			end
	done;
	!s -. t;;
	
	

	
let img = new imageBuffer width height black in
for x = 0 to width-1 do
	for y = 0 to height-1 do
		if potentiel x y > 0. then img#draw_pixel x y {c = white; z = 0.; pid = 0}
	done
done;
img#show_image ();
img#export_to_ppm "C:\\Users\\pierr\\Desktop\\graphic engine\\potentiel.ppm" false;;
