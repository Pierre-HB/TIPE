open Graphics;;
#load "graphics.cma";;

let create_sources n = 
	let temp = Array.make_matrix n 2 0 in
	for i = 0 to n-1 do
		temp.(i).(0) <- Random.int 500;
		temp.(i).(1) <- Random.int 500
	done;
	temp;;

let a = 0.4
and r = 50.
and t = 0.9
and distance x y = sqrt(x*.x +. y*.y);;

let sources = [|[|300; 300|]; [|190; 190|]; [|0; 0|]; [|0; 0|]|];;
let sources = create_sources 200;;

let potentiel x y = 
	let s = ref 0. in
	let n = Array.length sources in
	for i = 0 to n-1 do
		let temp = distance (float_of_int (x-sources.(i).(0))) (float_of_int (y-sources.(i).(1))) in
		if temp < r then
			s := !s +. 1. -. ((temp*.temp/.(r*.r)))**a
	done;
	!s -. t;;
	
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

	
	
Graphics.open_graph "";
for x = 0 to 499 do
	for y = 0 to 499 do
		if potentiel x y < 0. then Graphics.set_color Graphics.green
		else Graphics.set_color Graphics.red ;
		Graphics.plot x y
	done
done;
Graphics.set_color Graphics.white;	
for i = 0 to Array.length sources - 1 do
	Graphics.fill_circle sources.(i).(0) sources.(i).(1) 3
	done;;