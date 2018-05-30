(* Read a file line by line *)
let read_file  = 
	let chan = open_in "test.txt" in
	try
	  while true; do
	    let line = input_line chan in
	    print_endline line;
	  done; 
	with End_of_file ->
	  close_in chan;;