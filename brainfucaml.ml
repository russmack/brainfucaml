
let filename = "helloworld.bf"

let load_file f = 
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s);;

let eval = function 
    | '>' -> print_endline "next register"
    | '<' -> print_endline "previous register"
    | '+' -> print_endline "increment"
    | '-' -> print_endline "decrement"
    | '.' -> print_endline "output"
    | ',' -> print_endline "input"
    | '[' -> print_endline "start loop"
    | ']' -> print_endline "end loop"
    |  _   -> print_endline "invalid instruction"
;;

let parse s = 
    print_endline "the next function: eval byte";
    String.iter (fun i -> eval i) s
;;

    

let () = 
    let s = load_file filename in
    parse s;;
