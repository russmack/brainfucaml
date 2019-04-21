(* This is a Brainfuck interpreter implemented in OCaml. *)


(* Output switches, for debugging or curiosity. *)
let enable_print_state = false
let enable_print_program = false
let enable_print_registers = false
let enable_print_instruction = false
let enable_print_introduction = false
let enable_print_invalid_instruction = false

(* Load the brainfuck source file into a byte sequence. *)
let load_file f = 
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

(* Prints string and int, with optional newline bool. *)
let print_kv_int s i r = 
    print_string (s ^ ": ");
    print_int i;
    if r then
        print_newline ()

(* Prints program meta info. *)
let print_introduction s =
    if enable_print_introduction == true then (
        print_string s;
        print_newline ();
        print_endline "-----------------------";
        print_kv_int "string length" (String.length s) true;
    )

(* Prints all aspects of the machine state.*)
let print_state i p c n =
    match enable_print_state with
    | true  ->
        print_kv_int " | i" i true;
        print_kv_int " | p" p true;
        print_kv_int " | c[p]" (Array.get c p) true;
        print_kv_int " | n" n true
    | _     -> ()

(* Prints the instruction. *)
let print_instruction s =
    if enable_print_instruction == true then (
        print_string "[ ";
        print_endline s;
        print_string " ]";
        print_newline ();
    )

(* Prints an error if an invalid instruction is encountered. *)
let print_invalid_instruction instr =
    if enable_print_invalid_instruction == true then (
        print_string "invalid instruction: ";
        print_char '"';
        print_char instr;
        print_char '"';
        print_newline ();
    )

(* Print the registers. *)
let print_registers c =
    print_newline ();
    Array.iter (fun x -> print_int x; print_char ' ' ) c;
    print_newline ()

(* Print the program source and the registers. *)
let print_program_and_registers s c =
    if enable_print_program == true then
        print_endline s
    else () ;

    if enable_print_registers == true then
        print_registers c
    else ()

(* Skip to the end of the loop; loop counter has reached zero. *)
let rec skip_loop s i p c n =
    match n, String.get s i with
    | n, b when n > 0 && b != ']' -> 
        let idx = i + 1 in
        (match String.get s idx with
        | '[' ->    skip_loop s (idx+1) p c (n+1)
        | ']' -> skip_loop s (idx+1) p c (n-1)
        | _   -> skip_loop s (idx+1) p c n);
    | _, _ -> i

(* Skip back to the beginning of the loop. *)
let rec skip_loop_end s i p c n = 
    print_state i p c n;

    if i == 0 then
        0
    else
        let idx = i - 1 in
        match n, String.get s idx with
        | n, b when n > 0 || b != '[' ->
                (match String.get s idx with
                    | '[' ->    skip_loop_end s (idx) p c (n-1)
                    | ']' ->    print_state idx p c n;
                                skip_loop_end s (idx) p c (n+1)
                    | _   ->    skip_loop_end s (idx) p c n);
        | _ -> idx

(* Evaluate the source.
 * Parameters:  source, 
 *              instr_ptr, 
 *              register_ptr, 
 *              register_array, 
 *              loop_nest_level, 
 *              output_function 
 *)
let rec eval s i p c n f =  
    print_program_and_registers s c;
    match i with
    | x when x >= String.length s -> c; (* reached the end *)
    | _ ->
        print_state i p c n;

        let instr = String.get s i in
        match instr with
        | '>' ->    print_instruction "next register";
                    eval s (i + 1) (p+1) c n f
                    
        | '<' ->    print_instruction "previous register";
                    eval s (i + 1) (p-1) c n f
                    
        | '+' ->    print_instruction "increment";
                    Array.set c p ((Array.get c p) + 1);
                    eval s (i + 1) p c n f

        | '-' ->    print_instruction "decrement";
                    Array.set c p ((Array.get c p) - 1);
                    eval s (i + 1) p c n f
                    
        | '.' ->    print_instruction "output";
                    f (Array.get c p);
                    eval s (i+1) p c n f
                    
        | ',' ->    print_instruction "input";
                    eval s (i+1) p c n f
                    
        | '[' ->    print_instruction "'['";
                    (* If the current register value is zero
                        then skip to the loop-end +1 *)
                    let res = if (Array.get c p) == 0 then
                        skip_loop s i p c n
                    else
                        i
                    in
                    eval s (res+1) p c n f
                    
        | ']' ->    print_instruction "']'";
                    print_state i p c n;
                    if Array.get c p == 0 then
                        eval s (i+1) p c n f
                    else
                        let res = skip_loop_end s i p c n in
                        eval s (res+1) p c n f

        |  _   ->   (* ignore whitespace *)
                    print_invalid_instruction instr;
                    eval s (i + 1) p c n f

(* Create the program registers, start the evaluation, print the result. *)
let parse s = 
    print_introduction s;
    let c = Array.make 20 0 in
    let res = eval s 0 0 c 0 ( fun x -> print_char (char_of_int x)) in
    print_newline ();
    res

(* Program entry. *)
let run = 
    if Array.length Sys.argv == 2 then begin
        let filename = Sys.argv.(1) in
        let s = load_file filename in
        let _ = parse (Bytes.to_string s) in ()
    end
    else begin
        print_endline "Invalid number of arguments.";
        print_endline "Usage: ./brainfucaml <filename.bf>";
    end
