(* This is a Brainfuck interpreter implemented in OCaml. *)

(* Load the brainfuck source file into a byte sequence. *)
let load_file f = 
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

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
    Trace.print_state i p c n;

    if i == 0 then
        0
    else
        let idx = i - 1 in
        match n, String.get s idx with
        | n, b when n > 0 || b != '[' ->
                (match String.get s idx with
                    | '[' ->    skip_loop_end s (idx) p c (n-1)
                    | ']' ->    Trace.print_state idx p c n;
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
    Trace.print_program_and_registers s c;
    match i with
    | x when x >= String.length s -> c; (* reached the end *)
    | _ ->
        Trace.print_state i p c n;

        let instr = String.get s i in
        match instr with
        | '>' ->    Trace.print_instruction "next register";
                    eval s (i + 1) (p+1) c n f
                    
        | '<' ->    Trace.print_instruction "previous register";
                    eval s (i + 1) (p-1) c n f
                    
        | '+' ->    Trace.print_instruction "increment";
                    Array.set c p ((Array.get c p) + 1);
                    eval s (i + 1) p c n f

        | '-' ->    Trace.print_instruction "decrement";
                    Array.set c p ((Array.get c p) - 1);
                    eval s (i + 1) p c n f
                    
        | '.' ->    Trace.print_instruction "output";
                    f (Array.get c p);
                    eval s (i+1) p c n f
                    
        | ',' ->    Trace.print_instruction "input";
                    eval s (i+1) p c n f
                    
        | '[' ->    Trace.print_instruction "'['";
                    (* If the current register value is zero
                        then skip to the loop-end +1 *)
                    let res = if (Array.get c p) == 0 then
                        skip_loop s i p c n
                    else
                        i
                    in
                    eval s (res+1) p c n f
                    
        | ']' ->    Trace.print_instruction "']'";
                    Trace.print_state i p c n;
                    if Array.get c p == 0 then
                        eval s (i+1) p c n f
                    else
                        let res = skip_loop_end s i p c n in
                        eval s (res+1) p c n f

        |  _   ->   (* ignore whitespace *)
                    Trace.print_invalid_instruction instr;
                    eval s (i + 1) p c n f

(* Create the program registers, start the evaluation, print the result. *)
let parse s = 
    Trace.print_introduction s;
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
