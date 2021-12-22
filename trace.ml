(* Prints string and int, with optional newline bool. *)
let print_kv_int s i r = 
    print_string (s ^ ": ");
    print_int i;
    if r then
        print_newline ()

(* Prints program meta info. *)
let print_introduction s =
    if Config.enable_print_introduction == true then (
        print_string s;
        print_newline ();
        print_endline "-----------------------";
        print_kv_int "string length" (String.length s) true;
    )

(* Prints all aspects of the machine state.*)
let print_state i p c n =
    match Config.enable_print_state with
    | true  ->
        print_kv_int " | i" i true;
        print_kv_int " | p" p true;
        print_kv_int " | c[p]" (Array.get c p) true;
        print_kv_int " | n" n true
    | _     -> ()

(* Prints the instruction. *)
let print_instruction s =
    if Config.enable_print_instruction == true then (
        print_string "[ ";
        print_endline s;
        print_string " ]";
        print_newline ();
    )

(* Prints an error if an invalid instruction is encountered. *)
let print_invalid_instruction instr =
    if Config.enable_print_invalid_instruction == true then (
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
    if Config.enable_print_program == true then
        print_endline s
    else () ;

    if Config.enable_print_registers == true then
        print_registers c
    else ()


