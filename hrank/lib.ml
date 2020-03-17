let input_test =
    let a = read_int() in
    let b = read_int() in
    print_int(a+b)

let hello_world =
    print_string("Hello World")

(* 
    Modified from submission
    from: print_string("Hello World\n");
    to: print_endline("Hello World");
 *)
let hello_world_n_times =
    let n = read_int() in
        for i = 1 to n do
            print_endline("Hello World");
        done
