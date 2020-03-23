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

(*

*)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec f n arr = (*Complete this function*)
    for i = 1 to n do print_endline(arr); done;
    for i = 1 to n do 
    match arr with
    | [] -> 0
    | h::t -> f n t


let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;