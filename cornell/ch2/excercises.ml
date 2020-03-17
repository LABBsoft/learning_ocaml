(*  1. values - *
  
  What are the types/values of the following expressions
  7 * (1+2+3) -> int 42
  "CS " ^ string_of_int 3110 -> string "CS 3110"
*)

(* 2. operators - **
  
  Write an expression that multiplies 42 by 10.
  42 * 10
  Write an expression that divides 3.14 by 2.0. Hint: integer and floating-point operators are written differently in OCaml.
  3.14 /. 2.0
  Write an expression that computes 4.2 raised to the seventh power.
  let rec pow x y =
    if y=0 then 1 
    else x * pow x (y-1)
*)

(* 3. equality - *

  Write an expression that compares 42 to 42 using structural equality.
  42 = 42 - true
  Write an expression that compares "hi" to "hi" using structural equality. What is the result?
  "hi" = "hi" - false
  Write an expression that compares "hi" to "hi" using physical equality. What is the result?
  "hi" == "hi" - true
*)

(* 4. assert - *
  
  Enter assert true;; into utop and see what happens.
  assert true;; -> - : unit = ()
  Enter assert false;; into utop and see what happens.
  assert false;; -> Exception: Assert_failure ("//toplevel//", 1, 0).
  Write an expression that asserts 2110 is not (structurally) equal to 3110.
  assert (2110<>3110);;
*)

(* 5. if - *

  Write an if expression that evaluates to 42 if 2 is greater than 1 and otherwise evaluates to 7.
  if 2 > 1 then 42 else 7;;
*)

(* 6. double fun - *

  Using the increment function from above as a guide, 
  define a function double that multiplies its input by 2. 
  For example, double 7 would be 14. 
  Test your function by applying it to a few inputs. 
  Turn those test cases into assertions.

  let dub x = x*2;;
  assert (dub 2 = 4);;
  assert (dub 4 = 8);;

*)

(* 7. more fun - **

Define a function that computes the cube of a floating-point number. Test your function by applying it to a few inputs.
Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.
Define a function that computes the area of a circle given its radius. Test your function with assert.
For the latter, bear in mind that floating-point arithmetic is not exact. Instead of asserting an exact value, you should assert that the result is "close enough", e.g., within 1e-5. If that's unfamiliar to you, it would be worthwhile to read up on floating-point arithmetic.

A function that take multiple inputs can be defined just by providing additional names for those inputs as part of the let definition. For example, the following function computes the average of three arguments:

let avg3 x y z =
  (x +. y +. z) /. 3.

*)
let cube x = x *. x *. x

let sign x =
  if x < 0 then -1
  else if x = 0 then 0
  else 1

let area r = 3.14 *. r *. r


(* Exercise: RMS [✭✭]
Define a function that computes the root-mean-square of two numbers—i.e.,  (x2+y2)/2‾‾‾‾‾‾‾‾‾‾√ . Test your function with assert.
*)
let rms x y =
  sqrt((x *. x + y *. y) /. 2)

(*
Exercise: date fun [✭✭✭]
Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date. Here, a valid date has a month that is one of the following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number of days in that month, inclusive. For example, if the month is Jan, then the day is between 1 and 31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive.

How terse (i.e., few and short lines of code) can you make your function? You can definitely do this in fewer than 12 lines.

Exercise: editor tutorial [✭✭✭]
Which editor you use is largely a matter of personal preference. Atom, Sublime, and Komodo all provide a modern GUI. Emacs and Vim are more text-based. If you've never tried Emacs or Vim, why not spend 10 minutes with each? There are good reasons why they are beloved by many programmers.

To get started with learning Vim, run vimtutor -g.
To get started with learning Emacs, run emacs then press C-h t, that is, Control+H followed by t.
Exercise: master an editor [✭✭✭✭✭, advanced]
You'll be working on this exercise for the rest of your career! Try not to get caught up in any editor wars.

Exercise: fib [✭✭]
Define a recursive function fib : int -> int, such that fib n is the nth number in the Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13, ... That is:

fib 1 = 1,

fib 2 = 1, and

fib n = fib (n-1) + fib (n-2) for any n > 2.

Test your function in the toplevel.

Exercise: fib fast [✭✭✭]
How quickly does your implementation of fib compute the 50th Fibonacci number? If it computes nearly instantaneously, congratulations! But the recursive solution most people come up with at first will seem to hang indefinitely. The problem is that the obvious solution computes subproblems repeatedly. For example, computing fib 5 requires computing both fib 3 and fib 4, and if those are computed separately, a lot of work (an exponential amount, in fact) is being redone.

Create a function fib_fast that requires only a linear amount of work. Hint: write a recursive helper function h : int -> int -> int -> int, where h n pp p is defined as follows:

h 1 pp p = p, and

h n pp p = h (n-1) p (pp+p) for any n > 1.

The idea of h is that it assumes the previous two Fibonacci numbers were pp and p, then computes forward n more numbers. Hence, fib n = h n 0 1 for any n > 0.

What is the first value of n for which fib_fast n is negative, indicating that integer overflow occurred?

Exercise: poly types [✭✭✭]
What is the type of each of the functions below? You can ask the toplevel to check your answers.

let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y
Exercise: divide [✭✭]
Write a function divide : numerator:float -> denominator:float -> float. Apply your function.

Exercise: associativity [✭✭]
Which of the following produces an integer, which produces a function, and which produces an error? Decide on an answer, then check your answer in the toplevel.

add 5 1
add 5
(add 5) 1
add (5 1)
Exercise: average [✭✭]
Define an infix operator +/. to compute the average of two floating-point numbers. For example,

1.0 +/. 2.0 = 1.5
0. +/. 0. = 0.
Exercise: hello world [✭]
Type the following in utop:

# print_endline "Hello world!";;
# print_string "Hello world!";;
Notice the difference in output from each.

□

Exercise: print int list rec [✭✭]
Write a function print_int_list : int list -> unit that prints its input list, one number per line. For example, print_int_list [1;2;3] should result in this output:

1
2
3
Here is some code to get you started:

let rec print_int_list = function 
| [] -> () 
| h::t -> (* fill in here *); 
          print_int_list t
□

Exercise: print int list iter [✭✭]
Write a function print_int_list' : int list -> unit whose specification is the same as print_int_list. Do not use the keyword rec in your solution, but instead to use the List module function List.iter. Here is some code to get you started:

let print_int_list' lst = 
  List.iter (fun x -> (* fill in here *)) lst *)