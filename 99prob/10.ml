(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last a =
  match a with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;


(* 2. Find the last but one (last and penultimate) elements of a list.  *)
let rec last2 a =
  match a with
  | [], [x] -> None
  | [ x, y ] -> Some x::y
  | _ :: t -> last2 t;;

(* 3. Find the k'th element of a list. (easy)*)
let rec at k a =
  match a with
  | [] -> None
  | [h :: t] -> if k = 1 then Some h else at (k-1) t


(* 4. Find the number of elements of a list. (easy)*)
(* NOT TAIL RECURSIVE *)
let rec count a =
  match a with
  | [] -> 0
  | [h :: t] -> 1 + count t

(* TAIL RECURSIVE *)
let len list =
  let rec aux n a =
    [] -> n
    [h :: t] -> aux (n+1) t

(* 5. Reverse a list. (easy)
OCaml standard library has List.rev but we ask that you reimplement it.
*)
let rev list =
  match list with
  | [] -> []
  | [h :: t] -> rev t::h

(* 6. Find out whether a list is a palindrome. (easy) *)
let rec is_palindrome a =
  a = List.rev a


(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t in
  List.rev (aux [] list);;

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let compress list =
  let rec aux last a =
    match a with
    | [] -> []
    | h :: t -> if last=h then aux h t else h :: aux h t in
    match list with
    | [] -> []
    | h :: t -> h :: aux h t

(* Useful for the idea of nested patterns *)
let rec compress = function
    | a :: (b :: t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
  

(*

# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
10. Run-length encoding of a list. (easy)
If you need so, refresh your memory about run-length encoding.

Solution

Here is an example:

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
11. Modified run-length encoding. (easy)
Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

# type 'a rle =
    | One of 'a
    | Many of int * 'a;;
type 'a rle = One of 'a | Many of int * 'a
Solution

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
12. Decode a run-length encoded list. (medium)
Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

Solution

# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
13. Run-length encoding of a list (direct solution). (medium)
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X.

Solution

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
14. Duplicate the elements of a list. (easy)
Solution

# duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
15. Replicate the elements of a list a given number of times. (medium)
Solution

# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
16. Drop every N'th element from a list. (medium)
Solution

# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
17. Split a list into two parts; the length of the first part is given. (easy)
If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

Solution

# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
18. Extract a slice from a list. (medium)
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

Solution

# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
19. Rotate a list N places to the left. (medium)
Solution

# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
20. Remove the K'th element from a list. (easy)
The first element of the list is numbered 0, the second 1,...

Solution

# remove_at 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "c"; "d"]
21. Insert an element at a given position into a list. (easy)
Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

Solution

# insert_at "alfa" 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
# insert_at "alfa" 3 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "alfa"; "d"]
# insert_at "alfa" 4 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "d"; "alfa"]
22. Create a list containing all integers within a given range. (easy)
If first argument is greater than second, produce a list in decreasing order.

Solution

# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4]
23. Extract a given number of randomly selected elements from a list. (medium)
The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility.

Solution

# rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
- : string list = ["g"; "d"; "a"]
24. Lotto: Draw N different random numbers from the set 1..M. (easy)
The selected numbers shall be returned in a list.

Solution

# lotto_select 6 49;;
- : int list = [10; 20; 44; 22; 41; 2]
25. Generate a random permutation of the elements of a list. (easy)
Solution

# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Solution

# extract 2 ["a";"b";"c";"d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
27. Group the elements of a set into disjoint subsets. (medium)
In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.
Solution

# group ["a";"b";"c";"d"] [2;1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
28. Sorting a list of lists according to length of sublists. (medium)
We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Solution

# length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]
# frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]
Arithmetic
31. Determine whether a given integer number is prime. (medium)
Solution

# not(is_prime 1);;
- : bool = true
# is_prime 7;;
- : bool = true
# not (is_prime 12);;
- : bool = true
32. Determine the greatest common divisor of two positive integer numbers. (medium)
Use Euclid's algorithm.

Solution

# gcd 13 27;;
- : int = 1
# gcd 20536 7826;;
- : int = 2
33. Determine whether two positive integer numbers are coprime. (easy)
Two numbers are coprime if their greatest common divisor equals 1.

Solution

# coprime 13 27;;
- : bool = true
# not (coprime 20536 7826);;
- : bool = true
34. Calculate Euler's totient function φ(m). (medium)
Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of φ(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).

Solution

# phi 10;;
- : int = 4
# phi 13;;
- : int = 12
35. Determine the prime factors of a given positive integer. (medium)
Construct a flat list containing the prime factors in ascending order.

Solution

# factors 315;;
- : int list = [3; 3; 5; 7]
36. Determine the prime factors of a given positive integer (2). (medium)
Construct a list containing the prime factors and their multiplicity. Hint: The problem is similar to problem Run-length encoding of a list (direct solution).

Solution

# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]
37. Calculate Euler's totient function φ(m) (improved). (medium)
See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula:

φ(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × ⋯

Solution

# phi_improved 10;;
- : int = 4
# phi_improved 13;;
- : int = 12
38. Compare the two methods of calculating Euler's totient function. (easy)
Use the solutions of problems "Calculate Euler's totient function φ(m)" and "Calculate Euler's totient function φ(m) (improved)" to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate φ(10090) as an example.

Solution

# timeit phi 10090;;
- : float = 0.00386905670166015625
# timeit phi_improved 10090;;
- : float = 3.0040740966796875e-05
39. A list of prime numbers. (easy)
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

Solution

# List.length (all_primes 2 7920);;
- : int = 1000
40. Goldbach's conjecture. (medium)
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer.

Solution

# goldbach 28;;
- : int * int = (5, 23)
41. A list of Goldbach compositions. (medium)
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

Solution

# goldbach_list 9 20;;
- : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))]
# goldbach_limit 1 2000 50;;
- : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 (1928, (61, 1867))]
Logic and Codes
Let us define a small "language" for boolean expressions containing variables:

# type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;
type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
A logical expression in two variables can then be written in prefix notation. For example, (a ∨ b) ∧ (a ∧ b) is written:

# And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
- : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
46 & 47. Truth tables for logical expressions (2 variables). (medium)
Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr).

Solution

# table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)]
48. Truth tables for logical expressions. (medium)
Generalize the previous problem in such a way that the logical expression may contain any number of logical variables. Define table in a way that table variables expr returns the truth table for the expression expr, which contains the logical variables enumerated in variables.

Solution

# table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
# let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)]
49. Gray code. (medium)
An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code.

Solution

# gray 1;;
- : string list = ["0"; "1"]
# gray 2;;
- : string list = ["00"; "01"; "11"; "10"]
# gray 3;;
- : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
50. Huffman code (hard)
First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes (you can start with the Wikipedia page)!

We consider a set of symbols with their frequencies. For example, if the alphabet is "a",..., "f" (represented as the positions 0,...5) and respective frequencies are 45, 13, 12, 16, 9, 5:

# let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
             ("e", 9); ("f", 5) ];;
val fs : (string * int) list =
  [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]
Our objective is to construct the Huffman code c word for all symbols s. In our example, the result could be hs = [("a", "0"); ("b", "101"); ("c", "100"); ("d", "111"); ("e", "1101"); ("f", "1100")] (or hs = [ ("a", "1");...]). The task shall be performed by the function huffman defined as follows: huffman(fs) returns the Huffman code table for the frequency table fs

Solution

# huffman fs;;
- : (string * string) list =
[("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 ("d", "111")]
# huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
- : (string * string) list =
[("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
Binary Tree

Binary Trees
A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

In OCaml, one can define a new type binary_tree that carries an arbitrary value of type 'a (thus is polymorphic) at each node.

# type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;
type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree
An example of tree carrying char data is:

# let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;
val example_tree : char binary_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
   Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
# let example_int_tree =
    Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
         Node(3, Empty, Node(6, Node(7, Empty, Empty), Empty)));;
val example_int_tree : int binary_tree =
  Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
   Node (3, Empty, Node (6, Node (7, Empty, Empty), Empty)))
In OCaml, the strict type discipline guarantees that, if you get a value of type binary_tree, then it must have been created with the two constructors Empty and Node.

55. Construct completely balanced binary trees. (medium)
In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal_tree to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Solution

# cbal_tree 4;;
- : char binary_tree list =
[Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));
 Node ('x', Node ('x', Empty, Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Empty));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty))]
# List.length(cbal_tree 40);;
- : int = 524288
56. Symmetric binary trees. (medium)
Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a function is_symmetric to check whether a given binary tree is symmetric.

Hint: Write a function is_mirror first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Solution

57. Binary search trees (dictionaries). (medium)
Construct a binary search tree from a list of integer numbers.

Solution

# construct [3;2;5;7;1];;
- : int binary_tree =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty)))
Then use this function to test the solution of the previous problem.

# is_symmetric(construct [5;3;18;1;4;12;21]);;
- : bool = true
# not(is_symmetric(construct [3;2;5;7;4]));;
- : bool = true
58. Generate-and-test paradigm. (medium)
Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Solution

# sym_cbal_trees 5;;
- : char binary_tree list =
[Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty))]
How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? What if the number is even? Write an appropriate function.

# List.length (sym_cbal_trees 57);;
- : int = 256
# List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20);;
- : (int * int) list =
[(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8);
 (18, 0); (19, 16); (20, 0)]
59. Construct height-balanced binary trees. (medium)
In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Write a function hbal_tree to construct height-balanced binary trees for a given height. The function should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Solution

# let t = hbal_tree 3;;
val t : char binary_tree list =
  [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
# let x = 'x';;
val x : char = 'x'
# List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                 Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t;;
- : bool = true
# List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                 Node(x, Node(x, Empty, Empty), Empty))) t;;
- : bool = true
# List.length t;;
- : int = 15
60. Construct height-balanced binary trees with a given number of nodes. (medium)
Consider a height-balanced binary tree of height h. What is the maximum number of nodes it can contain? Clearly, max_nodes = 2h - 1.

# let max_nodes h = 1 lsl h - 1;;
val max_nodes : int -> int = <fun>
However, what is the minimum number min_nodes? This question is more difficult. Try to find a recursive statement and turn it into a function min_nodes defined as follows: min_nodes h returns the minimum number of nodes in a height-balanced binary tree of height h.

Solution

On the other hand, we might ask: what are the minimum (resp. maximum) height H a height-balanced binary tree with N nodes can have? min_height (resp. max_height n) returns the minimum (resp. maximum) height of a height-balanced binary tree with n nodes.

Solution

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. hbal_tree_nodes n returns a list of all height-balanced binary tree with n nodes.

Solution

Find out how many height-balanced trees exist for n = 15.

# List.length (hbal_tree_nodes 15);;
- : int = 1553
# List.map hbal_tree_nodes [0; 1; 2; 3];;
- : char binary_tree list list =
[[Empty]; [Node ('x', Empty, Empty)];
 [Node ('x', Node ('x', Empty, Empty), Empty);
  Node ('x', Empty, Node ('x', Empty, Empty))];
 [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))]]
61. Count the leaves of a binary tree. (easy)
A leaf is a node with no successors. Write a function count_leaves to count them.

Solution

# count_leaves Empty;;
- : int = 0
# count_leaves example_tree;;
- : int = 3
61A. Collect the leaves of a binary tree in a list. (easy)
A leaf is a node with no successors. Write a function leaves to collect them in a list.

Solution

# leaves Empty;;
- : 'a list = []
# leaves example_tree;;
- : char list = ['d'; 'e'; 'g']
62. Collect the internal nodes of a binary tree in a list. (easy)
An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list.

Solution

# internals (Node('a', Empty, Empty));;
- : char list = []
# internals example_tree;;
- : char list = ['b'; 'a'; 'c'; 'f']
62B. Collect the nodes at a given level in a list. (easy)
A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list.

Solution

# at_level example_tree 2;;
- : char list = ['b'; 'c']
# at_level example_tree 5;;
- : char list = []
Using at_level it is easy to construct a function levelorder which creates the level-order sequence of the nodes. However, there are more efficient ways to do that.

63. Construct a complete binary tree. (medium)
A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i, note that we start counting the levels from 1 at the root). In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a function is_complete_binary_tree with the following specification: is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes.

Solution

# complete_binary_tree [1;2;3;4;5;6];;
- : int binary_tree =
Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node (6, Empty, Empty), Empty))
64. Layout a binary tree (1). (medium)
As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration.

Binary Tree Grid

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence;
y(v) is equal to the depth of the node v in the tree.
In order to store the position of the nodes, we will enrich the value at each node with the position (x,y).

The tree pictured above is

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty)),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)),
    Empty))
Solution

# layout_binary_tree_1 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 8, 1),
 Node (('k', 6, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('h', 5, 4),
    Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
  Node (('m', 7, 3), Empty, Empty)),
 Node (('u', 12, 2),
  Node (('p', 9, 3), Empty,
   Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
  Empty))
65. Layout a binary tree (2). (medium)
Binary Tree Grid

An alternative layout method is depicted in this illustration. Find out the rules and write the corresponding OCaml function.

Hint: On a given level, the horizontal distance between neighbouring nodes is constant.

The tree shown is

# let example_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('e', leaf 'd', leaf 'g')),
                   leaf 'm'),
         Node('u', Node('p', Empty, leaf 'q'), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('e', Node ('d', Empty, Empty), Node ('g', Empty, Empty))),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('q', Empty, Empty)), Empty))
Solution

# layout_binary_tree_2 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 15, 1),
 Node (('k', 7, 2),
  Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
    Node (('g', 6, 5), Empty, Empty))),
  Node (('m', 11, 3), Empty, Empty)),
 Node (('u', 23, 2),
  Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty))
# let example2_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Empty,
         Node('u', Node('p', Empty, leaf 'q'), Empty));;
val example2_layout_tree : char binary_tree =
  Node ('n', Empty,
   Node ('u', Node ('p', Empty, Node ('q', Empty, Empty)), Empty))
# layout_binary_tree_2 example2_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 1, 1), Empty,
 Node (('u', 5, 2),
  Node (('p', 3, 3), Empty, Node (('q', 4, 4), Empty, Empty)), Empty))
66. Layout a binary tree (3). (hard)
Binary Tree Grid

Yet another layout strategy is shown in the above illustration. The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding predicate.

Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree? This is a difficult problem. Don't give up too early!

Solution

# layout_binary_tree_3 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 5, 1),
 Node (('k', 3, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 3, 4), Node (('d', 2, 5), Empty, Empty),
    Node (('g', 4, 5), Empty, Empty))),
  Node (('m', 4, 3), Empty, Empty)),
 Node (('u', 7, 2),
  Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty)), Empty))
# let example3_layout_tree =
    Node('a', Node('b', Empty, Node('e', Empty, Node('f', Empty, Empty))),
         Node('c', Empty, Node('d', Node('g', Empty, Empty), Empty)));;
val example3_layout_tree : char binary_tree =
  Node ('a', Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty))),
   Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)))
# layout_binary_tree_3 example3_layout_tree;;
- : (char * int * int) binary_tree =
Node (('a', 3, 1),
 Node (('b', 1, 2), Empty,
  Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty))),
 Node (('c', 5, 2), Empty,
  Node (('d', 6, 3), Node (('g', 5, 4), Empty, Empty), Empty)))
Which layout do you like most?

67. A string representation of binary trees. (medium)
Binary Tree

Somebody represents binary trees as strings of the following type (see example): "a(b(d,e),c(,f(g,)))".

Write an OCaml function string_of_tree which generates this string representation, if the tree is given as usual (as Empty or Node(x,l,r) term). Then write a function tree_of_string which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single function tree_string which can be used in both directions.
Write the same predicate tree_string using difference lists and a single predicate tree_dlist which does the conversion between a tree and a difference list in both directions.
For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string.

Solution

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('a', Node('b', leaf 'd', leaf 'e'),
    Node('c', Empty, Node('f', leaf 'g', Empty)));;
val example_layout_tree : char binary_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
   Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
# string_of_tree example_layout_tree;;
- : string = "a(b(d,e),c(,f(g,)))"
# tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree;;
- : bool = true
# tree_of_string "";;
- : char binary_tree = Empty
68. Preorder and inorder sequences of binary trees. (medium)
We consider binary trees with nodes that are identified by single lower-case letters, as in the example of the previous problem.

Write functions preorder and inorder that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in the previous problem.
Can you use preorder from problem part 1 in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.
If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a function pre_in_tree that does the job.
Solve problems 1 to 3 using difference lists. Cool! Use the function timeit (defined in problem “Compare the two methods of calculating Euler's totient function.”) to compare the solutions.
What happens if the same character appears in more than one node. Try for instance pre_in_tree "aba" "baa".

Solution

# preorder (Node (1, Node (2, Empty, Empty), Empty));;
- : int list = [1; 2]
# preorder (Node (1, Empty, Node (2, Empty, Empty)));;
- : int list = [1; 2]
# let p = preorder example_tree;;
val p : char list = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g']
# let i = inorder example_tree;;
val i : char list = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f']
# pre_in_tree p i = example_tree;;
- : bool = true
Solution using difference lists.

  (* solution pending *)
69. Dotstring representation of binary trees. (medium)
We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem “A string representation of binary trees”. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem “A string representation of binary trees” is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a function tree_dotstring which does the conversion in both directions. Use difference lists.

  (* solution pending *)
Multiway Trees
Multiway Tree

A multiway tree is composed of a root element and a (possibly empty) set of successors which are multiway trees themselves. A multiway tree is never empty. The set of successor trees is sometimes called a forest.

To represent multiway trees, we will use the following type which is a direct translation of the definition:

# type 'a mult_tree = T of 'a * 'a mult_tree list;;
type 'a mult_tree = T of 'a * 'a mult_tree list
The example tree depicted opposite is therefore represented by the following OCaml expression:

# T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]);;
- : char mult_tree =
T ('a',
 [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
70C. Count the nodes of a multiway tree. (easy)
Solution

# count_nodes (T('a', [T('f',[]) ]));;
- : int = 2
70. Tree construction from a node string. (medium)
Multiway Tree

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree in the figure opposite is represented as: afg^^c^bd^e^^^.

Write functions string_of_tree : char mult_tree -> string to construct the string representing the tree and tree_of_string : string -> char mult_tree to construct the tree when the string is given.

Solution

# let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;
val t : char mult_tree =
  T ('a',
   [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
# string_of_tree t;;
- : string = "afg^^c^bd^e^^^"
# tree_of_string "afg^^c^bd^e^^^";;
- : char mult_tree =
T ('a',
 [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
71. Determine the internal path length of a tree. (easy)
We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, the tree t in the figure of the previous problem has an internal path length of 9. Write a function ipl tree that returns the internal path length of tree.

Solution

# ipl t;;
- : int = 9
72. Construct the bottom-up order sequence of the tree nodes. (easy)
Write a function bottom_up t which constructs the bottom-up sequence of the nodes of the multiway tree t.

Solution

# bottom_up (T('a', [T('b', [])]));;
- : char list = ['b'; 'a']
# bottom_up t;;
- : char list = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a']
73. Lisp-like tree representation. (medium)
There is a particular notation for multiway trees in Lisp. The picture shows how multiway tree structures are represented in Lisp.

Lisp representation of multiway
trees

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')'. This is very close to the way trees are represented in OCaml, except that no constructor T is used. Write a function lispy : char mult_tree -> string that returns the lispy notation of the tree.

Solution

# lispy (T('a', []));;
- : string = "a"
# lispy (T('a', [T('b', [])]));;
- : string = "(a b)"
# lispy t;;
- : string = "(a (f g) c (b d e))"
Graphs
A graph

A graph is defined as a set of nodes and a set of edges, where each edge is a pair of different nodes.

There are several ways to represent graphs in OCaml.

One method is to list all edges, an edge being a pair of nodes. In this form, the graph depicted opposite is represented as the following expression:
# ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'];;
- : (char * char) list =
[('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]
We call this edge-clause form. Obviously, isolated nodes cannot be represented.

Another method is to represent the whole graph as one data object. According to the definition of the graph as a pair of two sets (nodes and edges), we may use the following OCaml type:
# type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;
type 'a graph_term = { nodes : 'a list; edges : ('a * 'a) list; }
Then, the above example graph is represented by:

# let example_graph =
    { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
      edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] };;
val example_graph : char graph_term =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]}
We call this graph-term form. Note, that the lists are kept sorted, they are really sets, without duplicated elements. Each edge appears only once in the edge list; i.e. an edge from a node x to another node y is represented as (x,y), the couple (y,x) is not present. The graph-term form is our default representation. You may want to define a similar type using sets instead of lists.

A third representation method is to associate with each node the set of nodes that are adjacent to that node. We call this the adjacency-list form. In our example:

(* example pending *)
The representations we introduced so far well suited for automated processing, but their syntax is not very user-friendly. Typing the terms by hand is cumbersome and error-prone. We can define a more compact and "human-friendly" notation as follows: A graph (with char labelled nodes) is represented by a string of atoms and terms of the type X-Y. The atoms stand for isolated nodes, the X-Y terms describe edges. If an X appears as an endpoint of an edge, it is automatically defined as a node. Our example could be written as:

"b-c f-c g-h d f-b k-f h-g"
We call this the human-friendly form. As the example shows, the list does not have to be sorted and may even contain the same edge multiple times. Notice the isolated node d.

80. Conversions. (easy)
Write functions to convert between the different graph representations. With these functions, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. This problem is not particularly difficult, but it's a lot of work to deal with all the special cases.

(* example pending *)
81. Path from one node to another one. (medium)
Write a function paths g a b that returns all acyclic path p from node a to node b ≠ a in the graph g. The function should return the list of all paths via backtracking.

Solution

# paths example_graph 'f' 'b';;
- : char list list = [['f'; 'c'; 'b']; ['f'; 'b']]
82. Cycle from a given node. (easy)
Write a functions cycle g a that returns a closed path (cycle) p starting at a given node a in the graph g. The predicate should return the list of all cycles via backtracking.

Solution

# cycles example_graph 'f';;
- : char list list =
[['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f'];
 ['f'; 'b'; 'f']; ['f'; 'k'; 'f']]
83. Construct all spanning trees. (medium)
Spanning tree graph

Write a function s_tree g to construct (by backtracking) all spanning trees of a given graph g. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the test below. When you have a correct solution for the s_tree function, use it to define two other useful functions: is_tree graph and is_connected Graph. Both are five-minutes tasks!

(* solution pending *);;
# let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
            edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
                     ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
                     ('e', 'h'); ('f', 'g'); ('g', 'h')] };;
val g : char graph_term =
  {nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
   edges =
    [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e'); ('c', 'e'); ('d', 'e');
     ('d', 'f'); ('d', 'g'); ('e', 'h'); ('f', 'g'); ('g', 'h')]}
84. Construct the minimal spanning tree. (medium)
Spanning tree graph

Write a function ms_tree graph to construct the minimal spanning tree of a given labelled graph. A labelled graph will be represented as follows:

# type ('a, 'b) labeled_graph = { nodes : 'a list;
                                  labeled_edges : ('a * 'a * 'b) list };;
type ('a, 'b) labeled_graph = {
  nodes : 'a list;
  labeled_edges : ('a * 'a * 'b) list;
}
(Beware that from now on nodes and edges mask the previous fields of the same name.)

Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found below.

(* solution pending *);;
# let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
            labeled_edges = [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2);
                             ('b', 'e', 4); ('c', 'e', 6); ('d', 'e', 7);
                             ('d', 'f', 4); ('d', 'g', 3); ('e', 'h', 5);
                             ('f', 'g', 4); ('g', 'h', 1)] };;
val g : (char, int) labeled_graph =
  {nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
   labeled_edges =
    [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2); ('b', 'e', 4);
     ('c', 'e', 6); ('d', 'e', 7); ('d', 'f', 4); ('d', 'g', 3);
     ('e', 'h', 5); ('f', 'g', 4); ('g', 'h', 1)]}
85. Graph isomorphism. (medium)
Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 → N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

Write a function that determines whether two graphs are isomorphic. Hint: Use an open-ended list to represent the function f.

(* solution pending *);;
# let g = { nodes = [1; 2; 3; 4; 5; 6; 7; 8];
            edges = [(1,5); (1,6); (1,7); (2,5); (2,6); (2,8); (3,5);
                     (3,7); (3,8); (4,6); (4,7); (4,8)] };;
val g : int graph_term =
  {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
   edges =
    [(1, 5); (1, 6); (1, 7); (2, 5); (2, 6); (2, 8); (3, 5); (3, 7); 
     (3, 8); (4, 6); (4, 7); (4, 8)]}
# let h = { nodes = [1; 2; 3; 4; 5; 6; 7; 8];
            edges = [(1,2); (1,4); (1,5); (6,2); (6,5); (6,7); (8,4);
                     (8,5); (8,7); (3,2); (3,4); (3,7)] };;
val h : int graph_term =
  {nodes = [1; 2; 3; 4; 5; 6; 7; 8];
   edges =
    [(1, 2); (1, 4); (1, 5); (6, 2); (6, 5); (6, 7); (8, 4); (8, 5); 
     (8, 7); (3, 2); (3, 4); (3, 7)]}
# iso g h;;
Error: Unbound value iso
86. Node degree and graph coloration. (medium)
Write a function degree graph node that determines the degree of a given node.
Write a function that generates a list of all nodes of a graph sorted according to decreasing degree.
Use Welsh-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.
(* example pending *);;
87. Depth-first order graph traversal. (medium)
Write a function that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).

Specifically, the graph will be provided by its adjacency-list representation and you must create a module M with the following signature:

# module type GRAPH = sig
    type node = char
    type t
    val of_adjacency : (node * node list) list -> t
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
  end;;
module type GRAPH =
  sig
    type node = char
    type t
    val of_adjacency : (node * node list) list -> t
    val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
  end
where M.dfs_fold g n f a applies f on the nodes of the graph g in depth first order, starting with node n.

Solution

# let g = M.of_adjacency
            ['u', ['v'; 'x'];
             'v',      ['y'];
             'w', ['z'; 'y'];
             'x',      ['v'];
             'y',      ['x'];
             'z',      ['z'];
            ];;
val g : M.t = <abstr>
# List.rev (M.dfs_fold g 'w' (fun acc c -> c :: acc) []);;
- : M.node list = ['w'; 'z'; 'y'; 'x'; 'v']
88. Connected components. (medium)
Write a predicate that splits a graph into its connected components.

(* example pending *);;
89. Bipartite graphs. (medium)
Write a predicate that finds out whether a given graph is bipartite.

(* example pending *);;
90. Generate K-regular simple graphs with N nodes. (hard)
In a K-regular graph all nodes have a degree of K; i.e. the number of edges incident in each node is K. How many (non-isomorphic!) 3-regular graphs with 6 nodes are there?

See also the table of results.

(* example pending *);;
Miscellaneous Problems
91. Eight queens problem. (medium)
This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4;2;7;3;6;8;5;1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Solution

# queens_positions 4;;
- : int list list = [[3; 1; 4; 2]; [2; 4; 1; 3]]
# List.length (queens_positions 8);;
- : int = 92
92. Knight's tour. (medium)
Another famous problem is this one: How can a knight jump on an N×N chessboard in such a way that it visits every square exactly once?

Hints: Represent the squares by pairs of their coordinates (x,y), where both x and y are integers between 1 and N. Define the function jump n (x,y) that returns all coordinates (u,v) to which a knight can jump from (x,y) to on a n×n chessboard. And finally, represent the solution of our problem as a list knight positions (the knight's tour).

(* example pending *);;
93. Von Koch's conjecture. (hard)
Several years ago I met a mathematician who was intrigued by a problem for which he didn't know a solution. His name was Von Koch, and I don't know whether the problem has been solved since.

Tree numbering

Anyway, the puzzle goes like this: Given a tree with N nodes (and hence N-1 edges). Find a way to enumerate the nodes from 1 to N and, accordingly, the edges from 1 to N-1 in such a way, that for each edge K the difference of its node numbers equals to K. The conjecture is that this is always possible.

For small trees the problem is easy to solve by hand. However, for larger trees, and 14 is already very large, it is extremely difficult to find a solution. And remember, we don't know for sure whether there is always a solution!

Larger tree

Write a function that calculates a numbering scheme for a given tree. What is the solution for the larger tree pictured here?

(* example pending *);;
94. An arithmetic puzzle. (hard)
Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that the result is a correct equation. Example: With the list of numbers [2;3;5;7;11] we can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).

(* example pending *);;
95. English number words. (medium)
On financial documents, like cheques, numbers must sometimes be written in full words. Example: 175 must be written as one-seven-five. Write a function full_words to print (non-negative) integer numbers in full words.

Solution

# full_words 175;;
- : string = "one-seven-five"
# full_words 23485;;
- : string = "two-three-four-eight-five"
# full_words 0;;
- : string = "zero"
96. Syntax checker. (medium)
Syntax graph

In a certain programming language (Ada) identifiers are defined by the syntax diagram (railroad chart) opposite. Transform the syntax diagram into a system of syntax diagrams which do not contain loops; i.e. which are purely recursive. Using these modified diagrams, write a function identifier : string -> bool that can check whether or not a given string is a legal identifier.

Solution

# identifier "this-is-a-long-identifier";;
- : bool = true
# identifier "this-ends-in-";;
- : bool = false
# identifier "two--hyphens";;
- : bool = false
# identifier "-dash-first";;
- : bool = false
97. Sudoku. (medium)
Sudoku puzzles go like this:

   Problem statement                 Solution

    .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
            |         |                      |         |
    6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
            |         |                      |         |
    5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
    --------+---------+--------      --------+---------+--------
    3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
            |         |                      |         |
    .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
            |         |                      |         |
    .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
    --------+---------+--------      --------+---------+--------
    1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
            |         |                      |         |
    .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
            |         |                      |         |
    2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3x3 square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears exactly once in each row, in each column, and in each square.

Solution

# (* The board representation is not imposed.  Here "0" stands for "." *)
  let initial_board =
    Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                   [6; 7; 0;  9; 0; 0;  0; 0; 0];
                   [5; 0; 8;  0; 3; 0;  0; 0; 4];
                   [3; 0; 0;  7; 4; 0;  1; 0; 0];
                   [0; 6; 9;  0; 0; 0;  7; 8; 0];
                   [0; 0; 1;  0; 6; 9;  0; 0; 5];
                   [1; 0; 0;  0; 8; 0;  3; 0; 6];
                   [0; 0; 0;  0; 0; 6;  0; 9; 1];
                   [2; 4; 0;  0; 0; 1;  5; 0; 0]];;
val initial_board : Board.t =
  [|0; 0; 4; 8; 0; 0; 0; 1; 7; 6; 7; 0; 9; 0; 0; 0; 0; 0; 5; 0; 8; 0; 3; 0;
    0; 0; 4; 3; 0; 0; 7; 4; 0; 1; 0; 0; 0; 6; 9; 0; 0; 0; 7; 8; 0; 0; 0; 1;
    0; 6; 9; 0; 0; 5; 1; 0; 0; 0; 8; 0; 3; 0; 6; 0; 0; 0; 0; 0; 6; 0; 9; 1;
    2; 4; 0; 0; 0; 1; 5; 0; 0|]
# Board.print (sudoku initial_board);;
9  3  4 | 8  2  5 | 6  1  7
        |         |        
6  7  2 | 9  1  4 | 8  5  3
        |         |        
5  1  8 | 6  3  7 | 9  2  4
--------+---------+--------
3  2  5 | 7  4  8 | 1  6  9
        |         |        
4  6  9 | 1  5  3 | 7  8  2
        |         |        
7  8  1 | 2  6  9 | 4  3  5
--------+---------+--------
1  9  7 | 5  8  2 | 3  4  6
        |         |        
8  5  3 | 4  7  6 | 2  9  1
        |         |        
2  4  6 | 3  9  1 | 5  7  8
- : unit = ()
98. Nonograms. (hard)
Around 1994, a certain kind of puzzles was very popular in England. The "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are currently published each week only in The Sunday Telegraph. Simply use your logic and skill to complete the grid and reveal a picture or diagram." As an OCaml programmer, you are in a better situation: you can have your computer do the work!

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap is annotated with the respective lengths of its distinct strings of occupied cells. The person who solves the puzzle must complete the bitmap given only these lengths.

          Problem statement:          Solution:

          |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
          |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
          |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
          |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
          |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
          |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
          |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
          |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
          |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
           1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
           2 1 5 1                     2 1 5 1
For the example above, the problem can be stated as the two lists [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]] and [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]] which give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively. Published puzzles are larger than this example, e.g. 25×20, and apparently always have unique solutions.

Solution

# solve [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]]
        [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]];;
|_|X|X|X|_|_|_|_|
|X|X|_|X|_|_|_|_|
|_|X|X|X|_|_|X|X|
|_|_|X|X|_|_|X|X|
|_|_|X|X|X|X|X|X|
|X|_|X|X|X|X|X|_|
|X|X|X|X|X|X|_|_|
|_|_|_|_|X|_|_|_|
|_|_|_|X|X|_|_|_|
- : unit = ()
99. Crossword puzzle. (hard)
Crossword

Given an empty (or almost empty) framework of a crossword puzzle and a set of words. The problem is to place the words into the framework.

The particular crossword puzzle is specified in a text file which first lists the words (one word per line) in an arbitrary order. Then, after an empty line, the crossword framework is defined. In this framework specification, an empty character location is represented by a dot (.). In order to make the solution easier, character locations can also contain predefined character values. The puzzle above is defined in the file p7_09a.dat, other examples are p7_09b.dat and p7_09d.dat. There is also an example of a puzzle (p7_09c.dat) which does not have a solution.

Words are strings (character lists) of at least two characters. A horizontal or vertical sequence of character places in the crossword puzzle framework is called a site. Our problem is to find a compatible way of placing words onto sites.

Hints:

The problem is not easy. You will need some time to thoroughly understand it. So, don't give up too early! And remember that the objective is a clean solution, not just a quick-and-dirty hack!
For efficiency reasons it is important, at least for larger puzzles, to sort the words and the sites in a particular order.
(* example pending *);;
Learn
Code Examples
Tutorials
Books
Success Stories
 
Documentation
Install
Manual
Packages
Compiler Releases
Logos
 
Community
Mailing Lists
Meetings
News
Support
Bug Tracker
 
Website
Edit this page
Website Issues
About This Site
Find Us on GitHub
Credits *)