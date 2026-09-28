(* You must implement preprocess_prog and Shift before running these tests! *)
open Prob1;;

(** Preprocessing **)
match preprocess_prog @@ LineSegment (3.2, 4.1, 3.2, 4.1) with
| Point (a, b) when eq a 3.2 && eq b 4.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_1"
;;

match preprocess_prog @@ LineSegment (-3.2, -4.1, 3.2, 4.1) with
| LineSegment (c, d, a, b)
  when eq a (-3.2) && eq b (-4.1) && eq c 3.2 && eq d 4.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_2"
;;

match preprocess_prog @@ LineSegment (3.2, -4.1, 3.2, 4.1) with
| LineSegment (c, d, a, b) when eq a 3.2 && eq b (-4.1) && eq c 3.2 && eq d 4.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_3"
;;

match preprocess_prog @@ LineSegment (3.2, 4.1, 3.2, 6.1) with
| LineSegment (c, d, a, b) when eq a 3.2 && eq b 4.1 && eq c 3.2 && eq d 6.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_4"
;;

match preprocess_prog @@ LineSegment (1.2, 4.1, 3.2, 6.1) with
| LineSegment (c, d, a, b) when eq a 1.2 && eq b 4.1 && eq c 3.2 && eq d 6.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_5"
;;

match preprocess_prog @@ LineSegment (6.2, 4.1, 3.2, 6.1) with
| LineSegment (c, d, a, b) when eq a 3.2 && eq b 6.1 && eq c 6.2 && eq d 4.1 -> print_endline "OK"
| _ -> print_endline "fail: preprocess_6"
;;


(** Evaluation **)
(* Using a Nope *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, Nope)) [] with
| Nope -> print_endline "OK"
| _ -> print_endline "fail: eval_1"
;;

(* Using a Point *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, Point (4.0, 4.0))) [] with
| Point (a, b) when eq a 7.0 && eq b 8.0 -> print_endline "OK"
| _ -> print_endline "fail: eval_2"
;;

(* Using a Var *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, Var "a")) [ ("a", Point (4.0, 4.0)) ] with
| Point (a, b) when eq a 7.0 && eq b 8.0 -> print_endline "OK"
| _ -> print_endline "fail: eval_3"
;;

(* Using a Line *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, Line (1.0, 5.0))) [] with
| Line (a, b) when eq a 1.0 && eq b 6.0 -> print_endline "OK"
| _ -> print_endline "fail: eval_4"
;;

(* Using a LineSegment *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, LineSegment (1.0, 2.0, 3.0, 4.0))) [] with
| LineSegment (x2, y2, c, d) when eq c 4.0 && eq d 6.0 && eq x2 6.0 && eq y2 8.0 -> print_endline "OK"
| _ -> print_endline "fail: eval_5"
;;

(* With Variable Shadowing *)
match eval_prog (preprocess_prog @@ Shift (3.0, 4.0, Var "a")) [ ("a", Point (4.0, 4.0)); ("a", Point (1.0, 1.0)) ] with
| Point (a, b) when eq a 7.0 && eq b 8.0 -> print_endline "OK"
| _ -> print_endline "fail: eval_6"
