(* Assignment 4 *)
(* Do not make changes to this code except where you see comments containing the word TODO. *)
(* V-number: V01039545 *)
(* Name: Sahil Sanghvi *)

type geom_exp =
  | Nope
  | Point of (float * float)
  | Line of (float * float)
  | VerticalLine of float
  | LineSegment of (float * float * float * float)
  | Intersect of (geom_exp * geom_exp)
  | Let of (string * geom_exp * geom_exp)
  | Var of string
  | Shift of (float * float * geom_exp)

exception BadProgram of string
exception Impossible of string

let epsilon = 0.00001
let eq r1 r2 = Float.abs (r1 -. r2) < epsilon
let eq_point (x1, y1) (x2, y2) = eq x1 x2 && eq y1 y2

let two_points_to_line (x1, y1, x2, y2) =
  if eq x1 x2 then VerticalLine x1
  else
    let m = (y1 -. y2) /. (x1 -. x2) in
    let b = y2 -. (m *. x2) in
    Line (m, b)

let rec intersect v1 v2 =
  match (v1, v2) with
  | Nope, _ | _, Nope -> Nope
  | Point p1, Point p2 -> if eq_point p1 p2 then v1 else Nope
  | Point (x, y), Line (m, b) ->
      if eq y ((m *. x) +. b) then v1 else Nope
  | Point (x1, _), VerticalLine x2 -> if eq x1 x2 then v1 else Nope
  | Point _, LineSegment _ | Line _, Point _ -> intersect v2 v1
  | Line (m1, b1), Line (m2, b2) ->
      if eq m1 m2 then
        if eq b1 b2 then v1 else Nope
      else
        let x = (b2 -. b1) /. (m1 -. m2) in
        let y = (m1 *. x) +. b1 in
        Point (x, y)
  | Line (m1, b1), VerticalLine x2 -> Point (x2, (m1 *. x2) +. b1)
  | Line _, LineSegment _ | VerticalLine _, Point _ | VerticalLine _, Line _ ->
      intersect v2 v1
  | VerticalLine x1, VerticalLine x2 ->
      if eq x1 x2 then v1 else Nope
  | VerticalLine _, LineSegment seg -> intersect v2 v1
  | LineSegment seg, _ -> (
      match intersect (two_points_to_line seg) v2 with
      | Nope -> Nope
      | Point (x0, y0) ->
          let inbetween v end1 end2 =
            (end1 -. epsilon <= v && v <= end2 +. epsilon) ||
            (end2 -. epsilon <= v && v <= end1 +. epsilon)
          in
          let x1, y1, x2, y2 = seg in
          if inbetween x0 x2 x1 && inbetween y0 y2 y1 then Point (x0, y0)
          else Nope
      | Line _ -> v1
      | VerticalLine _ -> v1
      | LineSegment seg2 ->
          let x1end, y1end, x1start, y1start = seg in
          let x2end, y2end, x2start, y2start = seg2 in
          if eq x1start x1end then
            let (aXend, aYend, _aXstart, _aYstart), (bXend, bYend, bXstart, bYstart) =
              if y1start < y2start then (seg, seg2) else (seg2, seg)
            in
            if eq aYend bYstart then Point (aXend, aYend)
            else if aYend < bYstart then Nope
            else if aYend > bYend then LineSegment (bXend, bYend, bXstart, bYstart)
            else LineSegment (aXend, aYend, bXstart, bYstart)
          else
            let (aXend, aYend, _aXstart, _aYstart), (bXend, bYend, bXstart, bYstart) =
              if x1start < x2start then (seg, seg2) else (seg2, seg)
            in
            if eq aXend bXstart then Point (aXend, aYend)
            else if aXend < bXstart then Nope
            else if aXend > bXend then LineSegment (bXend, bYend, bXstart, bYstart)
            else LineSegment (aXend, aYend, bXstart, bYstart)
      | _ -> raise (Impossible "bad result from intersecting with a line"))
  | _ -> raise (Impossible "bad call to intersect: only for shape values")

let rec eval_prog e env =
  match e with
  | Nope | Point _ | Line _ | VerticalLine _ | LineSegment _ -> e
  | Var s -> (
      match List.find_opt (fun (n, _) -> s = n) env with
      | None -> raise (BadProgram ("var not found: " ^ s))
      | Some (_, v) -> v)
  | Let (s, e1, e2) -> eval_prog e2 ((s, eval_prog e1 env) :: env)
  | Intersect (e1, e2) -> intersect (eval_prog e1 env) (eval_prog e2 env)
  | Shift (dx, dy, e) ->
      (match eval_prog e env with
       | Nope -> Nope
       | Point (x, y) -> Point (x +. dx, y +. dy)
       | Line (m, b) -> Line (m, b +. dy -. m *. dx)
       | VerticalLine x -> VerticalLine (x +. dx)
       | LineSegment (x1, y1, x2, y2) -> LineSegment (x1 +. dx, y1 +. dy, x2 +. dx, y2 +. dy)
       | _ -> raise (Impossible "shift of non-value"))

let rec preprocess_prog e =
  match e with
  | Nope | Point _ | Line _ | VerticalLine _ -> e
  | LineSegment (x1, y1, x2, y2) ->
      if eq_point (x1, y1) (x2, y2) then
        Point (x1, y1)
      else if eq x1 x2 then
        if y1 > y2 then LineSegment (x1, y1, x2, y2)
        else LineSegment (x2, y2, x1, y1)
      else
        if x1 > x2 then LineSegment (x1, y1, x2, y2)
        else LineSegment (x2, y2, x1, y1)
  | Intersect (e1, e2) -> Intersect (preprocess_prog e1, preprocess_prog e2)
  | Let (s, e1, e2) -> Let (s, preprocess_prog e1, preprocess_prog e2)
  | Var _ -> e
  | Shift (dx, dy, sub) -> Shift (dx, dy, preprocess_prog sub)
