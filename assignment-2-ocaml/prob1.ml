(* V-number: V01039545 *)
(* Name: Sahil Sanghvi *)
(* CSC 330 - Assignment 2 - Problem 1: Functional Sets *)

(* Module signature for functional sets *)
module type FSET = sig
  type 'a set
  exception EmptySet
  val empty_set : ('a -> 'a -> int) -> 'a set
  val is_empty : 'a set -> bool
  val size : 'a set -> int
  val insert : 'a set -> 'a -> 'a set
  val remove : 'a set -> 'a -> 'a set
  val union : 'a set -> 'a set -> 'a set
  val intersect : 'a set -> 'a set -> 'a set
  val from_list : ('a -> 'a -> int) -> 'a list -> 'a set
  val to_list : 'a set -> 'a list
  val map : ('a -> 'a) -> 'a set -> 'a set
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a
end

(* Implementation of functional sets using ordered lists *)
module FuncSet : FSET = struct
  (* A set is represented as a tuple of (comparator function, ordered list of elements) *)
  type 'a set = ('a -> 'a -> int) * 'a list
  
  exception EmptySet
  
  (* Create an empty set with the given comparator *)
  let empty_set cmp = (cmp, [])
  
  (* Check if a set is empty *)
  let is_empty (_, lst) = 
    match lst with
    | [] -> true
    | _ -> false
  
  (* Return the number of elements in a set *)
  let size (_, lst) = 
    let rec count acc l =
      match l with
      | [] -> acc
      | _ :: tail -> count (acc + 1) tail
    in
    count 0 lst
  
  (* Insert an element into a set, maintaining order and uniqueness *)
  let insert (cmp, lst) elem =
    let rec insert_helper l =
      match l with
      | [] -> [elem]
      | h :: t ->
          let c = cmp elem h in
          if c = 0 then l  
          else if c < 0 then elem :: l  
          else h :: (insert_helper t)  (* Continue searching *)
    in
    (cmp, insert_helper lst)
  
  (* Remove an element from a set, raise EmptySet if not found *)
let remove (cmp, lst) elem =
  let rec remove_helper l =
    match l with
    | [] -> raise EmptySet  (* Element not found *)
    | h :: t ->
        let c = cmp elem h in
        if c = 0 then t  
        else if c < 0 then raise EmptySet  
        else h :: (remove_helper t)  (* Continue searching *)
  in
  (cmp, remove_helper lst)

  (* Union of two sets - combine elements from both sets *)
  let union (cmp1, lst1) (_, lst2) =
    let rec union_helper l1 l2 =
      match l1, l2 with
      | [], _ -> l2
      | _, [] -> l1
      | h1 :: t1, h2 :: t2 ->
          let c = cmp1 h1 h2 in
          if c = 0 then h1 :: (union_helper t1 t2)  (* Same element *)
          else if c < 0 then h1 :: (union_helper t1 l2)  (* h1 is smaller *)
          else h2 :: (union_helper l1 t2)  (* h2 is smaller *)
    in
    (cmp1, union_helper lst1 lst2)
  
  (* Intersection of two sets - only elements in both sets *)
  let intersect (cmp1, lst1) (_, lst2) =
    let rec intersect_helper l1 l2 =
      match l1, l2 with
      | [], _ -> []
      | _, [] -> []
      | h1 :: t1, h2 :: t2 ->
          let c = cmp1 h1 h2 in
          if c = 0 then h1 :: (intersect_helper t1 t2)  (* Found common element *)
          else if c < 0 then intersect_helper t1 l2  (* Skip h1 *)
          else intersect_helper l1 t2  (* Skip h2 *)
    in
    (cmp1, intersect_helper lst1 lst2)
  
  (* Create a set from a list of elements *)
  let from_list cmp lst =
    let rec insert_all s l =
      match l with
      | [] -> s
      | h :: t -> insert_all (insert s h) t
    in
    insert_all (empty_set cmp) lst
  
  (* Convert a set to a list *)
  let to_list (_, lst) = lst
  
  (* Map a function over a set - result must maintain set invariant *)
  let map f (cmp, lst) =
    let rec map_helper l =
      match l with
      | [] -> []
      | h :: t -> f h :: map_helper t
    in
    from_list cmp (map_helper lst)
  
  (* Left fold over a set *)
  let fold f acc (_, lst) =
    let rec fold_helper a l =
      match l with
      | [] -> a
      | h :: t -> fold_helper (f a h) t
    in
    fold_helper acc lst
end

(* Bonus: Infix operator for set building *)
let ( ++ ) set elem = FuncSet.insert set elem

(*
 * BONUS QUESTION 14:
 * The ++ operator is defined above the bonus questions comment.
 * It uses functional composition - each application of ++ returns a new set,
 * which can then have ++ applied to it again. This creates a chain:
 * 
 * empty_set Int.compare ++ 1       produces a set {1}
 * (empty_set Int.compare ++ 1) ++ 2    produces a set {1, 2}
 * ((empty_set Int.compare ++ 1) ++ 2) ++ 3    produces a set {1, 2, 3}
 * 
 * The left-associativity of ++ makes this work naturally.
 *)

(* TESTS ARE WRITTEN BELOW *)

let test_empty_set () =
  let cmp = Int.compare in
  let set = FuncSet.empty_set cmp in
  assert (FuncSet.is_empty set = true);
  assert (FuncSet.size set = 0);
  assert (FuncSet.to_list set = []);
  ()

let test_insert () =
  let cmp = Int.compare in
  let set = FuncSet.empty_set cmp in
  let set = FuncSet.insert set 1 in
  assert (FuncSet.size set = 1);
  assert (FuncSet.to_list set = [1]);
  assert (FuncSet.is_empty set = false);
  let set = FuncSet.insert set (-1) in
  assert (FuncSet.to_list set = [-1; 1]);
  (* Testing duplicate insertion *)
  let set = FuncSet.insert set 1 in
  assert (FuncSet.to_list set = [-1; 1]);
  ()

let test_from_list () =
  let cmp = Int.compare in
  let set = FuncSet.from_list cmp [100; 1; 2; -2; 2; 1] in
  assert (FuncSet.to_list set = [-2; 1; 2; 100]);
  ()

let test_union () =
  let cmp = Int.compare in
  let set1 = FuncSet.from_list cmp [1] in
  let set2 = FuncSet.from_list cmp [1; 2; -1] in
  assert (FuncSet.to_list (FuncSet.union set1 set2) = [-1; 1; 2]);
  ()

let test_intersect () =
  let cmp = Int.compare in
  let set1 = FuncSet.from_list cmp [1] in
  let set2 = FuncSet.from_list cmp [1; 2; -1] in
  assert (FuncSet.to_list (FuncSet.intersect set1 set2) = [1]);
  ()

let test_remove () =
  let cmp = Int.compare in
  let set = FuncSet.from_list cmp [1; 2; 3] in
  let set = FuncSet.remove set 2 in
  assert (FuncSet.to_list set = [1; 3]);
  ()

let test_map () =
  let cmp = Int.compare in
  let set = FuncSet.from_list cmp [1; 2; -1] in
  assert (FuncSet.to_list (FuncSet.map (fun x -> x mod 2) set) = [-1; 0; 1]);
  ()

let test_fold () =
  let cmp = Int.compare in
  let set = FuncSet.from_list cmp [1; 2; -1] in
  assert (FuncSet.fold (fun acc x -> acc + x) 0 set = 2);
  ()


(* Comment out this section to run the unit tests *)

(*
let () =
  test_empty_set ();
  print_endline "test_empty_set passed!";
  test_insert ();
  print_endline "test_insert passed!";
  test_from_list ();
  print_endline "test_from_list passed!";
  test_union ();
  print_endline "test_union passed!";
  test_intersect ();
  print_endline "test_intersect passed!";
  test_remove ();
  print_endline "test_remove passed!";
  test_map ();
  print_endline "test_map passed!";
  test_fold ();
  print_endline "test_fold passed!";
  test_infix_operator ();
  print_endline "test_infix_operator passed!";
  print_endline "=== All Problem 1 tests passed! ==="

*)