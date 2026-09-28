(* V-number: V01039545 *)
(* Name: Sahil Sanghvi *)
(* CSC 330 - Assignment 2 - Problem 2: OToml *)

(* Type definitions for OToml structure *)
type ovalue =
  | Str of string
  | Int of int
  | Table of okeyvalue list
  | ListOf of ovalue list
and okeyvalue = string * ovalue
and otable = string * okeyvalue list
and otoml = otable list

(* Type definition for OToml patterns *)
type opattern =
  | WildcardPattern
  | BindPattern of string
  | StringPattern of string
  | IntPattern of int
  | TablePattern of (string * opattern) list
  | ListPattern of opattern list

let ( % ) f g x = f (g x)

let ( |> ) x f = f x

(* combines two lists into a list of pairs *)
let zip lst1 lst2 =
  let rec zip_helper l1 l2 acc =
    match l1, l2 with
    | [], [] -> Some (List.rev acc)
    | h1 :: t1, h2 :: t2 -> zip_helper t1 t2 ((h1, h2) :: acc)
    | _, _ -> None  (* Lists have different lengths *)
  in
  zip_helper lst1 lst2 []

(* applies function and concatenates results *)
let flatmap f lst =
  let rec concat_lists l =
    match l with
    | [] -> []
    | h :: t -> h @ (concat_lists t)
  in
  let rec map_helper l =
    match l with
    | [] -> []
    | h :: t -> (f h) :: (map_helper t)
  in
  concat_lists (map_helper lst)

(* flatmap that handles optional results *)
let flatmap_opt f lst =
  let rec flatmap_opt_helper l =
    match l with
    | [] -> Some []
    | h :: t ->
        match f h with
        | None -> None
        | Some result ->
            match flatmap_opt_helper t with
            | None -> None
            | Some rest -> Some (result @ rest)
  in
  flatmap_opt_helper lst

(* extracts all string values with their table names *)
let get_strings otoml_data =
  let rec extract_strings_from_value table_name value =
    match value with
    | Str s -> [(table_name, s)]
    | Int _ -> []
    | Table kvs ->
        flatmap (fun (_, v) -> extract_strings_from_value table_name v) kvs
    | ListOf vals ->
        flatmap (fun v -> extract_strings_from_value table_name v) vals
  in
  let rec extract_from_table table =
    match table with
    | (name, kvs) ->
        flatmap (fun (_, v) -> extract_strings_from_value name v) kvs
  in
  flatmap extract_from_table otoml_data

(* checks if string is all uppercase *)
let is_uppercase s =
  s = String.uppercase_ascii s && s <> ""

(* using composition operator *)
let get_uppercase_strings_1 otoml_data =
  let filter_uppercase lst =
    let rec filter l acc =
      match l with
      | [] -> List.rev acc
      | (_, s) :: t ->
          if is_uppercase s then filter t (s :: acc)
          else filter t acc
    in
    filter lst []
  in
  (filter_uppercase % get_strings) otoml_data

(* using pipe operator *)
let get_uppercase_strings_2 otoml_data =
  let filter_uppercase lst =
    let rec filter l acc =
      match l with
      | [] -> List.rev acc
      | (_, s) :: t ->
          if is_uppercase s then filter t (s :: acc)
          else filter t acc
    in
    filter lst []
  in
  otoml_data |> get_strings |> filter_uppercase

(* pattern matching for OToml values *)
let omatch pattern value =
  let rec match_helper pat val_data =
    match pat, val_data with
    | WildcardPattern, _ -> Some []
    | BindPattern name, v -> Some [(name, v)]
    | StringPattern s1, Str s2 ->
        if s1 = s2 then Some [] else None
    | IntPattern i1, Int i2 ->
        if i1 = i2 then Some [] else None
    | ListPattern pats, ListOf vals ->
        match_list pats vals
    | TablePattern pat_pairs, Table val_pairs ->
        match_table pat_pairs val_pairs
    | _, _ -> None  (* Type mismatch *)
  
  and match_list pats vals =
    match zip pats vals with
    | None -> None  (* Different lengths *)
    | Some pairs ->
        flatmap_opt (fun (p, v) -> match_helper p v) pairs
  
  and match_table pat_pairs val_pairs =
    (* Try to match each pattern with corresponding value *)
    let rec match_pairs pp vp =
      match pp with
      | [] -> Some []
      | (pat_name, pat) :: rest ->
          match find_match pat_name pat vp with
          | None -> None
          | Some bindings ->
              match match_pairs rest vp with
              | None -> None
              | Some rest_bindings -> Some (bindings @ rest_bindings)
    
    and find_match pat_name pat val_list =
      match val_list with
      | [] -> None
      | (val_name, val_data) :: rest ->
          (* If pattern name is empty, ignore the name and just match pattern *)
          if pat_name = "" then
            match match_helper pat val_data with
            | Some bindings -> Some bindings
            | None -> find_match pat_name pat rest
          (* If pattern name matches, try to match the pattern *)
          else if pat_name = val_name then
            match_helper pat val_data
          (* Otherwise, keep searching *)
          else
            find_match pat_name pat rest
    in
    match_pairs pat_pairs val_pairs
  in
  match_helper pattern value

(* ensures no duplicate names in tables or keys *)
let ocheck otoml_data =
  let rec has_duplicates lst =
    match lst with
    | [] -> false
    | h :: t ->
        let rec contains x l =
          match l with
          | [] -> false
          | y :: rest -> x = y || contains x rest
        in
        contains h t || has_duplicates t
  in
  let rec check_table_keys kvs =
    let keys = 
      let rec extract_keys kv_list acc =
        match kv_list with
        | [] -> List.rev acc
        | (k, _) :: rest ->
            (* Only check non-empty keys for duplicates *)
            if k = "" then extract_keys rest acc
            else extract_keys rest (k :: acc)
      in
      extract_keys kvs []
    in
    not (has_duplicates keys)
  in
  let rec check_all_tables tables =
    match tables with
    | [] -> true
    | (_, kvs) :: rest ->
        check_table_keys kvs && check_all_tables rest
  in
  let table_names =
    let rec extract_names tbl_list acc =
      match tbl_list with
      | [] -> List.rev acc
      | (name, _) :: rest ->
          (* Only check non-empty table names for duplicates *)
          if name = "" then extract_names rest acc
          else extract_names rest (name :: acc)
    in
    extract_names otoml_data []
  in
  not (has_duplicates table_names) && check_all_tables otoml_data

(* Type definitions and typechecking *)
type otype =
  | StrType
  | IntType
  | ListType of otype
  | TableType of otype list

let otypecheck otoml_data =
  let rec infer_value_type value =
    match value with
    | Str _ -> Some StrType
    | Int _ -> Some IntType
    | ListOf vals -> infer_list_type vals
    | Table kvs -> infer_table_type kvs
  
  and infer_list_type vals =
    match vals with
    | [] -> None  (* Can't infer type of empty list *)
    | first :: rest ->
        match infer_value_type first with
        | None -> None
        | Some first_type ->
            if all_same_type first_type rest then
              Some (ListType first_type)
            else
              None
  
  and all_same_type expected vals =
    match vals with
    | [] -> true
    | h :: t ->
        match infer_value_type h with
        | None -> false
        | Some h_type ->
            types_equal expected h_type && all_same_type expected t
  
  and types_equal t1 t2 =
    match t1, t2 with
    | StrType, StrType -> true
    | IntType, IntType -> true
    | ListType lt1, ListType lt2 -> types_equal lt1 lt2
    | TableType tl1, TableType tl2 ->
        (match zip tl1 tl2 with
         | None -> false
         | Some pairs ->
             let rec all_equal ps =
               match ps with
               | [] -> true
               | (a, b) :: rest -> types_equal a b && all_equal rest
             in
             all_equal pairs)
    | _, _ -> false
  
  and infer_table_type kvs =
    let rec infer_all kv_list acc =
      match kv_list with
      | [] -> Some (List.rev acc)
      | (_, v) :: rest ->
          match infer_value_type v with
          | None -> None
          | Some t -> infer_all rest (t :: acc)
    in
    match infer_all kvs [] with
    | None -> None
    | Some types -> Some (TableType types)
  in
  let rec infer_all_tables tables acc =
    match tables with
    | [] -> Some (List.rev acc)
    | (_, kvs) :: rest ->
        match infer_table_type kvs with
        | None -> None
        | Some (TableType types) -> infer_all_tables rest (TableType types :: acc)
        | _ -> None  (* Should never happen *)
  in
  infer_all_tables otoml_data []

(* TESTS ARE WRITTEN BELOW *)

let test_zip () =
  assert (zip [1; 2; 3] ["a"; "b"; "c"] = Some [(1, "a"); (2, "b"); (3, "c")]);
  assert (zip [1; 2] ["a"] = None);
  ()

let test_flatmap () =
  assert (flatmap (fun x -> [x; -x]) [1; 2; 3] = [1; -1; 2; -2; 3; -3]);
  ()

let test_flatmap_opt () =
  assert (flatmap_opt (fun x -> if x mod 2 = 0 then Some [x; -x] else None) [2; 4] 
          = Some [2; -2; 4; -4]);
  assert (flatmap_opt (fun x -> Some x) [] = Some []);
  assert (flatmap_opt (fun x -> if x mod 2 = 0 then Some [x; -x] else None) [1; 2; 4] 
          = None);
  ()

let test_get_strings () =
  let t = [ "table1", [ ("str", Str "hello")
                      ; ("int", Int 5)
                      ; ("table", Table [("a", Str "x"); ("b", Str "Y")])
                      ; ("list", ListOf [Int 1; Int 2]) ]
          ; "table2", [ ("something", Str "maybe" )] ] in
  assert (get_strings t = 
          [("table1", "hello"); ("table1", "x"); ("table1", "Y"); ("table2", "maybe")]);
  ()

let test_is_uppercase () =
  assert (is_uppercase "HELLO" = true);
  assert (is_uppercase "Hello" = false);
  ()

let test_get_uppercase_strings () =
  let t = [ "table1", [ ("str", Str "hello")
                      ; ("int", Int 5)
                      ; ("table", Table [("a", Str "x"); ("b", Str "Y")])
                      ; ("list", ListOf [Int 1; Int 2]) ]
          ; "table2", [ ("something", Str "maybe" )] ] in
  assert (get_uppercase_strings_1 t = ["Y"]);
  assert (get_uppercase_strings_2 t = ["Y"]);
  ()

let test_omatch () =
  let t = [ "table1", [ ("str", Str "hello")
                      ; ("int", Int 5)
                      ; ("table", Table [("a", Str "x"); ("b", Str "Y")])
                      ; ("list", ListOf [Int 1; Int 2]) ]
          ; "table2", [ ("something", Str "maybe" )] ] in
  let p1 = TablePattern
    [ ("", StringPattern "hello")
    ; ("int", BindPattern "x")
    ; ("", TablePattern [ ("", BindPattern "y"); ("b", WildcardPattern) ])
    ; ("", WildcardPattern) ] in
  assert (omatch p1 (Table (snd (List.hd t))) = Some [("x", Int 5); ("y", Str "x")]);
  let p2 = TablePattern
    [ ("", StringPattern "hello")
    ; ("", BindPattern "x")
    ; ("", TablePattern [ ("a", BindPattern "y"); ("c", WildcardPattern) ]) ] in
  assert (omatch p2 (Table (snd (List.hd t))) = None);
  ()

let test_ocheck () =
  let t = [ "table1", [ ("str", Str "hello")
                      ; ("int", Int 5)
                      ; ("table", Table [("a", Str "x"); ("b", Str "Y")])
                      ; ("list", ListOf [Int 1; Int 2]) ]
          ; "table2", [ ("something", Str "maybe" )] ] in
  assert (ocheck t = true);
  let t_dup = (List.hd t) :: ["table1", []] in
  assert (ocheck t_dup = false);
  ()

let test_otypecheck () =
  let t = [ "table1", [ ("str", Str "hello")
                      ; ("int", Int 5)
                      ; ("table", Table [("a", Str "x"); ("b", Str "Y")])
                      ; ("list", ListOf [Int 1; Int 2]) ]
          ; "table2", [ ("something", Str "maybe" )] ] in
  assert (otypecheck t = 
          Some [ TableType [ StrType; IntType; TableType [ StrType; StrType ]; ListType IntType ]
               ; TableType [ StrType ] ]);
  let t_bad = t @ ["table3", [("foo", ListOf [Int 1; Str "2"])]] in
  assert (otypecheck t_bad = None);
  ()

(* Comment out this section to run the unit tests *)

(*
let () =
  test_zip ();
  print_endline "test_zip passed!";
  test_flatmap ();
  print_endline "test_flatmap passed!";
  test_flatmap_opt ();
  print_endline "test_flatmap_opt passed!";
  test_get_strings ();
  print_endline "test_get_strings passed!";
  test_is_uppercase ();
  print_endline "test_is_uppercase passed!";
  test_get_uppercase_strings ();
  print_endline "test_get_uppercase_strings passed!";
  test_omatch ();
  print_endline "test_omatch passed!";
  test_ocheck ();
  print_endline "test_ocheck passed!";
  test_otypecheck ();
  print_endline "test_otypecheck passed!";
  print_endline "=== All Problem 2 tests passed! ==="

*)