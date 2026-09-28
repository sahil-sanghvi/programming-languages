(* CSC 330 Assignment 1 Problem 2*)
(* V-number: V01039545 *)
(* Name: Sahil Sanghvi *)

(* Country record type *)
type country = { id : string; name : string; rates : float option list }

(*Reading file contents into a string *)
let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

(* Converting a string to float, or None if the string isn't a valid number *)
let parse_rate s = Float.of_string_opt s

(* Convert each string in the list to some value or None) *)
let rec parse_rates strs =
  match strs with
  | [] -> []
  | h :: t -> (parse_rate h) :: (parse_rates t)

(* Parsing a single row into a country record *)
let parse_row row =
  let cols = String.split_on_char ',' row in
  match cols with
  | [] -> None
  | [_] -> None
  | name :: id :: rest ->
    Some { id = id; name = name; rates = parse_rates rest }

(*Parsing all rows into country records *)
let rec parse_rows rows =
  match rows with
  | [] -> []
  | h :: t ->
    match parse_row h with
    | None -> parse_rows t
    | Some c -> c :: (parse_rows t)

(* 1. get_records: parse file contents into list of country records *)
let get_records data =
  let lines = String.split_on_char '\n' data in
  (* Filter out empty lines *)
  let rec filter_empty lst =
    match lst with
    | [] -> []
    | h :: t -> if h = "" then filter_empty t else h :: (filter_empty t)
  in
  parse_rows (filter_empty lines)

(* 2. avail: count available yearly records (tail-recursive) *)
let avail country =
  let rec avail_helper rates acc =
    match rates with
    | [] -> acc
    | h :: t ->
      match h with
      | None -> (avail_helper [@tailcall]) t acc
      | Some _ -> (avail_helper [@tailcall]) t (acc + 1)
  in
  avail_helper country.rates 0

(* 3. last: find most recent year with available rate (tail-recursive) *)
(* Years start at 1960 *)
let last country =
  let start_year = 1960 in
  let rec last_helper rates year last_found =
    match rates with
    | [] -> last_found
    | h :: t ->
      match h with
      | None -> (last_helper [@tailcall]) t (year + 1) last_found
      | Some r -> (last_helper [@tailcall]) t (year + 1) (Some (year, r))
  in
  last_helper country.rates start_year None

(* 4. minmax: find years with lowest and highest inflation (tail-recursive) *)
let minmax country =
  let start_year = 1960 in
  let rec minmax_helper rates year min_found max_found =
    match rates with
    | [] -> (min_found, max_found)
    | h :: t ->
      match h with
      | None -> (minmax_helper [@tailcall]) t (year + 1) min_found max_found
      | Some r ->
        let new_min =
          match min_found with
          | None -> Some (year, r)
          | Some (_, min_r) -> if r < min_r then Some (year, r) else min_found
        in
        let new_max =
          match max_found with
          | None -> Some (year, r)
          | Some (_, max_r) -> if r > max_r then Some (year, r) else max_found
        in
        (minmax_helper [@tailcall]) t (year + 1) new_min new_max
  in
  minmax_helper country.rates start_year None None

(*Finding a country by ID in a list *)
let rec find_country countries target_id =
  match countries with
  | [] -> None
  | c :: t -> if c.id = target_id then Some c else find_country t target_id

(* 5. summarize: create summary string for a country *)
let summarize (countries, target_id) =
  match find_country countries target_id with
  | None -> "Cannot find " ^ target_id
  | Some c ->
    let avail_count = avail c in
    if avail_count = 0 then
      "Country: " ^ c.name ^ " (" ^ c.id ^ ")\n" ^
      "Records available: 0 years"
    else
      let last_record = last c in
      let (min_record, max_record) = minmax c in
      let base = "Country: " ^ c.name ^ " (" ^ c.id ^ ")\n" ^
                 "Records available: " ^ (string_of_int avail_count) ^ " years\n" in
      let last_str =
        match last_record with
        | None -> ""
        | Some (y, r) -> "Last record: " ^ (string_of_int y) ^ " with rate of " ^ (string_of_float r) ^ "%\n"
      in
      let min_str =
        match min_record with
        | None -> ""
        | Some (y, r) -> "Lowest rate: " ^ (string_of_int y) ^ " with rate of " ^ (string_of_float r) ^ "%\n"
      in
      let max_str =
        match max_record with
        | None -> ""
        | Some (y, r) -> "Highest rate: " ^ (string_of_int y) ^ " with rate of " ^ (string_of_float r) ^ "%"
      in
      base ^ last_str ^ min_str ^ max_str

(* BONUS 6. concat: concatenate strings with separator, ignoring empty strings (tail-recursive) *)
let concat (sep, strs) =
  (* First filter out empty strings *)
  let rec filter_empty lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
      if h = "" then (filter_empty [@tailcall]) t acc
      else (filter_empty [@tailcall]) t (acc @ [h])
  in
  let non_empty = filter_empty strs [] in
  (* Then concatenate with separator *)
  let rec concat_helper lst acc is_first =
    match lst with
    | [] -> acc
    | h :: t ->
      if is_first then (concat_helper [@tailcall]) t (acc ^ h) false
      else (concat_helper [@tailcall]) t (acc ^ sep ^ h) false
  in
  concat_helper non_empty "" true

(* TESTS ARE WRITTEN BELOW*)

(* Sample data for testing *)
let sample_data = "Canada,CAN,0.1,,2.1\nUnited States,USA,1.1,2.1,1.1\nUnknown,UNK,,,"

let test_get_records () =
  let records = get_records sample_data in
  (* Check using pattern matching *)
  match records with
  | [] -> assert false (* Should have 3 records *)
  | first :: _ ->
    assert (first.id = "CAN");
    assert (first.name = "Canada");
    match first.rates with
    | r1 :: r2 :: r3 :: [] ->
      assert (r1 = Some 0.1);
      assert (r2 = None);
      assert (r3 = Some 2.1);
      ()
    | _ -> assert false

let test_avail () =
  let records = get_records sample_data in
  match records with
  | can :: usa :: unk :: [] ->
    assert (avail can = 2);
    assert (avail usa = 3);
    assert (avail unk = 0);
    ()
  | _ -> assert false

let test_last () =
  let records = get_records sample_data in
  match records with
  | can :: usa :: unk :: [] ->
    assert (last can = Some (1962, 2.1));
    assert (last usa = Some (1962, 1.1));
    assert (last unk = None);
    ()
  | _ -> assert false

let test_minmax () =
  let records = get_records sample_data in
  match records with
  | can :: usa :: unk :: [] ->
    let (min_can, max_can) = minmax can in
    assert (min_can = Some (1960, 0.1));
    assert (max_can = Some (1962, 2.1));
    let (min_usa, max_usa) = minmax usa in
    assert (min_usa = Some (1960, 1.1));
    assert (max_usa = Some (1961, 2.1));
    let (min_unk, max_unk) = minmax unk in
    assert (min_unk = None);
    assert (max_unk = None);
    ()
  | _ -> assert false

let test_summarize () =
  let records = get_records sample_data in
  (* Test not found *)
  assert (summarize (records, "EEE") = "Cannot find EEE");
  assert (summarize (records, "XYZ") = "Cannot find XYZ");
  (* Test that summaries are non-empty strings *)
  let usa_summary = summarize (records, "USA") in
  let unk_summary = summarize (records, "UNK") in
  assert (usa_summary <> "");
  assert (unk_summary <> "");
  ()

let test_concat () =
  (* Basic test from assignment *)
  assert (concat (", ", ["a"; ""; "b"]) = "a, b");
  (* Empty list *)
  assert (concat (", ", []) = "");
  (* All empty strings *)
  assert (concat (", ", [""; ""; ""]) = "");
  (* Single element *)
  assert (concat (", ", ["a"]) = "a");
  (* Single empty element *)
  assert (concat (", ", [""]) = "");
  (* Multiple elements *)
  assert (concat ("-", ["x"; "y"; "z"]) = "x-y-z");
  (* Empty strings in middle *)
  assert (concat ("|", ["a"; ""; ""; "b"; ""; "c"]) = "a|b|c");
  ()


(*Comment out the this section to run the unit tests*)  
(*

  let () =
  test_get_records ();
  print_endline "test_get_records passed!";
  test_avail ();
  print_endline "test_avail passed!";
  test_last ();
  print_endline "test_last passed!";
  test_minmax ();
  print_endline "test_minmax passed!";
  test_summarize ();
  print_endline "test_summarize passed!";
  test_concat ();
  print_endline "test_concat passed!";
  print_endline "=== All Problem 2 tests passed! ==="

  *)