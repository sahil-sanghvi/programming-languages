(* CSC 330 Assignment 1 Problem 1*)
(* V-number: V01039545 *)
(* Name: Sahil Sanghvi *)

(* Helper functions for triplets - provided by assignment *)
let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let thd3 (_, _, x) = x

(* Here we are checking if a year is a leap year *)
let is_leap_year year =
  if year mod 400 = 0 then true
  else if year mod 100 = 0 then false
  else year mod 4 = 0

(*Checking if year is valid (1 <= year <= 3000) *)
let is_valid_year year =
  year >= 1 && year <= 3000

(*Checking if month is valid (1 <= month <= 12) *)
let is_valid_month month =
  month >= 1 && month <= 12

(* Getting number of days in a month (assumption is that valid year and month) *)
let get_days_in_month year month =
  if month = 1 then 31
  else if month = 2 then (if is_leap_year year then 29 else 28)
  else if month = 3 then 31
  else if month = 4 then 30
  else if month = 5 then 31
  else if month = 6 then 30
  else if month = 7 then 31
  else if month = 8 then 31
  else if month = 9 then 30
  else if month = 10 then 31
  else if month = 11 then 30
  else 31

(*Checking if a date is valid *)
let is_valid_date date =
  let year = fst3 date in
  let month = snd3 date in
  let day = thd3 date in
  if is_valid_year year && is_valid_month month then
    day >= 1 && day <= get_days_in_month year month
  else false

(* 1. is_older: returns true if the first date comes before the second date *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) : bool =
  let y1 = fst3 date1 in
  let m1 = snd3 date1 in
  let d1 = thd3 date1 in
  let y2 = fst3 date2 in
  let m2 = snd3 date2 in
  let d2 = thd3 date2 in
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

(* 2. days_in_month: returns the number of days in a given month *)
let days_in_month (year, month) =
  if is_valid_year year && is_valid_month month then
    Some (get_days_in_month year month)
  else None

(*Building a list of dates for a month recursively *)
let rec build_dates_list year month day max_day acc =
  if day > max_day then acc
  else build_dates_list year month (day + 1) max_day (acc @ [(year, month, day)])

(* 3. dates_in_month: returns a list of valid dates in that month *)
let dates_in_month (year, month) =
  if is_valid_year year && is_valid_month month then
    let max_day = get_days_in_month year month in
    Some (build_dates_list year month 1 max_day [])
  else None

(* Sum of days in months from 1 to month-1 *)
let rec sum_days_until_month year month acc =
  if month <= 1 then acc
  else sum_days_until_month year (month - 1) (acc + get_days_in_month year (month - 1))

(* 4. num_of_days: returns number of days from beginning of year until (and including) the given date *)
let num_of_days date =
  if is_valid_date date then
    let year = fst3 date in
    let month = snd3 date in
    let day = thd3 date in
    let days_before = sum_days_until_month year month 0 in
    Some (days_before + day)
  else None

(*Get total days in a year *)
let days_in_year year =
  if is_leap_year year then 366 else 365

(* Find date from day number - returns (year, month, day) triplet *)
let rec find_month_day year month remaining =
  let days_this_month = get_days_in_month year month in
  if remaining <= days_this_month then (year, month, remaining)
  else find_month_day year (month + 1) (remaining - days_this_month)

(* 5. nth_day: returns the date that corresponds to the n-th day of that year *)
let nth_day (year, n) =
  if is_valid_year year && n >= 1 && n <= days_in_year year then
    Some (find_month_day year 1 n)
  else None

(* TESTS ARE WRITTEN BELOW*)

let test_is_older () =
  
  assert (is_older ((1990, 1, 1), (1992, 1, 1)) = true);
  assert (is_older ((1995, 1, 1), (1992, 1, 1)) = false);

  assert (is_older ((1990, 1, 1), (1990, 6, 1)) = true);
  assert (is_older ((1990, 6, 1), (1990, 1, 1)) = false);

  assert (is_older ((1990, 1, 1), (1990, 1, 15)) = true);
  assert (is_older ((1996, 2, 21), (1996, 2, 20)) = false);

  assert (is_older ((2000, 6, 15), (2000, 6, 15)) = false);
  ()

let test_days_in_month () =

  assert (days_in_month (2023, 1) = Some 31);
  assert (days_in_month (2023, 4) = Some 30);

  assert (days_in_month (2023, 2) = Some 28);

  assert (days_in_month (2024, 2) = Some 29);

  assert (days_in_month (1900, 2) = Some 28);

  assert (days_in_month (2000, 2) = Some 29);

  assert (days_in_month (2023, 0) = None);
  assert (days_in_month (2023, 13) = None);
  assert (days_in_month (0, 1) = None);
  assert (days_in_month (3001, 1) = None);
  ()

let test_dates_in_month () =
  (* Testing invalid inputs *)
  assert (dates_in_month (2023, 15) = None);
  assert (dates_in_month (0, 1) = None);

  let rec list_length lst acc =
    if Option.is_none lst then 0
    else
      let l = Option.get lst in
      if l = [] then acc
      else list_length (Some (List.tl l)) (acc + 1)
  in
  assert (list_length (dates_in_month (2023, 1)) 0 = 31);
  assert (list_length (dates_in_month (2024, 2)) 0 = 29);
  assert (list_length (dates_in_month (2023, 2)) 0 = 28);
  (* Check first date *)
  assert (Option.is_some (dates_in_month (2023, 1)));
  assert (List.hd (Option.get (dates_in_month (2023, 1))) = (2023, 1, 1));
  ()

let test_num_of_days () =

  assert (num_of_days (2023, 1, 1) = Some 1);

  assert (num_of_days (2023, 1, 31) = Some 31);

  assert (num_of_days (2023, 2, 1) = Some 32);

  assert (num_of_days (2023, 12, 31) = Some 365);

  assert (num_of_days (2024, 12, 31) = Some 366);

  assert (num_of_days (2023, 2, 30) = None);
  assert (num_of_days (2023, 13, 1) = None);
  assert (num_of_days (0, 1, 1) = None);
  ()

let test_nth_day () =

  (* Valid inputs *)

  assert (nth_day (2023, 1) = Some (2023, 1, 1));

  assert (nth_day (2023, 31) = Some (2023, 1, 31));

  assert (nth_day (2023, 32) = Some (2023, 2, 1));

  assert (nth_day (2023, 365) = Some (2023, 12, 31));

  assert (nth_day (2024, 366) = Some (2024, 12, 31));

  assert (nth_day (2024, 60) = Some (2024, 2, 29));

  assert (nth_day (2023, 60) = Some (2023, 3, 1));

  (* Invalid inputs *)
  assert (nth_day (2023, 0) = None);
  assert (nth_day (2023, 366) = None);
  assert (nth_day (0, 1) = None);
  ()

(*Comment out the this section to run the unit tests*)  

(*

  let () =
  test_is_older ();
  print_endline "test_is_older passed!";
  test_days_in_month ();
  print_endline "test_days_in_month passed!";
  test_dates_in_month ();
  print_endline "test_dates_in_month passed!";
  test_num_of_days ();
  print_endline "test_num_of_days passed!";
  test_nth_day ();
  print_endline "test_nth_day passed!";
  print_endline "=== All Problem 1 tests passed! ==="

*)