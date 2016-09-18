(* 1 *)
fun is_older (date1: int*int*int, date2: int*int*int) =
	if (#1 date1) < (#1 date2) then true
	else if (#1 date1) > (#1 date2) then false
	else if (#2 date1) < (#2 date2) then true 
	else if (#2 date1) > (#2 date2) then false
	else if (#3 date1) < (#3 date2) then true
	else false

(* 2 *)
fun number_in_month (date_list: (int*int*int) list, month: int) = 
	if null date_list then 0
	else 
		let fun isMonth (date: int*int*int, month: int) = 
			if #2 date = month then 1 else 0
		in
			number_in_month(tl date_list, month) + isMonth(hd date_list, month)
		end 

(* 3 *)
fun number_in_months (date_list: (int*int*int) list, month_list: int list) =
	if null month_list then 0
	else number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

(* 4 *)
fun dates_in_month (date_list: (int*int*int) list, month: int) =
	if null date_list then []
	else 
		let val date = hd date_list 
		in
			if #2 date = month then date :: dates_in_month(tl date_list, month)
			else dates_in_month(tl date_list, month)
		end

(* 5 *)
fun dates_in_months (date_list: (int*int*int) list, month_list: int list) =
	if null month_list then []
	else dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list)

(* 6 *)
fun get_nth (strings: string list, n: int) = 
	if n > 1 then get_nth(tl strings, n-1)
	else hd strings

(* 7 *)
fun date_to_string (date: int*int*int) = 
	let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end 

(* 8 *)
fun number_before_reaching_sum (sum: int, nums: int list) = 
	let val new_head = hd nums + hd (tl nums)
	in 
		if hd nums >= sum then 0
		else number_before_reaching_sum(sum, new_head :: tl (tl nums)) + 1
	end 

(* 9 *)
fun what_month (dayofyear: int) = 
	let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in number_before_reaching_sum(dayofyear, days_in_months) + 1
	end 

(* 10 *)
fun month_range (day1: int, day2: int) = 
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest (date_list: (int*int*int) list) = 
	if null date_list then NONE
	else 
		let fun order_date (date1: int*int*int, other_dates: (int*int*int) list) =
			if null other_dates then SOME date1
			else if is_older(date1, hd other_dates) then order_date(date1, tl other_dates)
			else order_date(hd other_dates, tl other_dates)
		in 
			order_date(hd date_list, tl date_list)
		end 
			