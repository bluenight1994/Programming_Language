(* function 1 *)

fun is_older(d1: int * int * int, d2: int * int * int) =
	if (#1 d1) < (#1 d2) then true
	else
		if (#1 d1) > (#1 d2) then false
		else
			if (#2 d1) < (#2 d2) then true
			else
				if (#2 d1) > (#2 d2) then false
				else
					if (#3 d1) < (#3 d2) then true
					else false


(* function 2 *)

fun number_in_month(xs: (int * int * int) list, month: int) =
	if null xs
	then 0
	else
		if #2 (hd xs) = month
		then 1 + number_in_month((tl xs), month)
		else 0 + number_in_month((tl xs), month)


(* function 3 *)

fun number_in_months(xs: (int * int * int) list, months: int list) = 
	if null months
	then 0
	else
		number_in_month(xs, (hd months)) + number_in_months(xs, (tl months))


(* function 4 *)

fun dates_in_month(dates: (int * int * int) list, month: int) = 
	if null dates
	then []
	else
		if #2 (hd dates) = month
		then (hd dates)::dates_in_month((tl dates), month)
		else dates_in_month((tl dates), month)


(* function 5 *)

fun dates_in_months(dates: (int * int * int) list, months: int list) =
	if null months
	then []
	else
		dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))


(* function 6 *)

fun get_nth(a: string list, n: int) =
	if n = 1
	then (hd a)
	else get_nth((tl a), n-1)


(* function 7 *)

fun date_to_string(year: int, month: int, day: int) =
	get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)


(* function 8 *)

fun number_before_reaching_sum(sum: int, l: int list) =
	if (hd l) < sum
	then 1 + number_before_reaching_sum(sum - (hd l), (tl l))
	else 0


(* function 9 *)

fun what_month(day: int) =
	1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])


(* function 10 *)

fun month_range(day1: int, day2: int) =
	if day1 < day2
	then what_month(day1)::month_range(day1+1, day2)
	else what_month(day2)::[];


(* function 11 *)

fun oldest(dates: (int * int * int) list) =
	if null dates
	then NONE
	else
		let
			val tl_ans = oldest(tl dates)
		in
			if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
			else SOME(hd dates)
		end


