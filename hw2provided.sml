(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) = 
	case str_list of
		  [] => NONE
		| hd::tl => if same_string(str, hd) 
					then SOME tl
					else case all_except_option(str, tl) of
					  	NONE => NONE
					 | SOME xs => SOME (hd::xs)

fun get_substitutions1(str_lists, str) =
	case str_lists of
		  [] => []
		| hd::tl => case all_except_option(str, hd) of
						  NONE => get_substitutions1(tl, str)
						| SOME xs => xs @ get_substitutions1(tl, str)

fun get_substitutions2(str_lists, str) = 
	let fun sub(str_lists, acc) = 
		case str_lists of
			  [] => acc
			| hd::tl => case all_except_option(str, hd) of
							  NONE => sub(tl, acc)
							| SOME xs => sub(tl, xs @ acc)
	in sub(str_lists, [])
	end

fun similar_names(str_lists, fullname) = 
	let val {first=x, middle=y, last=z} = fullname
		fun generate_names(sub_list) = 
			case sub_list of
				[] => []
				| hd::tl => {first=hd, middle=y, last=z}::generate_names(tl)
	in 
		fullname::generate_names(get_substitutions2(str_lists, x))
	end 
		 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(card) = 
	case card of 
		  (Spades, _) => Red
		| (Hearts, _) => Red
		| (Clubs, _) => Black
		| (Diamonds, _) => Black

fun card_value(card) =
	case card of 
		  (_, Ace) => 11
		| (_, Num n) => n
		| (_, _) => 10

 fun remove_card(cs, c, e) = 
 	case cs of 
 		  [] => raise e
 		| hd::tl => if hd = c then tl else hd::remove_card(tl, c, e)

 fun all_same_color(cs) = 
 	case cs of
 		  hd::(nk::tl) => (card_color(hd) = card_color(nk)) andalso all_same_color(nk::tl)
 		| _ => true

 fun sum_cards(cs) = 
 	let fun aux(cs, acc) = 
 		case cs of
 			  [] => acc
 			| hd::tl => aux(tl, card_value(hd) + acc)
 	in aux(cs, 0)
 	end 

(* fun score(cs, goal) = 
 	let fun pre_score(cs, goal) =
 		let val sum = sum_cards(cs)
 		in 
 			if sum > goal then 3 * (sum - goal) else goal - sum
 		end 
 	in 
 		if all_same_color(cs) then pre_score(cs, goal) div 2 else pre_score(cs, goal)
	end *)
fun score (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end

fun officiate(cs, ms, goal) = 
	let fun aux(cs, ms, hs) = 
		case ms of
			  [] => score(hs, goal)
			| (Discard c)::tl => aux(cs, tl, remove_card(hs, c, IllegalMove))
			| Draw::ms_tl => case cs of
								[] => score(hs, goal)
								| hd::cs_tl => if sum_cards(hd::hs) > goal then score(hd::hs, goal) else aux(cs_tl, ms_tl, hd::hs)
	in aux(cs, ms, [])
	end 