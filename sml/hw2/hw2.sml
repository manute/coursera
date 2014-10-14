
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, []) = NONE
  | all_except_option (s, x :: xs) =  
    case same_string(s,x) of
       true => SOME xs
     | false => case all_except_option(s, xs) of
                    NONE => NONE
                  | SOME y => SOME (x::y) 
     

fun get_substitutions1 ([], s) = []
  | get_substitutions1 (x :: xs, s) =
    case all_except_option(s, x) of
      NONE   => get_substitutions1(xs, s)
    | SOME y => y @ get_substitutions1(xs, s)

fun get_substitutions2 ([], s) = []
  | get_substitutions2 (x :: xs, s) =
    case all_except_option(s, x) of
      NONE   => get_substitutions2(xs, s)
    | SOME y => y @ get_substitutions2(xs, s)

fun similar_names (subs: string list list, name) =
  let val {first:string, middle:string, last:string} = name
      fun aux ([], result) = result
        | aux ((x::xs), result) = aux (xs, {first=x, middle=middle, last=last}::result)
  in 
    let val all_first_names = first::(get_substitutions1(subs, first))
    in
      rev (aux (all_first_names, []))
    end
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

fun card_color (card) =
    let val (suit, _) = card
    in
        case suit of
            Spades => Black
          | Clubs => Black
          | Diamonds => Red
          | Hearts => Red 
    end

fun card_value (card) = 
    let val (_, rank) = card
    in
        case rank of         
           Ace => 11 
         | Jack => 10 
         | Queen => 10
         | King => 10
         | Num i => i               
    end

fun remove_card ([], _, e) = raise e
  | remove_card ((x::xs), c, e) =
    if x = c 
    then xs
    else x::remove_card(xs,c,e)
                      
val cards = [(Clubs, Jack), (Spades, Queen), (Hearts, Ace)]

fun all_same_color [] = true
  | all_same_color (c1::[]) = true
  | all_same_color (c1::c2::[]) = card_color(c1) = card_color(c2)
  | all_same_color (c1::c2::c3::tail) = 
    let 
      val precondition = card_color(c1) = card_color(c2)
    in
      if not precondition 
      then false 
      else all_same_color(c2::c3::tail)
    end 
  
fun sum_cards (cs: card list) =
  let fun aux [] = 0
        | aux (x::xs) = card_value(x) + aux(xs)
  in 
    aux cs
  end 

fun score (cs, goal) =
  let val sum = sum_cards cs
      val preliminary_score = 
        if sum > goal then 3 * (sum - goal)
                      else goal - sum
  in 
    if all_same_color(cs) 
    then preliminary_score
    else preliminary_score div 2
  end     
