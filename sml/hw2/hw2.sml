
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
              
