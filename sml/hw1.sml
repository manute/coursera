
type date = (int * int * int)
                            
             
fun is_older (d1: date, d2: date): bool =
    if #1 d1 > #1 d1
    then
        true
    else
        if #2 d1 > #2 d2
        then
            true
        else
            #3 d1 > #3 d2
                       

fun filter _ nil      = nil
  | filter f (hd::tl) = if (f hd) then hd::(filter f tl)
                        else (filter f tl) 
                                     

fun number_in_month (xs: date list, m) =
    length (filter (fn x => #2 x = m ) xs)                      
        

fun number_in_months (xs: date list, nil) = 0
  | number_in_months (xs: date list, (hd::tl)) = number_in_month(xs,hd) + number_in_months(xs,tl)
                                           

fun dates_in_month (xs : date list , m) =
    filter (fn x => #2 x = m ) xs
    
     
fun dates_in_months (xs: date list, nil) = []
  | dates_in_months (xs: date list, (hd::tl)) = dates_in_month(xs,hd) @ dates_in_months(xs,tl)


fun get_nth (xs: string list, 1) = hd xs
  | get_nth (xs: string list, n: int) = get_nth(tl xs, n - 1)
                                                                                    

val months = ["January", "February", "March", "April", "May", "June",
              "July", "August","September", "October", "November", "December"]

                                               
fun date_to_string (d: date) =
    get_nth(months, #2 d) ^ " " ^ (Int.toString (#3 d)) ^ ", " ^ (Int.toString (#1 d))
                                               

fun number_before_reaching_sum (sum: int, l: int list) : int=   
    if hd l >= sum
    then 0
    else
        1 + number_before_reaching_sum (sum - hd l, tl l)
                                       
val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
                         
fun what_month (day: int):int = 1 + number_before_reaching_sum (day, days_in_months)
                                      
fun map f l =
  if null l then [] else
    f (hd l) :: map f (tl l)

fun range a b =
    if a > b then [] else a :: range (a+1) b
                                     
fun month_range (day1: int, day2: int): int list =
  map what_month (range day1 day2)
    
        
             
                
