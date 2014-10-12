
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
                       

fun filter _ nil = nil
  | filter f (hd::tl) = if (f hd) then hd::(filter f tl)
                        else (filter f tl) 
                                     
fun number_in_month (xs: date list, m: int): int =
    length (filter (fn x => #2 x = m ) xs)                      
        

fun number_in_months (xs: date list, ms: int list): int =
    if null ms then
        0
    else
        number_in_month(xs, hd ms) + number_in_months(xs, tl ms)
                                           
        
fun dates_in_month (xs : date list , m: int): date list =
    filter (fn x => #2 x = m ) xs
    
