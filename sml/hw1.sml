
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
                       

fun is_month d m = #2 d = m

fun filter _ nil      = nil
  | filter f (hd::tl) = if (f hd) then hd::(filter f tl)
                        else (filter f tl) 
                                     
fun number_in_month (xs, m) = filter fn x => is_month(x, m) xs                          
        

fun number_in_months (xs, ms) =
    if null ms then
        0
    else
        length number_in_month(xs, hd ms) + length number_in_months(xs, tl ms)
                                           
        
fun dates_in_month (xs : date list , m: int): date list =
    let
        fun filter([], dates_filter) = dates_filter                                      
          | filter(dates, dates_filter) =                   
            if is_month(hd dates, m) then
                filter(tl dates, hd dates :: dates_filter)
            else
                filter(tl dates, dates_filter)                                                          
                   
    in       
        filter(xs, [])

    end
    
