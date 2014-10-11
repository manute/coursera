
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

fun is_month (d: date,m: int): bool =
    #2 d = m

                       
fun number_in_month (xs: date list, m: int): int =
    let
        fun filter([], count) = count
          | filter(dates, count) =                   
            if is_month(hd dates, m) then
                filter(tl dates, count + 1)
            else
                filter(tl dates, count)                                                  
                   
    in       
        filter(xs, 0)

    end  
