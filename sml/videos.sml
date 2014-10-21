
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x =
    case x of
        Pizza => 3
     |  Str s => 8
     |  TwoInts(i1,i2) => i1 + i2

datatype exp = Constant of int 
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun max_constant e =
    case e of
        Constant i => i
      | Negate e2 => max_constant e2
      | Add(e1,e2) => Int.max(max_constant e1, max_constant e2)
      | Multiply(e1,e2) => Int.max(max_constant e1, max_constant e2)


val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp



fun n_times (f, n, x) =
    if n=0
    then x
    else f (n_times(f, n - 1, x))

fun addition(n, x) = n_times((+ 1), n, x)
