(* fun cnt(x:int)=
    if x=0
    then []
    else x::cnt(x-1)

fun append(xs:int list, ys: int list)=
    if null xs
    then ys
    else (hd xs) ::append((tl xs),ys) *)


fun count (from: int)=
    from + 10
val a=count 16