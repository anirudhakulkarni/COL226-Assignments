structure AST =
struct
  datatype exp = Num of int
  | Id of string
  | BinApp of binop * exp * exp
  and binop = Add | Sub | Mul | Div
  fun expToString (expr:exp)=
    let
      fun opToString (opr:binop)=
        (case opr of
           Add => "+"
         | Sub => "-"
         | Mul => "*"
         | Div => "/")
    in
      (case expr of
        Num n => Int.toString(n)
        | Id id=>id
        | BinApp(opr,e1,e2)=> expToString e1^ " "^opToString (opr)^" "^expToString e2
        )
    end
end