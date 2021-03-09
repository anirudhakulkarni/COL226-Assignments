structure AST =
struct
  datatype form = Id of string
  | forma of string
  | IfFormula of forma * forma * forma 
  | ImFormula of form * string * form 
  | BFormula of form * string * form 
  | NFormula of string * form 
  | PFormula of string *form* string
  fun expToString (f:form)=
    let
    in
      (case f of
        Id id=>id
        | IfFormula(f1,f2,f3)=> "IF "^ expToString f1^" Then "^ expToString f2^" Else "^ expToString f3
        )
    end
end