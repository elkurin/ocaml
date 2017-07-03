type tyvar = string
type ty = TyInt | TyBool | TyFun of ty * ty | TyVar of tyvar | TyPair of ty * ty | TyList of ty | TyNil
let rec print_type t = 
    match t with
    | TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyFun (a,b) -> print_string "("; print_type a; print_string " -> "; print_type b; print_string ")"
    | TyVar a -> print_string a
    | TyPair (a,b) -> print_string "("; print_type a; print_string " * "; print_type b; print_string ")"
    | TyList (a) -> print_type a; print_string " list"
    | TyNil -> print_string "nil"

let counter = ref 0
let alphabet = [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"|]
let new_tyvar () = 
    if !counter < 26 then (counter := !counter + 1; "'" ^ alphabet.(!counter - 1))
    else (counter := !counter + 1; "'" ^ alphabet.((!counter - 1) mod 26) ^ string_of_int((!counter - 1) / 26))
