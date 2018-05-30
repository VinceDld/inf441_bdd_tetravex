open Parser;;
open P;;
open F;;

(* Debug de split *)
let _ = assert (stringToArray "azert" = ['a'; 'z'; 'e'; 'r'; 't'])
let _ = assert (split "(1)A(2)" = StringSplit("1", 'A', "2"))
let _ = assert (split "(1)A2" = StringSplit("1", 'A', "2"))
let _ = assert (split "1A2" = StringSplit("1", 'A', "2"))
let _ = assert (split "1A(2)" = StringSplit("1", 'A', "2"))
let _ = assert (split "N(2)" = NotOperation("2"))
let _ = assert (split "N2" = NotOperation("2"))
let _ = assert (split "a" = Single("a"))

(* Debug de builder *)
let _ = assert (builder "a" = Var('a'))
let _ = assert (builder "T" = Cst(true))
let _ = assert (builder "aAb" = And(Var('a'), Var('b')))
let _ = assert (builder "(aAb)Oc" = Or(And(Var('a'), Var('b')),Var('c')))
let _ = assert (builder "aA(bOc)" = And(Var('a'), Or(Var('b'),Var('c'))))
let _ = assert (builder "Na" = Not(Var('a')))
let _ = assert (builder "N(aA(bOc))" = Not(And(Var('a'), Or(Var('b'),Var('c')))))

(* Debug de replace, replace_all*)
let _ = assert (replace_all (Or(Not(Var 'a'),Var 'c')) [('a',true) ; ('c',false)] = Or(Not(Cst true),Cst false))
let _ = assert (replace_all (Eq(And(Var 'a', Cst true),Var 'b')) [('a',true)] = Eq(And(Cst true, Cst true),Var 'b'))

(* Debug de valuation*)
let _ = assert (valuation (Or(Not(Cst true),Cst false)) = false)
let _ = assert (valuation (Imp(Cst true,Cst false)) = true)
let _ = assert (valuation (Eq(And(Cst true, Cst true),Cst true)) = true)

let s = input_formula ()
let clean_formula = P.normaliser s
let a = builder clean_formula
let c = replace_all a [('c' , true) ; ('b' , false) ]
let _ = if valuation c then print_endline "true" else print_endline "false"