open Parser;;
open P;;
open F;;
open Bdd;;
open Bdt;;

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
let _ = assert (valuation (Imp(Cst true,Cst false)) = false)
let _ = assert (valuation (Eq(And(Cst true, Cst true),Cst true)) = true) 

(* Debug de valuation*)
let _ = assert (is_constant(And(Var 'b',Var 'a')) = false)

(* Debug de valuation*)
let _ = assert (only_true_valuation(And(Var 'b',Var 'a')) = true)

let a = make_bdt (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
let b = make_bdt (And(Var 'a', Cst false))
let _ = satisfiable (Or(Imp(Var 'p', Var 'q'),And(Var 'r', Var 's')))
let _ = valid (And(Var 'a', Cst false))
(* let s = input_formula ()
let clean_formula = normaliser s
let a = builder clean_formula
let _ = print_endline (toString a) *)