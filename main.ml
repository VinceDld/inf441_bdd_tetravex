open Parser;;
open P;;
open F;;
open Bdd;;
open Bdt;;

let s = input_formula ()
let clean_formula = normaliser s
let a = builder clean_formula
let _ = satisfiable (a)
let _ = valid (a)
