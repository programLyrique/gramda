(** Represents a grammar.
   Manages the production rule, the axiom (start symbol), the terminal and non terminal symbols...
*)

exception SyntaxError of (int * string)

type terminal = char

(*
  We apply a conversion of non terminals symbols chosen by the 
  user to integers. All the identifiers which are on the left 
  of an arrow and the strings started by a % and terminated
  by %.
*)
type nonTerminal = int

type symbol = Term of terminal | NTerm of nonTerminal

(* A nonTerminal is the axiom ,
   the other member of the tuple is the rules
*)
type grammar = nonTerminal * 
( ( nonTerminal -> symbol list list) list )

(* Cut the rule into two parts : the non terminal on the left,
the transformation on the right.
*)
let cutRule r = 
  let res = Str.split (Str.regexp "->") r in
  (List.hd res, List.hd (List.tl res))

(* Cut the right member of a production rule into its different
components, separated by |
*)
let cutRightMember r = Str.split (Str.regexp "|") r

(* Extracts symbols from a part of a right member of a
   production rule
 *)
let extractSymbols str =
  let length = String.length str in
  let rec nextSymbol i =
    if i < length then
      (
	if str.[i] = '%' then
	  (
	    let j = ref (i+1) in
	    (* If over the limits of the string, raise
	       an exception.
	       Which will be caught and thrown again as a 
	       syntax error.
	    *)
	    while str.[!j] <> '%' do
	      incr j
	    done;
	    (* The symbol, the place to start*)
	    (NTerm(Hashtbl.hash (String.sub str (i+1) (!j))))::
	      nextSymbol (!j+1)
	  )
	else
	  (
	    let j = ref (i+1) in
	    while !j < length || str.[!j] <> '%' do
	      incr j
	    done;
	    let e = if !j < length then !j else !j - 1 in
	    (Term(String.sub str (i+1) e))::nextSymbol (!j + 1)
	  )
      )
  in
  nextSymbol 0

 
(* Format of a file representing a grammar.
first line :
   'nonTerminal' -> production rules right member.
 'nonTerminal' is the start symbol
Next lines :
   'nonTerminal' -> production rules right member
A right member :
terminal and non terminal symbols, on terminal ones are enclosed
by %, separated by |.
For instance :
S -> a%S%b | d%S%%S%
*)

(* The start symbol is noted S *)
let fromFile path = 
  let file = input_in path in
  try 
    while true do
      let s = input_line file in
      let nT, rightMember = cutRule s in
      let transfs = cutRightMember rightMember in
      
