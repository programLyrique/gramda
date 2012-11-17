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

(* From a string to a list of terminals *)
let toTerminals str =
  let length = String.length str in
  let rec aux = function
    | 0 -> []
    | i  -> (Term(str.[length - i]))::(aux (i - 1))
  in
  aux length

(* Extracts symbols from a part of a right member of a
   production rule
 *)
let extractSymbols str =
  let parts = Str.full_split (Str.regexp "%") str in
  (* delim : false pas de % rencontre ; true, % rencontre *)
  let rec aux delim = function
    | [] -> []
    | (Str.Text(s))::l when not delim ->
      (toTerminals s) @ (aux delim l)
    | (Str.Text(s))::l ->
     ( NTerm(Hashtbl.hash s)) :: (aux delim l)
    | (Str.Delim(s))::l -> aux (not delim) l
  in
  aux false parts
 

(* Converts a line of the grammar into a machine readable
   format
*)
let processLine line =
  let nT, rightMember = cutRule line in
  let transfs = cutRightMember rightMember in
  let rec convert = function
    | [] -> []
    | s::l -> (extractSymbols s)::(convert l)
  in
  (nT, convert transfs)

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
let grammarFromFile path = 
  let file = open_in path in
  try 
    while true do
      let s = input_line file in
      let res = processLine s in ()
    done;
  with _ -> ()
