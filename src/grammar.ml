(** Represents a grammar.
   Manages the production rule, the axiom (start symbol), the terminal and non terminal symbols...
*)

type terminal = char

(*
  We apply a conversion of non terminals symbols chosen by the 
  user to integers. All the identifiers which are on the left 
  of an arrow and the strings started by a % and terminated by
  a space or a new line.
*)
type nonTerminal = int

type symbol = Term of terminal | NTerm of nonTerminal

(* A nonTerminal is the axiom ,
   the other member of the tuple is the rules
*)
type grammar = nonTerminal * 
( ( nonTerminal -> symbol list) list )

