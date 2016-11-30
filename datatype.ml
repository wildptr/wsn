type terminal =
  | Char of char
  | Keyword of string
  | Symbol of string

type item =
  | Terminal of terminal
  | Nonterminal of int

type nonterminal = item list list
