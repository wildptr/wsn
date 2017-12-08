open Format

type symbol =
  | Terminal of string
  | Nonterminal of string

type expr =
  | Symbol of symbol
  | Concat of expr list
  | Choice of expr list
  | Repeated of expr

type definition = symbol * expr

type grammar = definition list

let string_of_symbol = function
  | Terminal s -> s
  | Nonterminal s -> s

let rec pp_expr f = function
  | Symbol s -> fprintf f "%s" (string_of_symbol s)
  | Concat [] -> fprintf f "()"
  | Concat (e::es) ->
      fprintf f "@[%a" pp_expr e;
      es |> List.iter (fprintf f "@ %a" pp_expr);
      fprintf f "@]"
  | Choice [] -> assert false
  | Choice (e::es) ->
      fprintf f "@[(%a" pp_expr e;
      es |> List.iter (fprintf f " |@ %a" pp_expr);
      fprintf f ")@]"
  | Repeated e ->
      fprintf f "@[{%a}@]" pp_expr e

let pp_definition f (s, e) =
  fprintf f "@[%s =@ %a@] ." (string_of_symbol s) pp_expr e

let pp_grammar f = function
  | [] -> ()
  | def::defs ->
      fprintf f "@[<v>%a" pp_definition def;
      defs |> List.iter (fprintf f "@ %a" pp_definition);
      fprintf f "@]"
