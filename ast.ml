open Format

type expr =
  | Terminal of string
  | Nonterminal of string
  | Concat of expr list
  | Choice of expr list
  | Repeated of expr

type definition = string * expr

type grammar = definition list

let rec pp_expr f = function
  | Terminal s -> fprintf f "%s" s
  | Nonterminal s -> fprintf f "%s" s
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
  fprintf f "@[%s =@ %a@] ." s pp_expr e

let pp_grammar f = function
  | [] -> ()
  | def::defs ->
      fprintf f "@[<v>%a" pp_definition def;
      defs |> List.iter (fprintf f "@ %a" pp_definition);
      fprintf f "@]"
