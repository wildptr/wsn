open Printf

open Datatype
open Token

let rec read_all lexer : unit =
  let token, pos = Lexer.next lexer in
  match token with
    | Token.EOF -> ()
    | _ ->
        print_string (syntax_error_to_string (pos, (Token.to_string token)));
        print_newline ();
        read_all lexer

let item_to_string item : string =
  match item with
  | Terminal t ->
      begin match t with
      | Char c -> sprintf "'%c'" c
      | Keyword s -> sprintf "\"%s\"" s
      | Symbol s -> s
      end
  | Nonterminal num -> sprintf "#%d" num

let rule_to_string rule : string =
  String.concat " " (List.map item_to_string rule)

let print_nonterminal_definition ident_num_table name nt : unit =
  printf "%s ::= # %d\n" name (Hashtbl.find ident_num_table name);
  List.iter (fun rule -> printf "  | %s\n" (rule_to_string rule)) nt

let () =
  try
    let lexer = Lexer.create stdin in
    let nonterminal_table, ident_num_table = Parser.parse lexer in
    Hashtbl.iter (print_nonterminal_definition ident_num_table) nonterminal_table
  with SyntaxError (pos, msg) ->
    print_string (syntax_error_to_string (pos, msg));
    print_newline ()
