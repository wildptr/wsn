open Datatype
open Token

type environment = {
  nonterminal_table : (string, nonterminal) Hashtbl.t;
  ident_num_table : (string, int) Hashtbl.t;
}

let num_of_ident env ident =
  if Hashtbl.mem env.ident_num_table ident
  then Hashtbl.find env.ident_num_table ident
  else begin
    let n = Hashtbl.length env.ident_num_table in
    Hashtbl.add env.ident_num_table ident n;
    n
  end

let parse_definition_intro lexer : string =
  let id = Token.check_id (Lexer.next lexer) in
  Token.check_def (Lexer.next lexer);
  Token.check_newline (Lexer.next lexer);
  id

let rec parse_inline_nonterminal env lexer : item list list =
  let rec recurse rules =
    match fst (Lexer.peek lexer) with
    | OR -> Lexer.lex lexer; recurse (parse_rule env lexer :: rules)
    | _ -> List.rev rules
  in
  match fst (Lexer.peek lexer) with
  | ID _ | CHAR _ | STRING _ | LPAREN | LBRACK | LBRACE ->
      recurse [parse_rule env lexer]
  | _ -> []

and parse_item env lexer : item =
  let synthesize_name (pos : Token.position) =
    let line, col = pos in
    let name = Printf.sprintf "%d:%d" line col in
    name
  in
  let token, pos = Lexer.next lexer in
  match token with
  | ID s ->
      begin match s.[0] with
      | 'A'..'Z' -> (Terminal (Symbol s))
      | _ -> Nonterminal (num_of_ident env s)
      end
  | CHAR c -> (Terminal (Char c))
  | STRING s -> (Terminal (Keyword s))
  | LPAREN ->
      let inline_nt = parse_inline_nonterminal env lexer in
      Token.check_rparen (Lexer.next lexer);
      let name = synthesize_name pos in
      Hashtbl.add env.nonterminal_table name inline_nt;
      Nonterminal (num_of_ident env name)
  | LBRACK ->
      let inline_nt = parse_inline_nonterminal env lexer in
      Token.check_rbrack (Lexer.next lexer);
      let name = synthesize_name pos in
      Hashtbl.add env.nonterminal_table name ([] :: inline_nt);
      Nonterminal (num_of_ident env name)
  | LBRACE ->
      let inline_nt = parse_inline_nonterminal env lexer in
      Token.check_rbrace (Lexer.next lexer);
      let inner_name = synthesize_name pos in
      Hashtbl.add env.nonterminal_table inner_name inline_nt;
      let outer_name = inner_name ^ "'" in
      let outer_item = Nonterminal (num_of_ident env outer_name) in
      Hashtbl.add env.nonterminal_table outer_name [[]; [Nonterminal (num_of_ident env inner_name); outer_item]];
      outer_item
  | _ -> raise (SyntaxError (pos, "invalid item"))

and parse_rule env lexer : item list =
  let rec recurse items =
    match fst (Lexer.peek lexer) with
    | ID _ | CHAR _ | STRING _ | LPAREN | LBRACK | LBRACE ->
        recurse (parse_item env lexer :: items)
    | _ -> items
  in
  List.rev (recurse [])

let parse_rule_with_newline env lexer : item list =
  Token.check_or (Lexer.next lexer);
  let rule = parse_rule env lexer in
  Token.check_newline (Lexer.next lexer);
  rule

let parse_definition env lexer : unit =
  let name = parse_definition_intro lexer in
  let _ = num_of_ident env name in
  let rec recurse rules =
    match fst (Lexer.peek lexer) with
    | OR -> recurse (parse_rule_with_newline env lexer :: rules)
    | _ -> rules
  in
  let nt = recurse [] in
  Hashtbl.add env.nonterminal_table name nt

let parse lexer =
  let env = {
    nonterminal_table = Hashtbl.create 256;
    ident_num_table = Hashtbl.create 256;
  } in
  let rec recurse () =
    let token, pos = Lexer.peek lexer in
    match token with
    | NEWLINE ->
        Lexer.lex lexer;
        recurse ()
    | ID _ ->
        parse_definition env lexer;
        recurse ()
    | EOF -> ()
    | _ -> raise (SyntaxError (pos, (Printf.sprintf "Unexpected token: ‘%s’" (Token.to_string token))))
  in
  recurse ();
  (env.nonterminal_table, env.ident_num_table)
