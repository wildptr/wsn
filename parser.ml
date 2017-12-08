open Lex

let expect_ident lexer =
  let token, pos = Lexer.peek lexer in
  match token with
  | IDENT s -> Lexer.getsym lexer; s
  | _ -> raise (Syntax_error (pos, "identifier expected"))

let expect_punct lexer punct =
  let token, pos = Lexer.peek lexer in
  let punct_token = Token.of_char punct in
  if token = punct_token
  then Lexer.getsym lexer
  else raise (Syntax_error (pos, Printf.sprintf "‘%c’ expected" punct))

let symbol_of_string s =
  assert (s <> "");
  match s.[0] with
  | 'A'..'Z' -> Ast.Terminal s
  | _ -> Ast.Nonterminal s

let rec parse_expr lexer =
  let rec loop terms =
    let term = parse_term lexer in
    let terms' = term :: terms in
    match fst (Lexer.peek lexer) with
    | OR -> Lexer.getsym lexer; loop terms'
    | _ -> terms'
  in
  let es = List.rev (loop []) in
  match es with
  | [] -> assert false
  | [e] -> e
  | _ -> Ast.Choice es

and parse_term lexer =
  let rec loop factors =
    match fst (Lexer.peek lexer) with
    | IDENT _ | CHAR _ | STRING _ | LPAREN | LBRACK | LBRACE ->
        let factor = parse_factor lexer in
        loop (factor :: factors)
    | _ -> factors
  in
  let es = List.rev (loop []) in
  match es with
  | [] -> Ast.Concat []
  | [e] -> e
  | _ -> Ast.Concat es

and parse_factor lexer =
  let token, pos = Lexer.peek lexer in
  match token with
  | IDENT s ->
      Lexer.getsym lexer;
      symbol_of_string s
  | CHAR c ->
      Lexer.getsym lexer;
      Ast.Terminal (Printf.sprintf "'%c'" c)
  | STRING s ->
      Lexer.getsym lexer;
      Ast.Terminal (Printf.sprintf "\"%s\"" s)
  | LPAREN ->
      Lexer.getsym lexer;
      let e = parse_expr lexer in
      expect_punct lexer ')';
      e
  | LBRACK ->
      Lexer.getsym lexer;
      let e = parse_expr lexer in
      expect_punct lexer ']';
      Ast.Choice [Ast.Concat []; e]
  | LBRACE ->
      Lexer.getsym lexer;
      let e = parse_expr lexer in
      expect_punct lexer '}';
      Ast.Repeated e
  | _ -> raise (Syntax_error (pos, "factor expected"))

let parse_definition lexer =
  let head = expect_ident lexer in
  expect_punct lexer '=';
  let body = parse_expr lexer in
  expect_punct lexer '.';
  head, body

let parse lexer =
  let rec loop defs =
    let token, pos = Lexer.peek lexer in
    match token with
    | IDENT _ ->
        let def = parse_definition lexer in
        loop (def :: defs)
    | EOF -> defs
    | _ ->
        let msg =
          Printf.sprintf "Unexpected token: ‘%s’" (Token.to_string token)
        in
        raise (Syntax_error (pos, msg))
  in
  List.rev (loop [])
