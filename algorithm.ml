type expr =
  | Terminal of int
  | Nonterminal of int
  | Concat of expr list
  | Choice of expr list
  | Repeated of expr

let rec convert_expr terminal_dict nonterminal_dict =
  let rec f = function
    | Ast.Terminal s -> Terminal (Hashtbl.find terminal_dict s)
    | Ast.Nonterminal s -> Nonterminal (Hashtbl.find nonterminal_dict s)
    | Ast.Concat es -> Concat (List.map f es)
    | Ast.Choice es -> Choice (List.map f es)
    | Ast.Repeated e -> Repeated (f e)
  in
  f

let rec union l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | hd1::tl1, hd2::tl2 ->
    let hd, tl =
      if hd1 < hd2 then hd1, union tl1 l2 else hd2, union l1 tl2
    in
    match tl with
    | [] -> [hd]
    | hd'::tl' -> if hd = hd' then tl else hd::tl

let union_set = List.fold_left union []

let rec symbols_used_by ast_expr =
  let fold es =
    es |> List.fold_left (fun acc e -> union acc (symbols_used_by e)) []
  in
  match ast_expr with
  | Ast.Terminal _ | Ast.Nonterminal _ -> [ast_expr]
  | Ast.Concat es -> fold es
  | Ast.Choice es -> fold es
  | Ast.Repeated e -> symbols_used_by e

(* X is well defined iff X can derive a string of terminals *)
let rec is_well_defined table =
  let rec f = function
    | Terminal _ -> true
    | Nonterminal i -> table.(i)
    | Concat es -> List.for_all f es
    | Choice es -> List.exists f es
    | Repeated _ -> true
  in
  f

let rec is_nullable table =
  let rec f = function
    | Terminal _ -> false
    | Nonterminal i -> table.(i)
    | Concat es -> List.for_all f es
    | Choice es -> List.exists f es
    | Repeated _ -> true
  in
  f

let rec first_set_of nullable table =
  let rec f = function
    | Terminal i -> [i]
    | Nonterminal i -> table.(i)
    | Concat es ->
      let rec g = function
        | [] -> []
        | e::es' ->
          let first_e = f e in
          if is_nullable nullable e
          then union first_e (g es')
          else first_e
      in
      g es
    | Choice es ->
      es |> List.map f |> union_set
    | Repeated e -> f e
  in
  f

let least_fixpoint grammar f =
  let n = Array.length grammar in
  let table = Array.make n false in
  let changed = ref false in
  let rec loop () =
    for i=0 to n-1 do
      if not table.(i) then begin
        if f table grammar.(i) then begin
          table.(i) <- true;
          changed := true
        end
      end
    done;
    if !changed then begin
      changed := false;
      loop ()
    end
  in
  loop ();
  table

let least_fixpoint_set grammar f =
  let n = Array.length grammar in
  let table = Array.make n [] in
  let changed = ref false in
  let rec loop () =
    for i=0 to n-1 do
      let old_set = table.(i) in
      let new_set = f table grammar.(i) in
      if old_set <> new_set then begin
        table.(i) <- new_set;
        changed := true
      end
    done;
    if !changed then begin
      changed := false;
      loop ()
    end
  in
  loop ();
  table

type analyzed_grammar = {
  grammar : expr array;
  terminal_count : int;
  nonterminal_count : int;
  terminal_array : string array;
  nonterminal_array : string array;
  terminal_dict : (string, int) Hashtbl.t;
  nonterminal_dict : (string, int) Hashtbl.t;
  well_defined : bool array;
  nullable : bool array;
  first_set_array : int list array;
}

exception Undefined_nonterminals of string list

let analyze ast_grammar =
  let ast_grammar_array = Array.of_list ast_grammar in
  let terminals, used_nonterminals =
    let ast_t, ast_n =
      ast_grammar_array |>
      Array.fold_left (fun acc (_, expr) ->
          union acc (symbols_used_by expr)) [] |>
      List.partition (function Ast.Terminal _ -> true | _ -> false)
    in
    List.map (function Ast.Terminal s -> s | _ -> assert false) ast_t,
    List.map (function Ast.Nonterminal s -> s | _ -> assert false) ast_n
  in

  (* build name<->index mapping for terminals *)
  let terminal_array = Array.of_list terminals in
  let m = List.length terminals in
  let terminal_dict = Hashtbl.create m in
  terminals |> List.iteri (fun i t_name -> Hashtbl.add terminal_dict t_name i);

  (* build name<->index mapping for nonterminals *)
  let n = Array.length ast_grammar_array in
  let nonterminal_array = ast_grammar_array |> Array.map fst in
  let nonterminal_dict = Hashtbl.create n in
  ast_grammar_array |>
  Array.iteri (fun i (nt_name, _) -> Hashtbl.add nonterminal_dict nt_name i);

  (* look for undefined nonterminals *)
  let undefined_nonterminals =
    used_nonterminals |>
    List.filter (fun nt_name ->
        not (Hashtbl.mem nonterminal_dict nt_name))
  in
  if undefined_nonterminals <> [] then
    raise (Undefined_nonterminals undefined_nonterminals);

  (* convert expressions *)
  let grammar : expr array =
    ast_grammar_array |>
    Array.map (fun (_, ast_expr) ->
        convert_expr terminal_dict nonterminal_dict ast_expr);
  in

  let well_defined = least_fixpoint grammar is_well_defined in

  let nullable = least_fixpoint grammar is_nullable in

  let first_set_array = least_fixpoint_set grammar (first_set_of nullable) in

  {
    grammar;
    terminal_count = m;
    nonterminal_count = n;
    terminal_array;
    nonterminal_array;
    terminal_dict;
    nonterminal_dict;
    well_defined;
    nullable;
    first_set_array;
  }
