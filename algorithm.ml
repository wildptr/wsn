type bexp =
  | False
  | True
  | Var of int
  | And of bexp list
  | Or of bexp list

let rec to_bexp index_of = function
  | Ast.Terminal _ -> False
  | Ast.Nonterminal s -> Var (index_of s)
  | Ast.Concat es -> And (List.map (to_bexp index_of) es)
  | Ast.Choice es -> Or (List.map (to_bexp index_of) es)
  | Ast.Repeated _ -> True

module Int = struct
  type t = int
  let compare (a:int) (b:int) = compare a b
end
module IntSet = Set.Make(Int)
module StringSet = Set.Make(String)

let rec nonterminals_used_by = function
  | Ast.Terminal _ -> StringSet.empty
  | Ast.Nonterminal s -> StringSet.add s StringSet.empty
  | Ast.Concat es ->
      let f acc e =
        StringSet.union acc (nonterminals_used_by e)
      in
      es |> List.fold_left f StringSet.empty
  | Ast.Choice es ->
      let f acc e =
        StringSet.union acc (nonterminals_used_by e)
      in
      es |> List.fold_left f StringSet.empty
  | Ast.Repeated e -> nonterminals_used_by e

exception Break

let rec depend = function
  | False | True -> IntSet.empty
  | Var i -> IntSet.add i IntSet.empty
  | And es ->
      let f acc e =
        match e with
        | False -> raise Break
        | _ -> IntSet.union acc (depend e)
      in
      (try es |> List.fold_left f IntSet.empty with Break -> IntSet.empty)
  | Or es ->
      let f acc e =
        match e with
        | True -> raise Break
        | _ -> IntSet.union acc (depend e)
      in
      (try es |> List.fold_left f IntSet.empty with Break -> IntSet.empty)

let rec eval value_of = function
  | False -> false
  | True -> true
  | Var i -> value_of i
  | And es ->
      let f acc e = acc && eval value_of e in
      es |> List.fold_left f true
  | Or es ->
      let f acc e = acc || eval value_of e in
      es |> List.fold_left f false

type analyzed_grammar = {
  grammar : (string * Ast.expr) array;
  ill_defined_nonterminals : int list;
  nullable : bool array;
}

exception Undefined_nonterminals of string list

let analyze ast_grammar =
  let grammar = Array.of_list ast_grammar in
  let n = Array.length grammar in
  (* build name->index table *)
  let name_to_index = Hashtbl.create n in
  grammar |>
  Array.iteri (fun i (nt_name, _) -> Hashtbl.add name_to_index nt_name i);
  (* look for undefined nonterminals *)
  let undefined_nonterminals =
    let used_nonterminals =
      grammar |>
      Array.fold_left (fun acc (_, expr) ->
        StringSet.union acc (nonterminals_used_by expr)) StringSet.empty
    in
    used_nonterminals |>
    StringSet.filter (fun nt_name -> not (Hashtbl.mem name_to_index nt_name)) |>
    StringSet.elements
  in
  if undefined_nonterminals <> [] then
    raise (Undefined_nonterminals undefined_nonterminals);
  let nullable = Array.make n false in
  let ill_defined_nonterminals =
    let resolved = Array.make n false in
    (* convert to bexp *)
    let bexp =
      grammar |>
      Array.map (fun (_, expr) -> to_bexp (Hashtbl.find name_to_index) expr)
    in
    (* dependence graph; edge <a,b> means b depends on a *)
    let g = Array.make n IntSet.empty in
    let g' = Array.make n IntSet.empty in
    let add_edge a b =
      g.(a) <- IntSet.add b g.(a);
      g'.(b) <- IntSet.add a g'.(b)
    in
    let remove_edge a b =
      g.(a) <- IntSet.remove b g.(a);
      g'.(b) <- IntSet.remove a g'.(b)
    in
    (* build dependence graph *)
    for i=0 to n-1 do
      depend bexp.(i) |> IntSet.iter (fun pred -> add_edge pred i)
    done;
    let worklist = Queue.create () in
    for i=0 to n-1 do
      if IntSet.is_empty g'.(i) then Queue.push i worklist;
    done;
    while not (Queue.is_empty worklist) do
      let pred = Queue.pop worklist in
      resolved.(pred) <- true;
      (* value of variable pred is determined *)
      nullable.(pred) <- eval (Array.get nullable) bexp.(pred);
      g.(pred) |>
      IntSet.iter (fun succ ->
        remove_edge pred succ;
        if IntSet.is_empty g'.(succ) then Queue.push succ worklist)
    done;
    (* look for ill-defined nonterminals *)
    let ill = ref [] in
    for i=n-1 downto 0 do
      if not resolved.(i) then ill := i :: !ill;
    done;
    !ill
  in
  { grammar; ill_defined_nonterminals; nullable }
