type position = int * int (* line, col *)
type syntax_error = position * string
exception Syntax_error of syntax_error 

let format_syntax_error (pos, msg) =
  let line, col = pos in
  Printf.sprintf "%d:%d: %s" line col msg
