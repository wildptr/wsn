type t

val create : in_channel -> t
val peek : t -> Token.t * Lex.position
val getsym : t -> unit
