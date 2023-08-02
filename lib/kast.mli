type variable = Sym of string | Auto of string * int
type kvariable = variable
type value =
  | Unit
  | Lit of string
  | Integer of int
  | Pair of kvariable * kvariable
  | LeftV of variable
  | RightV of variable
val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

type t =
  | LetVal of variable * value * t
  | LetFun of variable * variable * kvariable * t * t
  | LetFst of variable * variable * t
  | LetSnd of variable * variable * t
  | LetCont of kvariable * variable * t * t
  | ContCall of kvariable * variable
  | FuncCall of variable * variable * kvariable
  | Case of variable * kvariable * kvariable
  
module Debug :
  sig
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

val pp : Format.formatter -> t -> unit
val pp_variable : Format.formatter -> variable -> unit
val new_fn : string -> variable
val new_var : string -> variable
