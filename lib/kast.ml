type variable = Sym of string | Auto of string * int [@@deriving show]

type kvariable = variable [@@deriving show]

type value =
  | Unit
  | Lit of string
  | Integer of int
  | Pair of variable * variable
  | LeftV of variable
  | RightV of variable
[@@deriving show]

module Debug = struct
  type t =
    | LetVal of variable * value * t
    | LetFun of variable * kvariable * variable * t * t
    | LetFst of variable * variable * t
    | LetSnd of variable * variable * t
    | LetCont of kvariable * variable * t * t
    | ContCall of kvariable * variable
    | FuncCall of variable * kvariable * variable
    | Case of variable * kvariable * kvariable
  [@@deriving show]
end

type t = Debug.t =
  | LetVal of variable * value * t
  | LetFun of variable * variable * kvariable * t * t
  | LetFst of variable * variable * t
  | LetSnd of variable * variable * t
  | LetCont of kvariable * variable * t * t
  | ContCall of kvariable * variable
  | FuncCall of variable * variable * kvariable
  | Case of variable * kvariable * kvariable

let pp_variable f var =
  let open Format in
  match var with
  | Sym s -> pp_print_string f s
  | Auto (s, i) -> fprintf f "%s#%d" s i

let pp_value f value =
  let open Format in
  match value with
  | Unit -> pp_print_string f "()"
  | Lit s -> pp_print_string f s
  | Integer i -> pp_print_int f i
  | Pair (x, y) -> fprintf f "(%a, %a)" pp_variable x pp_variable y
  | LeftV s -> fprintf f "(Left %a)" pp_variable s
  | RightV s -> fprintf f "(Right %a)" pp_variable s

let pp fmt kast =
  let open Format in
  let rec pp fmt kast =
    match kast with
    | LetVal (x, v, e) ->
        fprintf fmt "@[<v 4>let %a =@ %a in@]@;%a" pp_variable x pp_value v pp e
    | LetFun (f, k, x, e1, e2) ->
        fprintf fmt "@[<v 4>let %a %a %a =@ %a in@]@;%a" pp_variable f
          pp_variable k pp_variable x pp e1 pp e2
    | LetCont (k, x, e1, e2) ->
        fprintf fmt "@[<v 4>let %a %a =@ %a in@]@;%a" pp_variable k pp_variable
          x pp e1 pp e2
    | FuncCall (f, k, x) ->
        fprintf fmt "%a %a %a" pp_variable f pp_variable k pp_variable x
    | ContCall (k, x) -> fprintf fmt "%a %a" pp_variable k pp_variable x
    | Case (z, kt, kf) ->
        fprintf fmt "case %a of %a || %a" pp_variable z pp_variable kt
          pp_variable kf
    | _ -> failwith ""
  in
  fprintf fmt "@[<v>%a@]" pp kast

let new_fn =
  let counter = ref 0 in
  fun x ->
    counter := !counter + 1;
    Auto (x, !counter)

let new_var = new_fn
