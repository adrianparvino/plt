open Kast

type lisp =
  | Symbol of string
  | Lit of string
  | Fn of string list * lisp
  | If of lisp * lisp * lisp
  | Cons of lisp * lisp
  | App of lisp * lisp list
  | Let of (string * lisp) list * lisp
[@@deriving show]

type context = unit

type cont =
  | KCont of { k : kvariable }
  | KFnCall of { right : lisp; next : cont }
  | KFnCall2 of { left : variable; next : cont }
  | Cons1 of { right : lisp; next : cont }
  | Cons2 of { left : variable; next : cont }
  | Case of { t : lisp; f : lisp; next : cont }

let rec trans ast trace =
  let[@tail_mod_cons] rec make trace = function
    | Symbol s -> break (Sym s) trace
    | Lit l -> (
        let x = new_var "x" in
        match int_of_string_opt l with
        | Some i -> LetVal (x, Integer i, break x trace)
        | None -> LetVal (x, Lit l, break x trace))
    | Fn ([ x ], body) ->
        let f = new_fn "f" in
        let k = new_fn "k" in
        LetFun (f, Sym x, k, trans body (KCont { k }), break f trace)
    | Fn (x :: rest, body) -> make trace (Fn ([ x ], Fn (rest, body)))
    | If (cond, t, f) -> make (Case { t; f; next = trace }) cond
    | Cons (left, right) -> make (Cons1 { right; next = trace }) left
    | App (f, [ x ]) -> make (KFnCall { right = x; next = trace }) f
    | App (f, x :: rest) -> make trace (App (App (f, [ x ]), rest))
    | Let ([ (x, value) ], body) ->
      let j = new_fn "j" in
      LetCont (j, Sym x, make trace body, trans value (KCont { k = j }))
    | Let ((binding::rest), body) -> make trace (Let ([binding], Let (rest, body)))
    | _ -> failwith "Unmatched language construct"
  and[@tail_mod_cons] break variable = function
    | KCont { k } -> ContCall (k, variable)
    | KFnCall { right; next = trace } ->
        make (KFnCall2 { left = variable; next = trace }) right
    | KFnCall2 { left; next = KCont { k } } -> FuncCall (left, variable, k)
    | KFnCall2 { left; next = trace } ->
        let k = new_fn "k" in
        let x = new_var "x" in
        LetCont (k, x, break x trace, FuncCall (left, variable, k))
    | Cons1 { right; next = trace } ->
        make (Cons2 { left = variable; next = trace }) right
    | Cons2 { left; next = trace } ->
        let x = new_var "x" in
        LetVal (x, Pair (left, variable), break x trace)
    | Case { t; f; next = KCont { k = j } } ->
        let tk = new_fn "tk" in
        let tx = new_var "tx" in
        let fk = new_fn "fk" in
        let fx = new_var "fx" in
        LetCont
          ( tk,
            tx,
            trans t (KCont { k = j }),
            LetCont (fk, fx, trans f (KCont { k = j }), Case (variable, tk, fk))
          )
    | Case { t; f; next } ->
        let j = new_fn "j" in
        let x = new_var "x" in
        let tk = new_fn "tk" in
        let tx = new_var "tx" in
        let fk = new_fn "fk" in
        let fx = new_var "fx" in
        LetCont
          ( j,
            x,
            break x next,
            LetCont
              ( tk,
                tx,
                trans t (KCont { k = j }),
                LetCont
                  (fk, fx, trans f (KCont { k = j }), Case (variable, tk, fk))
              ) )
    (* | _ -> failwith "Break" *)
  in
  make trace ast
