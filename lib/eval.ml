module rec Env : sig
  type t

  val pp : Format.formatter -> t -> unit
  val get : Kast.variable -> t -> Value.t
  val put : Kast.variable -> Value.t -> t -> t
  val empty : t
end = struct
  type t = (Kast.variable * Value.t) list [@@deriving show]

  let get = List.assoc
  let put k v env = (k, v) :: env
  let empty = []
end

and Value : sig
  type t =
    | Unit
    | Lit of string
    | Integer of int
    | Pair of t * t
    | LeftV of t
    | RightV of t
    | PrimK of (t -> t)
    | PrimF of (t -> (t -> t) -> t)

  val pp : Format.formatter -> t -> unit
  val from : Env.t -> Kast.value -> t
end = struct
  type t =
    | Unit
    | Lit of string
    | Integer of int
    | Pair of t * t
    | LeftV of t
    | RightV of t
    | PrimK of (t -> t)
    | PrimF of (t -> (t -> t) -> t)
  [@@deriving show]

  let from env = function
    | Kast.Unit -> Unit
    | Kast.Lit s -> Lit s
    | Kast.Integer i -> Integer i
    | Kast.LeftV v -> LeftV (Env.get v env)
    | Kast.RightV v -> RightV (Env.get v env)
    | Kast.Pair (a, b) -> Pair (Env.get a env, Env.get b env)
end

let eval =
  let rec eval (env : Env.t) : Kast.t -> Value.t = function
    | LetVal (var, value, cont) ->
        eval Env.(env |> put var (Value.from env value)) cont
    | LetCont (cont, arg, impl, body) ->
        let env =
          Env.(
            env
            |> put cont (PrimK (fun x -> eval Env.(env |> put arg x) impl)))
        in
        eval env body
    | LetFun (f, arg, cont, impl, body) ->
        let env =
          Env.(
            env
            |> put f
                 (PrimF
                    (fun x k ->
                      eval Env.(env |> put arg x |> put cont (PrimK k)) impl)))
        in
        eval env body
    | ContCall (cont, var) ->
        let k =
          Env.get cont env |> function PrimK k -> k | _ -> failwith "awa"
        in
        let x = Env.get var env in
        k x
    | FuncCall (f, var, cont) ->
        let f =
          Env.get f env |> function PrimF f -> f | _ -> failwith "awa"
        in
        let k =
          Env.get cont env |> function PrimK k -> k | _ -> failwith "awa"
        in
        let x = Env.get var env in
        f x k
    | Case (x, l, r) -> (
        let x = Env.get x env in
        let l =
          Env.get l env |> function PrimK k -> k | _ -> failwith "awa"
        in
        let r =
          Env.get r env |> function PrimK k -> k | _ -> failwith "awa"
        in
        match x with LeftV x -> l x | RightV x -> r x | _ -> failwith "awa")
    | _ -> failwith "awa"
  in
  eval
    Env.(
      empty
      |> put (Sym "halt") (PrimK (fun x -> x))
      |> put (Sym "print")
           (PrimF
              (fun x k ->
                Format.printf "%a@." Value.pp x;
                k Unit))
      |> put (Sym "fst")
           (PrimF
              (fun p k ->
                match p with
                | Pair (a, _) -> k a
                | _ -> failwith "fst: argument must be a pair"))
      |> put (Sym "snd")
           (PrimF
              (fun p k ->
                match p with
                | Pair (_, b) -> k b
                | _ -> failwith "snd: argument must be a pair"))
      |> put (Sym "true") (LeftV Unit)
      |> put (Sym "false") (RightV Unit)
      |> put (Sym "-")
           (PrimF
              (fun x k ->
                match x with
                | Integer x -> k (Integer (-x))
                | _ -> failwith "awa"))
      |> put (Sym "call/cc")
           (PrimF
              (fun f k ->
                match f with
                | PrimF f -> f (PrimF (fun x _ -> k x)) k
                | _ -> failwith "awa"))
      |> put (Sym "#+")
           (PrimF
              (fun x k ->
                match x with
                | Pair (Integer x, Integer y) -> k (Integer (x + y))
                | Pair (Integer _, _) ->
                    failwith "#+: second argument must be an integer"
                | Pair _ -> failwith "#+: first argument must be an integer"
                | _ -> failwith "#+: argument must be a tuple")))
