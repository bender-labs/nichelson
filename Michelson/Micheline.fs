namespace Bender.Michelson.Micheline

type Annotation = string list

type Prim =
    | T_Nat
    | T_Or
    | T_Pair
    | T_String
    | T_Address
    | D_Pair
    | D_Right
    | D_Left

type PrimExpression =
    { Prim: Prim
      Args: Expr
      Annotations: Annotation }
    static member Create(p: Prim, ?args: Expr, ?annotation: Annotation) =
        { Prim = p
          Args = defaultArg args (Seq [])
          Annotations = defaultArg annotation [] }

and Expr =
    | Int of int64
    | String of string
    | Bytes of byte array
    | Node of PrimExpression
    | Seq of Expr list

module Expr =
    let foldNodeOrSeq fNode fSeq state expr =
        match expr with
        | Node n -> fNode state n
        | Seq s -> fSeq state s
        | _ -> state

    let fold f state expr =
        let rec folder s' expr' =
            match expr' with
            | Node n ->
                let newState = f s' expr'
                folder newState n.Args
            | Seq s ->
                let newState = f s' expr'
                s |> List.fold folder newState
            | _ -> f state expr'

        folder state expr

    let private orArgs (Seq ([ left; right ])) = (left, right)

    let (|AnnotatedOr|_|) input =
        match input with
        | Node v ->
            match v.Prim with
            | T_Or -> if v.Annotations.IsEmpty then None else Some(v, v.Args |> orArgs)
            | _ -> None
        | _ -> None

    let (|Or|_|) input =
        match input with
        | Node v ->
            match v.Prim with
            | T_Or -> if v.Annotations.IsEmpty then Some(v, v.Args |> orArgs) else None
            | _ -> None
        | _ -> None

    let (|AnnotatedNode|_|) input =
        match input with
        | Node v -> if v.Annotations.IsEmpty then None else Some(v)
        | _ -> None
