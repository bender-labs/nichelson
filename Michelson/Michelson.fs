namespace Bender.Michelson

type Annotation = string list

type Prim =
    | T_Nat
    | T_Or
    | T_Pair
    | T_String
    | T_Address
    | T_Signature
    | T_ChainId
    | T_List
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
    | IntLiteral of int64
    | StringLiteral of string
    | BytesLiteral of byte array
    | Node of PrimExpression
    | Seq of Expr list

[<AutoOpen>]
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

    let (|NamedOr|_|) input =
        match input with
        | Node ({ Prim = T_Or; Annotations = name :: _ } as v) -> Some(v, name, v.Args |> orArgs)
        | _ -> None

    let (|Or|_|) input =
        match input with
        | Node ({ Prim = T_Or } as v) -> Some(v, v.Args |> orArgs)
        | _ -> None

    let (|NamedNode|_|) input =
        match input with
        | Node ({ Annotations = name :: _ ; Args = args} as n)  -> Some (n, name, args)
        | _ -> None
        
    let (|ANode|_|) input =
        match input with
        | Node ({ Args = args} as n)  -> Some (n, args)
        | _ -> None        

    let (|Primitive|_|) input =
        match input with
        | Node v ->
            match v.Prim with
            | T_String
            | T_Nat
            | T_Address
            | T_Signature -> Some(v)
            | _ -> None
        | _ -> None

    let (|IntLiteral|_|) input =
        match input with
        | IntLiteral i -> Some i
        | _ -> None
