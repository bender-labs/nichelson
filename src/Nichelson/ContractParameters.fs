namespace Nichelson.Contract

open Nichelson
open Nichelson.Parser.Michelson

type Arg =
    | Record of (string * Arg) list
    | Either of Either
    | List of Arg list
    | Value of Value
    | Tuple of Arg list

and Either =
    | Left of Arg
    | Right of Arg

and Value =
    | Int of bigint
    | String of string
    | Bytes of byte array
    | Address of TezosAddress.T
    | Signature of Signature.T
    | Key of PublicKey.T


module Arg =
    let Rec = Record

    let LeftArg = Left >> Either

    let RightArg = Right >> Either

    let IntArg = Int >> Arg.Value

    let StringArg = String >> Arg.Value

    let BytesArg = Bytes >> Arg.Value

    let SignatureArg = Signature >> Arg.Value

    let AddressArg = Address >> Arg.Value

    let Find (fields: (string * Arg) list) key =
        let r =
            fields |> List.tryFind (fun (k, _) -> k = key)

        match r with
        | Some (_, v) -> v
        | None -> failwith (sprintf "Field %s not found in %s" key (fields.ToString()))

type Values =
    | Named of Map<string, obj>
    | Unnamed of obj list


type EntryPoint =
    { Name: string
      Expression: PrimExpression
      Path: Prim list }

[<RequireQualifiedAccess>]
module private EntryPoint =
    let create name expr path =
        { Name = name
          Expression = { expr with Annotations = [] }
          Path = path }

module private Extract =

    let cast (value: Expr) : 'v =
        match value with
        | IntLiteral v -> v |> Unchecked.unbox<'v>
        | BytesLiteral v -> v |> Unchecked.unbox<'v>

    let extract (annot: string) (ep: Expr) (value: Expr) : 'v option =
        let rec walk (p: Expr) (v: Expr) =
            match p, v with
            | Node { Annotations = annotations }, _ when annotations |> List.contains annot -> Some(cast v)
            | Node { Prim = T_Pair; Args = (Seq args) }, Node { Prim = D_Pair; Args = (Seq values) } ->
                Seq.zip args values
                |> Seq.map (fun (x, y) -> walk x y)
                |> Seq.tryFind (fun e -> e.IsSome)
                |> Option.flatten
            | _ -> None

        walk ep value

type ContractParameters(typeExpression) =

    let entryPoints =
        let rec lookup
            (s: {| Locations: Map<string, EntryPoint>
                   Path: Prim list |})
            (t: Expr)
            : {| Locations: Map<string, EntryPoint>
                 Path: Prim list |} =
            match t with
            | NamedOr (node, name, (left, right)) ->
                let updatedLocations =
                    s.Locations.Add(name, EntryPoint.create name node (s.Path |> List.rev))

                let newState =
                    lookup
                        {| Locations = updatedLocations
                           Path = D_Left :: s.Path |}
                        left

                lookup
                    {| Locations = newState.Locations
                       Path = D_Right :: s.Path |}
                    right
            | Or (_, (left, right)) ->
                let newState =
                    lookup {| s with Path = D_Left :: s.Path |} left

                lookup
                    {| Locations = newState.Locations
                       Path = D_Right :: s.Path |}
                    right
            | NamedNode (node, name, _) ->
                let updatedLocations =
                    s.Locations.Add(name, EntryPoint.create name node (s.Path |> List.rev))

                {| s with
                       Locations = updatedLocations |}
            | _ -> s

        (lookup {| Locations = Map.empty; Path = [] |} (Node typeExpression))
            .Locations

    let instantiate t (values: Arg) =

        let exploreLeftOrRight routes args loop =
            let (left, right) = routes

            try
                let (leftValue, next) = loop left args
                (Node(PrimExpression.Create(D_Left, args = leftValue)), next)
            with _ ->
                let (rightValue, next) = loop right args
                (Node(PrimExpression.Create(D_Right, args = rightValue)), next)

        let instantiatePrim loop prim v =
            match prim, v with
            | { Prim = T_Pair; Args = Seq (args) }, _ ->
                let (next, i) =
                    args
                    |> Seq.fold
                        (fun (v, acc) e ->
                            let (r, next) = loop e v
                            (next, r :: acc))
                        (v, [])

                let i = i |> Seq.toList |> List.rev
                (Node(PrimExpression.Create(D_Pair, args = Seq i)), next)
            | { Prim = T_List
                Args = Node _ as listType },
              List elements ->
                let children =
                    elements
                    |> Seq.map
                        (fun e ->
                            let (exp, _) = loop listType e
                            exp)
                    |> Seq.toList

                (Seq children, v)
            | { Prim = T_Or
                Args = Seq ([ left; _ ]) },
              Either (Left v) ->
                let (arg, _) = loop left v
                (Node(PrimExpression.Create(D_Left, args = arg)), v)
            | { Prim = T_Or
                Args = Seq ([ _; right ]) },
              Either (Right v) ->
                let (arg, _) = loop right v
                (Node(PrimExpression.Create(D_Right, args = arg)), v)
            | { Prim = T_Or
                Args = Seq ([ left; right ]) },
              _ ->
                let (r, _) = exploreLeftOrRight (left, right) v loop
                (r, v)
            | { Prim = T_String }, Value (String s) -> (StringLiteral s, v)
            | { Prim = T_Bytes }, Value (Bytes b) -> (BytesLiteral b, v)
            | { Prim = T_Bytes }, Value (String s) -> (BytesLiteral(Encoder.hexToBytes s), v)
            | { Prim = T_Nat }, Value (Int i) -> (IntLiteral i, v)
            | { Prim = T_Address }, Value (String s) ->
                ((BytesLiteral(
                    s
                    |> TezosAddress.FromStringUnsafe
                    |> TezosAddress.ToBytes
                 )),
                 v)
            | { Prim = T_Address }, Value (Address addr) -> (BytesLiteral(addr |> TezosAddress.ToBytes), v)
            | { Prim = T_Signature }, Value (String s) ->
                (BytesLiteral(s |> Signature.FromString |> Signature.ToBytes), v)
            | { Prim = T_Signature }, Value (Signature s) -> (BytesLiteral(s |> Signature.ToBytes), v)
            | { Prim = T_Key }, Value (String s) -> (BytesLiteral(s |> PublicKey.FromString |> PublicKey.ForgeMicheline), v)
            | { Prim = T_Key }, Value (Key s) -> (BytesLiteral(s |> PublicKey.ForgeMicheline), v)
            | { Prim = T_ChainId }, Value (String s) -> (BytesLiteral(ChainId.toBytes s), v)
            | _, _ -> failwith (sprintf "Bad parameters. \nPrim: %s \nParams:  %s" (prim.ToString()) (v.ToString()))

        let consume loop (expr: PrimExpression) (values: Arg) =
            match Node expr, values with
            | Primitive _, Tuple (head :: tail) ->
                let (i, _) = instantiatePrim loop expr head
                (i, Tuple tail)
            | _, _ -> instantiatePrim loop expr values

        let rec loop (expr: Expr) (args: Arg) =
            match expr, args with
            | NamedNode (prim, name, _), Record record ->
                let field = Arg.Find record name
                let (n, _) = consume loop prim field
                (n, args)
            | ANode (prim, _), _ -> consume loop prim args

            | _ -> failwith (sprintf "Parameter can only contains Node. Found %s" (expr.ToString()))

        let (expr, _) = loop (Node t) values
        expr

    new(michelson) = ContractParameters(Parameters.load michelson)

    member this.Instantiate(args: Arg) = instantiate typeExpression args

    member this.Instantiate(entryPoint, args: Arg) =
        let ep = this.Find(entryPoint)
        let result = instantiate ep.Expression args
        List.foldBack (fun p e -> Node(PrimExpression.Create(p, args = e))) ep.Path result

    member this.Find(annotation) = entryPoints.[annotation]

    member this.Only(annotation) =
        ContractParameters(this.Find(annotation).Expression)

    member this.Extract(annotation: string, value: Expr) =
        match Extract.extract annotation (Node typeExpression) value with
        | Some v -> v
        | None -> failwith "Value not found"
