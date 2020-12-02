namespace Bender.Michelson.Contract

open Bender.Michelson


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
    | Int of int64
    | String of string
    | Address of TezosAddress.T
    | Signature of Signature.T


module Arg =
    
    let Rec v = Record v

    let LeftArg v = v |> Left |> Either

    let RightArg v = v |> Right |> Either 
    
    let IntArg v = v |> Int |> Arg.Value
    let StringArg v = v |> String |> Arg.Value
    
    let Find (r:(string * Arg) list) key = r |> List.tryFind (fun (k, _) -> k = key)

type Values =
    | Named of Map<string, obj>
    | Unnamed of obj list

type Or =
    | Left = 0
    | Right = 1

type EntryPoint =
    { Name: string
      Expression: PrimExpression
      Path: Prim list }

[<RequireQualifiedAccess>]
module private EntryPoint =
    let create name expr path =
        { Name = name
          Expression = expr
          Path = path }

type ContractParameters(typeExpression) =

    let record (p: PrimExpression) = ()

    let entryPoints =
        let rec lookup (s: {| Locations: Map<string, EntryPoint>
                              Path: Prim list |})
                       (t: Expr)
                       : {| Locations: Map<string, EntryPoint>
                            Path: Prim list |} =
            match t with
            | NamedOr (node, (left, right)) ->
                let updatedLocations =
                    s.Locations.Add
                        (node.Annotations.Head, EntryPoint.create node.Annotations.Head node (s.Path |> List.rev))

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
            | NamedNode (node) ->
                let updatedLocations =
                    s.Locations.Add
                        (node.Annotations.Head, EntryPoint.create node.Annotations.Head node (s.Path |> List.rev))

                {| s with
                       Locations = updatedLocations |}
            | _ -> s

        (lookup {| Locations = Map.empty; Path = [] |} (Node typeExpression))
            .Locations

    
    let instantiateWithArg t (values:Arg) =
        
        let instantiate (prim) (v: Arg) =
            match prim, v with
            | T_String, Value (String s) -> StringLiteral s
            | T_Nat, Value (Int i) -> IntLiteral i
            | T_Address, Value (String s) ->
                BytesLiteral
                    (s
                     |> TezosAddress.FromString
                     |> TezosAddress.ToBytes)
            | T_Address, Value (Address addr) -> BytesLiteral(addr |> TezosAddress.ToBytes)
            | T_Signature, Value (String s) -> BytesLiteral(s |> Signature.FromString |> Signature.ToBytes)
            | T_Signature, Value (Signature s) -> BytesLiteral(s |> Signature.ToBytes)
            | t, _ as arg ->
                failwith (sprintf "Bad parameters. %s does not match with %s" (t.ToString()) (arg.ToString()))

        let consume (expr: PrimExpression) (values: Arg) =
            match expr.Annotations, values with
            | [ name ], Record (m) ->
                let (_, value) as t =
                    match Arg.Find m name with
                    | Some t -> t
                    | None -> failwith (sprintf "Field not found %s" (name))
                
                (instantiate expr.Prim value, values)
            | _, Tuple (head :: tail) -> (instantiate expr.Prim head, (Tuple tail))
            | _, Value v -> (instantiate expr.Prim values, Tuple [])
            | _ -> failwith "Bad arguments"


        let rec loop (expr: Expr) (v:Arg) =
            match expr, v with
            | AnonymousPair(_, (left, right)), _ | Pair (_, (left, right)), Tuple _ ->
                let (leftValue, next) = loop left v
                let (rightValue, next) = loop right next
                let p =
                    PrimExpression.Create(D_Pair, args = Seq [ leftValue; rightValue ])
                (Node p, next)
            | Pair (node, (left, right)), Record record ->
                let sub =
                    match Arg.Find record node.Annotations.Head with
                    | Some (_, sub)->sub
                    | None -> v
                let (leftValue, next) = loop left sub
                let (rightValue, next) = loop right next

                let p =
                    PrimExpression.Create(D_Pair, args = Seq [ leftValue; rightValue ])

                (Node p, next)
            | Or(_, (left, right)), Either e ->
                match e with
                | Left v ->
                    let (arg, _) = loop left v 
                    (Node (PrimExpression.Create(D_Left, args = arg)), v)
                | Right v ->
                    let (arg, _) = loop right v 
                    (Node (PrimExpression.Create(D_Right, args = arg)), v)
            | Primitive (n), _ -> consume n v
            | _ -> failwith (sprintf "Bad parameter type. %s" (expr.ToString()))

        let (expr, _) = loop (Node t) values
        expr

    new(michelson) = ContractParameters(Parameters.fromMichelson michelson)

    member this.Instantiate(args: Arg) = instantiateWithArg typeExpression args
    
    member this.Instantiate(entryPoint, args: Arg) =
        let ep = this.Find(entryPoint)
        let result = instantiateWithArg ep.Expression args
        List.foldBack (fun p e -> Node(PrimExpression.Create(p, args = e))) ep.Path result

    member this.Find(annotation) = entryPoints.[annotation]

    member this.Only(annotation) =
        ContractParameters(this.Find(annotation).Expression)
