namespace Bender.Michelson.Contract

open Bender.Michelson
open System

(*
    (pair nat nat)

    -> [1 ; 2]

    -> (Pair 1 2)

    --

    (list nat)
    -> [[1; 2; 3; 5]]


    (or (pair nat nat) nat)
    -> (Left (pair nat nat))
    -> 1 ; 2
    [left 1 2]

    parameters.extract(None).instanciate("mint", ["token_id" "mon_token" "amount" 10]);

    let exp = instanciate (params, ["mint"], [ "token_id" "mon_token" "amount" 10 ])
    // instanciate (params, [Left Right], [10])
    let packed = pack exp
    let payload = sign packed
*)

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

    let entryPoints =
        let rec lookup (s: {| Locations: Map<string, EntryPoint>
                              Path: Prim list |})
                       (t: Expr)
                       : {| Locations: Map<string, EntryPoint>
                            Path: Prim list |} =
            match t with
            | AnnotatedOr (node, (left, right)) ->
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
            | AnnotatedNode (node) ->
                let updatedLocations =
                    s.Locations.Add
                        (node.Annotations.Head, EntryPoint.create node.Annotations.Head node (s.Path |> List.rev))

                {| s with
                       Locations = updatedLocations |}
            | _ -> s

        (lookup {| Locations = Map.empty; Path = [] |} (Node typeExpression))
            .Locations

    let instantiate t values =
        let instantiate prim (v: obj) =
            match prim, v with
            | T_String, (:? string as s) -> Expr.String s
            | T_Nat, (:? Int64 as i) -> Int i
            | T_Address, (:? string as s) ->
                Bytes
                    (s
                     |> TezosAddress.FromString
                     |> TezosAddress.ToBytes)
            | T_Address, (:? TezosAddress.T as addr) -> Bytes(addr |> TezosAddress.ToBytes)
            | T_Signature, (:? string as s) -> Bytes(s |> Signature.FromString |> Signature.ToBytes)
            | T_Signature, (:? Signature.T as s) -> Bytes(s |> Signature.ToBytes)
            | t, _ as arg ->
                failwith (sprintf "Bad parameters. %s does not match with %s" (t.ToString()) (arg.ToString()))

        let consume (expr: PrimExpression) (values: Values) =
            match expr.Annotations, values with
            | [ name ], Named (m) -> (instantiate expr.Prim m.[name], Named(m.Remove(name)))
            | _, Unnamed (head :: tail) -> (instantiate expr.Prim head, Unnamed tail)
            | _ -> failwith "Bad arguments"

        
        let rec loop (expr: Expr) v =
            match expr with
            | Pair (_, (left, right)) ->
                let (leftValue, next) = loop left v
                let (rightValue, next) = loop right next

                let p =
                    PrimExpression.Create(D_Pair, args = Seq [ leftValue; rightValue ])

                (Node p, next)
            | Or(_, (left, right)) ->
                match v with
                | Named(_) -> failwith "Instantiating or by named arguments not supported yet"
                | Unnamed(head::tail) ->
                    match head with
                        | :? Or as d ->
                            if d = Or.Left
                            then
                                let (leftValue, next) = loop left (Unnamed tail) 
                                let p = PrimExpression.Create(D_Left, args = leftValue)
                                (Node p, next)
                            else
                                let (value, next) = loop right (Unnamed tail) 
                                let p = PrimExpression.Create(D_Right, args = value)
                                (Node p, next)
                        | _ -> failwith "Bad parameter for Or"
                 | _ -> failwith "Missing parameters for or"
                            
            | Primitive (n) -> consume n v
            | _ -> failwith (sprintf "Bad parameter type. %s" (expr.ToString()))

        let (expr, _) = loop (Node t) values
        expr

    new(michelson) = ContractParameters(Parameters.fromMichelson michelson)

    member this.Find(annotation) = entryPoints.[annotation]

    member this.Only(annotation) =
        ContractParameters(this.Find(annotation).Expression)

    member this.Instantiate(entryPoint, values: Values) =
        let ep = this.Find(entryPoint)
        let result = instantiate ep.Expression values
        List.foldBack (fun p e -> Node(PrimExpression.Create(p, args = e))) ep.Path result

    member this.Instantiate(values: Values) = instantiate typeExpression values
