namespace Bender.Michelson.Contract

open System
open Bender.Michelson.Micheline
open Bender.Michelson.Micheline.Expr

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

type Path = Path of string

module Path =
    let value (Path p) = p

type Values =
    | Map of Map<string, obj>
    | List of obj list

type Find = PrimExpression -> Path -> PrimExpression

type Instantiate = PrimExpression -> Path option -> Values -> Expr

type EntryPoint =
    { Name: string
      Expression: PrimExpression
      Path: Prim list }

[<RequireQualifiedAccess>]
module EntryPoint =
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


    new(michelson) = ContractParameters(Parameters.fromMichelson michelson)

    member this.Find(annotation) = entryPoints.[annotation]

    member this.Only(annotation) =
        ContractParameters(this.Find(annotation).Expression)

    member this.Instantiate(entryPoint, values: Values) =
        let ep = this.Find(entryPoint)

        let instantiate prim (v: obj) =
            match prim, v with
            | T_String, (:? string as s) -> String s
            | T_Nat, (:? Int64 as i) -> Int i
            | T_Address, (:? string as s) -> String s
            | _, _ -> failwith "Bad parameters"

        let consume (expr: PrimExpression) (values: Values) =
            match expr.Annotations, values with
            | [ name ], Map (m) -> (instantiate expr.Prim m.[name], Map(m.Remove(name)))
            | _, List (head :: tail) -> (instantiate expr.Prim head, List tail)
            | _ -> failwith "Bad arguments"

        let rec loop (expr: Expr) v =
            match expr with
            | Pair (_, (left, right)) ->
                let (leftValue, next) = loop left v
                let (rightValue, next) = loop right next

                let p =
                    PrimExpression.Create(D_Pair, args = Seq [ leftValue; rightValue ])

                (Node p, next)
            | Primitive (n) -> consume n v
            | _ -> failwith "Bad parameter type"

        let (expr, _) = loop (Node ep.Expression) values
        List.foldBack (fun p e -> Node(PrimExpression.Create(p, args = e))) ep.Path expr
