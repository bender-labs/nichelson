namespace Bender.Michelson.Micheline

type Annotation = string list

type Prim =
    | T_Nat
    | T_Or
    | T_Pair
    | T_String
    | T_Address
    | D_Pair

type PrimExpression =
    { Prim: Prim
      Args: Expr option
      Annotations: Annotation }
    static member Create(p: Prim, ?args: Expr, ?annotation: Annotation) =
        { Prim = p
          Args = args
          Annotations = defaultArg annotation []}

and Expr =
    | Int of int64
    | String of string
    | Bytes of byte array
    | Node of PrimExpression
    | Seq of Expr list
