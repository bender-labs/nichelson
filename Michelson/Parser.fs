[<AutoOpen>]
module Bender.Michelson.Micheline.Parser

open Bender.Michelson.Micheline
open FParsec

type private UserState = unit

let private ws = spaces

let private openParen = pstring "("
let private closingParen = pstring ")"
    
let private annotations: Parser<Annotation, UserState> =
    let start = anyOf "%!:$&?"

    let value =
        many1Satisfy (fun c -> isLetter (c) || isAnyOf "_.%@" c)

    many
        (spaces
         >>? pipe2 start value (fun a v -> (string a) + v))

let private nodeWithArgs str p (parser: Parser<Expr, UserState>) =
    let v = skipString str

    v
    >>. (pipe2 annotations parser (fun annot args ->
             { Prim = p
               Args = Some args
               Annotations = annot }))

let private nodeWithTwoArgs sub str p =
    nodeWithArgs str p (pipe2 sub sub (fun el1 el2 -> Seq [ el1; el2 ]))


[<RequireQualifiedAccess>]
module Parameters =
    let (private prims: Parser<PrimExpression, UserState>), (private primsR: Parser<PrimExpression, UserState> ref) =
        createParserForwardedToRef ()

    let private noArg str p =
        let v = skipString str

        v >>. annotations
        |>> (fun a ->
            { Prim = p
              Args = None
              Annotations = a })

    let private tNat = noArg "nat" T_Nat
    let private tUnit = noArg "unit" T_Nat

    let private tAddress = noArg "address" T_Address

    let private tString = noArg "string" T_String

    let paramNodeWithTwoArgs = nodeWithTwoArgs (prims |>> (Node))

    let private tPair = paramNodeWithTwoArgs "pair" T_Pair

    let private tOr = paramNodeWithTwoArgs "or" T_Or

    let private primChoice =
        choice [ tPair
                 tOr
                 tNat
                 tAddress
                 tString
                 tUnit ]

    let private prim =
        spaces
        >>. between openParen closingParen primChoice

    do primsR := prim

    let parse = prims .>> eof

[<RequireQualifiedAccess>]
module Expression =
    let (private values: Parser<Expr, UserState>), (private valuesR: Parser<Expr, UserState> ref) =
        createParserForwardedToRef ()

    let private intLiteral = pint64 |>> (Expr.Int)
    
    let private stringLiteral =
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"')
        between (pstring @"""") (pstring @"""") normalCharSnippet |>> (String)

    let private exprWithTwoArgs = nodeWithTwoArgs values
    
    let pairD = exprWithTwoArgs "Pair" Prim.D_Pair |>> Node
    
    let leftD = exprWithTwoArgs "Left" Prim.D_Left |>> Node
    let rightD = exprWithTwoArgs "Right" Prim.D_Right |>> Node
    
    let private constantChoices =
        choice [
            pairD
            leftD
            rightD
        ]

    let private literalChoices =
        choice [
            intLiteral
            stringLiteral
        ]
    let private expr =
        spaces
        >>? between openParen closingParen constantChoices
    
    let private lit =
        spaces
        >>? literalChoices

    do valuesR := expr <|> lit

    let parse = values .>> eof