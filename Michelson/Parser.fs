[<AutoOpen>]
module Bender.Michelson.Parser

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

let private noArg str p =
    let v = skipString str

    v >>. annotations
    |>> (fun a ->
        { Prim = p
          Args = Seq []
          Annotations = a })

let private nodeWithArgs str p (parser: Parser<Expr, UserState>) =
    let v = skipString str

    v
    >>. (pipe2 annotations parser (fun annot args ->
             { Prim = p
               Args = args
               Annotations = annot }))

let private nodeWithTwoArgs sub str p =
    nodeWithArgs str p (pipe2 sub sub (fun el1 el2 -> Seq [ el1; el2 ]))
    
let private nodeWithOneArg sub str p =
    nodeWithArgs str p sub

[<RequireQualifiedAccess>]
module Parameters =
    let (private prims: Parser<PrimExpression, UserState>), (private primsR: Parser<PrimExpression, UserState> ref) =
        createParserForwardedToRef ()

    let private tNat = noArg "nat" T_Nat
    let private tUnit = noArg "unit" T_Nat

    let private tAddress = noArg "address" T_Address

    let private tString = noArg "string" T_String

    let paramNodeWithTwoArgs = nodeWithTwoArgs (prims |>> (Node))

    let private tPair = paramNodeWithTwoArgs "pair" T_Pair

    let private tOr = paramNodeWithTwoArgs "or" T_Or

    let private prim =
        choice [ tPair
                 tOr
                 tNat
                 tAddress
                 tString
                 tUnit ]

    let private primBetweenParen = between openParen closingParen prim

    do primsR := spaces >>? (primBetweenParen <|> prim)

    let parse = prims .>> eof
    
    let fromMichelson michelson =
        let p = run parse michelson

        match p with
        | Success (v, _, _) -> v
        | Failure (m, _, _) -> failwith m

[<RequireQualifiedAccess>]
module Expression =
    let (private values: Parser<Expr, UserState>), (private valuesR: Parser<Expr, UserState> ref) =
        createParserForwardedToRef ()

    let private intLiteral = pint64 |>> (Int)

    let private stringLiteral =
        let normalCharSnippet = manySatisfy (fun c -> c <> '"')

        between (pstring @"""") (pstring @"""") normalCharSnippet
        |>> (String)

    let private exprWithTwoArgs = nodeWithTwoArgs values
    let private exprWithOneArg = nodeWithOneArg values

    let private pairD =
        exprWithTwoArgs "Pair" Prim.D_Pair |>> Node

    let leftD =
        exprWithOneArg "Left" Prim.D_Left |>> Node

    let rightD =
        exprWithOneArg "Right" Prim.D_Right |>> Node

    let private instruction =
        choice [ intLiteral
                 stringLiteral
                 pairD
                 leftD
                 rightD ]

    let private instructionBetweenParen =
        spaces
        >>? between openParen closingParen instruction

    do valuesR := spaces >>? (instruction <|> instructionBetweenParen)

    let parse = values .>> eof
    
    let fromMichelson michelson =
        let p = run parse michelson

        match p with
        | Success (v, _, _) -> v
        | Failure (m, _, _) -> failwith m
