[<AutoOpen>]
module Nichelson.Parser

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

let private nodeWithOneArg sub str p = nodeWithArgs str p sub

[<RequireQualifiedAccess>]
module Parameters =
    let (private prims: Parser<PrimExpression, UserState>), (private primsR: Parser<PrimExpression, UserState> ref) =
        createParserForwardedToRef ()

    let private tNat = noArg "nat" T_Nat
    let private tUnit = noArg "unit" T_Nat

    let private tAddress = noArg "address" T_Address

    let private tString = noArg "string" T_String
    
    let private tSignature = noArg "signature" T_Signature
    
    let private tChainId = noArg "chain_id" T_ChainId

    let private tList =
        nodeWithOneArg (prims |>> Node) "list" T_List

    let paramNodeWithTwoArgs = nodeWithTwoArgs (prims |>> Node)

    let private tPair = paramNodeWithTwoArgs "pair" T_Pair

    let private tOr = paramNodeWithTwoArgs "or" T_Or

    let private prim =
        choice [ tPair
                 tList
                 tOr
                 tNat
                 tAddress
                 tString
                 tSignature
                 tChainId
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

    let private intLiteral = pint64 |>> (IntLiteral)

    let private stringLiteral =
        let normalCharSnippet = manySatisfy ((<>) '"')

        between (pstring @"""") (pstring @"""") normalCharSnippet
        |>> (StringLiteral)

    let private hexLiteral =
        let start = pstring "0x"
        let hex = manySatisfy isHex
        pipe2 start hex (fun s n -> s + n |> Encoder.hexToBytes |> BytesLiteral)

    let private listD =
        let sep = pstring ";"
        let elems = sepBy values sep 
        between (pstring "{") (pstring "}") elems
        |>> (Seq)
    
    let private exprWithTwoArgs = nodeWithTwoArgs values
    let private exprWithOneArg = nodeWithOneArg values

    let private pairD =
        exprWithTwoArgs "Pair" Prim.D_Pair |>> Node

    let leftD =
        exprWithOneArg "Left" Prim.D_Left |>> Node

    let rightD =
        exprWithOneArg "Right" Prim.D_Right |>> Node

    let private instruction =
        choice [ hexLiteral
                 intLiteral
                 stringLiteral
                 pairD
                 leftD
                 rightD
                 listD ]

    let private instructionBetweenParen =
        spaces
        >>? between openParen closingParen instruction

    do valuesR
       := spaces
          >>? (instruction <|> instructionBetweenParen)

    let parse = values .>> eof

    let fromMichelson michelson =
        let p = run parse michelson

        match p with
        | Success (v, _, _) -> v
        | Failure (m, _, _) -> failwith m