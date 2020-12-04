namespace Bender.Michelson

open Netezos.Forge.Utils

module ChainId =
    let toBytes =
        Base58.Parse >> Array.skip 3 
