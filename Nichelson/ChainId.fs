namespace Nichelson

open Netezos.Forge.Utils

module ChainId =
    let toBytes =
        Base58.Parse >> Array.skip 3 
