Nichelson, for .NET Michelson.

So far, Nichelson is an experiment to let a user parse a michelson expression to build a micheline representation in plain F#. 
The goal would be to ease smart contracts invocation. For instance, one could parse a smart contract parameter, and then build and serialize a call, to sign it for the multisig, or call it straigh away.

A full exemple can be found [here](Michelson.Test/EndToEnd.Test.fs)