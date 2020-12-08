Nichelson, for .NET Michelson.

So far, Nichelson is an experiment to let a user parse a michelson expression to build a micheline representation in plain F#. 
The goal would be to ease smart contracts invocation. For instance, one could parse a smart contract parameter, and then build and serialize a call, to sign it for the multisig, or call it straigh away.

A full exemple can be found [here](Nichelson.Test/EndToEnd.Test.fs)


## Configure nuget

For now, Nichelson is published under github packages. 

Follow [this instructions](https://docs.github.com/en/free-pro-team@latest/packages/using-github-packages-with-your-projects-ecosystem/configuring-dotnet-cli-for-use-with-github-packages#authenticating-to-github-packages) to add bender-labs as a nuget source.


## Publishing a version

`dotnet pack -c Release --include-symbols -o nugets -p:PackageVersion="0.0.4"`

`dotnet nuget push nugets/Nichelson.{version}.nupkg --source <the source you configured>`