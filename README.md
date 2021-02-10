# Nichelson, for .NET Michelson.

So far, Nichelson is an experiment to let a user parse a michelson expression to build a micheline representation in plain F#. 
The goal would be to ease smart contracts invocation. For instance, one could parse a smart contract parameter, and then build and serialize a call, to sign it for the multisig, or call it straigh away.

A full exemple can be found [here](Nichelson.Test/EndToEnd.Test.fs)

## Builds

GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/bender-labs/nichelson/workflows/Build%20master/badge.svg)](https://github.com/bender-labs/nichelson/actions?query=branch%3Amaster) |
[![Build History](https://buildstats.info/github/chart/bender-labs/nichelson)](https://github.com/benderlabs/nichelson/actions?query=branch%3Amaster) |


## NuGet 

Package | Stable | Prerelease
--- | --- | ---
Nichelson | [![NuGet Badge](https://buildstats.info/nuget/nichelson)](https://www.nuget.org/packages/nichelson/) | [![NuGet Badge](https://buildstats.info/nuget/nichelson?includePreReleases=true)](https://www.nuget.org/packages/nichelson/)

---

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands.  If not set, it will default to Release.
  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`
- `GITHUB_TOKEN` will be used to upload release notes and Nuget packages to GitHub.
  - Be sure to set this before releasing
- `DISABLE_COVERAGE` Will disable running code coverage metrics.  AltCover can have [severe performance degradation](https://github.com/SteveGilham/altcover/issues/57) so it's worth disabling when looking to do a quicker feedback loop.
  - `DISABLE_COVERAGE=1 ./build.sh`


---

### Building


```sh
> build.cmd <optional buildtarget> // on windows
$ ./build.sh  <optional buildtarget>// on unix
```


---

### Build Targets

- `Clean` - Cleans artifact and temp directories.
- `DotnetRestore` - Runs [dotnet restore](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- [`DotnetBuild`](#Building) - Runs [dotnet build](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `DotnetTest` - Runs [dotnet test](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator](https://github.com/danielpalme/ReportGenerator).
- `WatchTests` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) with the test projects. Useful for rapid feedback loops.
- `GenerateAssemblyInfo` - Generates [AssemblyInfo](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8) for libraries.
- `DotnetPack` - Runs [dotnet pack](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-pack). This includes running [Source Link](https://github.com/dotnet/sourcelink).
- `SourceLinkTest` - Runs a Source Link test tool to verify Source Links were properly generated.
- `PublishToNuGet` - Publishes the NuGet packages generated in `DotnetPack` to NuGet via [paket push](https://fsprojects.github.io/Paket/paket-push.html).
- `GitRelease` - Creates a commit message with the [Release Notes](https://fake.build/apidocs/v5/fake-core-releasenotes.html) and a git tag via the version in the `Release Notes`.
- `GitHubRelease` - Publishes a [GitHub Release](https://help.github.com/en/articles/creating-releases) with the Release Notes and any NuGet packages.
- `FormatCode` - Runs [Fantomas](https://github.com/fsprojects/fantomas) on the solution file.
- [`Release`](#Releasing) - Task that runs all release type tasks such as `PublishToNuGet`, `GitRelease`, `ReleaseDocs`, and `GitHubRelease`. Make sure to read [Releasing](#Releasing) to setup your environment correctly for releases.
---


### Releasing

- [Create your NuGeT API key](https://docs.microsoft.com/en-us/nuget/nuget-org/publish-a-package#create-api-keys)
    - [Add your NuGet API key to paket](https://fsprojects.github.io/Paket/paket-config.html#Adding-a-NuGet-API-key)

    ```sh
    paket config add-token "https://www.nuget.org" <api_key>
    ```

    - or set the environment variable `NUGET_TOKEN` to your key


- [Create a GitHub OAuth Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  - You can then set the environment variable `GITHUB_TOKEN` to upload release notes and artifacts to github
  - Otherwise it will fallback to username/password

- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog](https://keepachangelog.com/en/1.1.0/) format.

NOTE: Its highly recommend to add a link to the Pull Request next to the release note that it affects. The reason for this is when the `RELEASE` target is run, it will add these new notes into the body of git commit. GitHub will notice the links and will update the Pull Request with what commit referenced it saying ["added a commit that referenced this pull request"](https://github.com/TheAngryByrd/MiniScaffold/pull/179#ref-commit-837ad59). Since the build script automates the commit message, it will say "Bump Version to x.y.z". The benefit of this is when users goto a Pull Request, it will be clear when and which version those code changes released. Also when reading the `CHANGELOG`, if someone is curious about how or why those changes were made, they can easily discover the work and discussions.

Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.

```markdown
## [Unreleased]

### Added
- Does cool stuff!

### Fixed
- Fixes that silly oversight

## [0.1.0] - 2017-03-17
First release

### Added
- This release already has lots of features

[Unreleased]: https://github.com/benderlabs/nichelson.git/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/benderlabs/nichelson.git/releases/tag/v0.1.0
```

- You can then use the `Release` target, specifying the version number either in the `RELEASE_VERSION` environment
  variable, or else as a parameter after the target name.  This will:
  - update `CHANGELOG.md`, moving changes from the `Unreleased` section into a new `0.2.0` section
    - if there were any prerelease versions of 0.2.0 in the changelog, it will also collect their changes into the final 0.2.0 entry
  - make a commit bumping the version:  `Bump version to 0.2.0` and adds the new changelog section to the commit's body
  - publish the package to NuGet
  - push a git tag
  - create a GitHub release for that git tag

macOS/Linux Parameter:

```sh
./build.sh Release 0.2.0
```

macOS/Linux Environment Variable:

```sh
RELEASE_VERSION=0.2.0 ./build.sh Release
```

