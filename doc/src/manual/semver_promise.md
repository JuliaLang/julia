# The Julia Language API and SemVer

This pages documents the finer details of how Julia follows the [SemVer 2.0 specification](https://semver.org/).

In particular it lists things that are *not* covered by SemVer and may change without tagging a **major** release.

For brevity, in the following sections the following defintions appply:
 - **API Items**: Functions, Macros, Types, Modules and Constants
 - **Standard Libraries**: Base, Core, as well as the other packages included in a julia install by default, such as Pkg, Statistics, LinearAlgebra etc.
 

The following are excluded:
 - _API Items_ that are not exported
      - For example: `Pkg.installed` is not exported and is removed in Julia v1.4
 - _API Items_ that are marked _experimental_ in their release notes or documentation.
      - For example: the `Base.Threading` module introduced in julia v0.4 was maked experimental until julia v1.3; and thus could have been removed at any time.
 - The type of exception that might be thrown by a particular method or action.
      - For example: `run(`failingcommand`) changed in 1.2 to throw a `ProcessFailedException` rather than an `ErrorException`.
 - Whether or not an exception is thrown at all. I.e. behavour can be improved to make less things errors:
      - For example in julia 1.0 `print(nothing)` errored, but in 1.3 it works.
 - The exact set of exported _API Items_: more things may be exported expanding the API
      - For example julia v1.1 added `eachcol`.

 
Further understand that according to SemVer 2: bugs, such as when a functions behavour disagrees with the documentation, my be removed in any release.
This also includes when a breaking change was mistakenly added, a patch release can revert it.


The following things *are* covered by the SemVer garentee (many other things also, but these might catch you by surprise):
 - 
 - 
 
 
## TODO CLassify these:
  - The serialization format used by `Base.serialize`; except when dealing with stored values that are of types that were allowed to be changed.
  - The stream of random numbers given by the default random number generator for a given seed.
