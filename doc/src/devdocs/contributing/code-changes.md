### Contributing to core functionality or base libraries

*By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

The Julia community uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests (PR).

Issues and pull requests should have self explanatory titles such that they can be understood from the list of PRs and Issues.
i.e. `Add {feature}` and `Fix {bug}` are good, `Fix #12345. Corrects the bug.` is bad.

You can make pull requests for incomplete features to get code review. The convention is to open these as draft PRs and prefix
the pull request title with "WIP:" for Work In Progress, or "RFC:" for Request for Comments when work is completed and ready
for merging. This will prevent accidental merging of work that is in progress.

Note: These instructions are for adding to or improving functionality in the base library. Before getting started, it can be helpful to discuss the proposed changes or additions on the [Julia Discourse forum](https://discourse.julialang.org) or in a GitHub issue---it's possible your proposed change belongs in a package rather than the core language. Also, keep in mind that changing stuff in the base can potentially break a lot of things. Finally, because of the time required to build Julia, note that it's usually faster to develop your code in stand-alone files, get it working, and then migrate it into the base libraries.

Add new code to Julia's base libraries as follows (this is the "basic" approach; see a more efficient approach in the next section):

 1. Edit the appropriate file in the `base/` directory, or add new files if necessary. Create tests for your functionality and add them to files in the `test/` directory. If you're editing C or Scheme code, most likely it lives in `src/` or one of its subdirectories, although some aspects of Julia's REPL initialization live in `cli/`.

 2. Add any new files to `sysimg.jl` in order to build them into the Julia system image.

 3. Add any necessary export symbols in `exports.jl`.

 4. Include your tests in `test/Makefile` and `test/choosetests.jl`.

Build as usual, and do `make clean testall` to test your contribution. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent (be careful – this command will delete any files lying around that aren't checked into git).

#### Running specific tests

There are `make` targets for running specific tests:

    make test-bitarray

You can also use the `runtests.jl` script, e.g. to run `test/bitarray.jl` and `test/math.jl`:

    ./usr/bin/julia test/runtests.jl bitarray math

#### Modifying base more efficiently with Revise.jl

[Revise](https://github.com/timholy/Revise.jl) is a package that
tracks changes in source files and automatically updates function
definitions in your running Julia session. Using it, you can make
extensive changes to Base without needing to rebuild in order to test
your changes.

Here is the standard procedure:

1. If you are planning changes to any types or macros, make those
   changes and build julia using `make`. (This is
   necessary because `Revise` cannot handle changes to type
   definitions or macros.) Unless it's
   required to get Julia to build, you do not have to add any
   functionality based on the new types, just the type definitions
   themselves.

2. Start a Julia REPL session. Then issue the following commands:

```julia
using Revise    # if you aren't launching it in your `.julia/config/startup.jl`
Revise.track(Base)
```

3. Edit files in `base/`, save your edits, and test the
   functionality.

If you need to restart your Julia session, just start at step 2 above.
`Revise.track(Base)` will note any changes from when Julia was last
built and incorporate them automatically. You only need to rebuild
Julia if you made code-changes that Revise cannot handle.

For convenience, there are also `test-revise-*` targets for every [`test-*`
target](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#running-specific-tests) that use Revise to load any modifications to Base into the current
system image before running the corresponding test. This can be useful as a shortcut
on the command line (since tests aren't always designed to be run outside the
runtest harness).

### Contributing to the standard library

The standard library (stdlib) packages are baked into the Julia system image.
When running the ordinary test workflow on the stdlib packages, the system image
version overrides the version you are developing.
To test stdlib packages, you can do the following steps:

1. Edit the UUID field of the `Project.toml` in the stdlib package
2. Change the current directory to the directory of the stdlib you are developing
3. Start julia with `julia --project=.`
4. You can now test the package by running `pkg> test` in Pkg mode.

Because you changed the UUID, the package manager treats the stdlib package as
different from the one in the system image, and the system image version will
not override the package.

Be sure to change the UUID value back before making the pull request.

#### News-worthy changes

For new functionality and other substantial changes, add a brief summary to `NEWS.md`. The news item should cross reference the pull request (PR) parenthetically, in the form `([#pr])`. To add the PR reference number, first create the PR, then push an additional commit updating `NEWS.md` with the PR reference number. We periodically run `./julia doc/NEWS-update.jl` from the julia directory to update the cross-reference links, but this should not be done in a typical PR in order to avoid conflicting commits.

#### Annotations for new features, deprecations and behavior changes

API additions and deprecations, and minor behavior changes are allowed in minor version releases.
For documented features that are part of the public API, a compatibility note should be added into
the manual or the docstring. It should state the Julia minor version that changed the behavior
and have a brief message describing the change.

At the moment, this should always be done with the following `compat` admonition
(so that it would be possible to programmatically find the annotations in the future):

  ```
  !!! compat "Julia 1.X"
      This method was added in Julia 1.X.
  ```
