# Notes for Julia Contributors

Hi! If you are new to the Julia community: welcome, and thanks for trying Julia. Please be sure to respect our [community standards](https://julialang.org/community/standards) in all interactions.

If you are already familiar with Julia itself, this blog post by Katharine Hyatt on [Making your first Julia pull request](https://kshyatt.github.io/post/firstjuliapr/) is a great way to get started.

## Learning Julia

[The learning page](https://julialang.org/learning) has a great list of resources for new and experienced users alike. [This tutorial video](https://www.youtube.com/watch?v=vWkgEddb4-A) is one recommended starting point, as is the "[Invitation to Julia](https://www.youtube.com/watch?v=gQ1y5NUD_RI)" workshop video from JuliaCon 2015  ([slide materials here](https://github.com/dpsanders/invitation_to_julia)). The [Julia documentation](https://docs.julialang.org) covers the language and core library features, and is searchable.

## Before filing an issue

- Reporting a potential bug? Please read the "[How to file a bug report](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#how-to-file-a-bug-report)" section to make sure that all necessary information is included.

- Contributing code? Be sure to review the [contributor checklist](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#contributor-checklist) for helpful tips on the tools we use to build Julia.

- Library feature requests are generally not accepted on this issue tracker. New libraries should be developed as [packages](https://julialang.github.io/Pkg.jl/v1/creating-packages/). Discuss ideas for libraries at the [Julia Discourse forum](https://discourse.julialang.org). Doing so will often lead to pointers to existing projects and bring together collaborators with common interests.

## Contributor Checklist

* Create a [GitHub account](https://github.com/signup/free).

* [Fork Julia](https://github.com/JuliaLang/julia/fork).

* Build the software and libraries (the first time takes a while, but it's fast after that). Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.

* Keep Julia current. Julia is a fast-moving target, and many details of the language are still settling out. Keep the repository up-to-date and rebase work-in-progress frequently to make merges simpler.

* Learn to use [git](https://git-scm.com), the version control system used by GitHub and the Julia project. Try a tutorial such as the one [provided by GitHub](https://try.GitHub.io/levels/1/challenges/1).

* Review discussions on the [Julia Discourse forum](https://discourse.julialang.org).

* For more detailed tips, read the [submission guide](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#submitting-contributions) below.

* Relax and have fun!

## How to file a bug report

A useful bug report filed as a GitHub issue provides information about how to reproduce the error.

1. Before opening a new [GitHub issue](https://github.com/JuliaLang/julia/issues):
  - Try searching the existing issues or the [Julia Discourse forum](https://discourse.julialang.org) to see if someone else has already noticed the same problem.
  - Try some simple debugging techniques to help isolate the problem.
    - Try running the code with the debug build of Julia with `make debug`, which produces the `usr/bin/julia-debug`.
    - Consider running `julia-debug` with a debugger such as `gdb` or `lldb`. Obtaining even a simple [backtrace](http://www.unknownroad.com/rtfm/gdbtut/gdbsegfault.html) is very useful.
    - If Julia segfaults, try following [these debugging tips](https://docs.julialang.org/en/v1/devdocs/backtraces/) to help track down the specific origin of the bug.

2. If the problem is caused by a Julia package rather than core Julia, file a bug report with the relevant package author rather than here.

3. When filing a bug report, provide where possible:
  - The full error message, including the backtrace.
  - A minimal working example, i.e. the smallest chunk of code that triggers the error. Ideally, this should be code that can be pasted into a REPL or run from a source file. If the code is larger than (say) 50 lines, consider putting it in a [gist](https://gist.github.com).
  - The version of Julia as provided by the `versioninfo()` command. Occasionally, the longer output produced by `versioninfo(verbose = true)` may be useful also, especially if the issue is related to a specific package.

4. When pasting code blocks or output, put triple backquotes (\`\`\`) around the text so GitHub will format it nicely. Code statements should be surrounded by single backquotes (\`). Be aware that the `@` sign tags users on GitHub, so references to macros should always be in single backquotes. See [GitHub's guide on Markdown](https://guides.github.com/features/mastering-markdown) for more formatting tricks.

## Submitting contributions

### Contributing a Julia package

Julia has a built-in [package manager](https://julialang.github.io/Pkg.jl/v1/) based on `git`. A number of [packages](https://pkg.julialang.org) across many domains are already available for Julia. Developers are encouraged to provide their libraries as a Julia package. The manual provides instructions on [creating Julia packages](https://julialang.github.io/Pkg.jl/v1/creating-packages/).

For developers who need to wrap C libraries so that they can be called from Julia, the [Clang.jl](https://github.com/ihnorton/Clang.jl) package can help generate the wrappers automatically from the C header files.

### Semantic Versioning and Package Compatibility Across Releases

When tagging a new release of your package you should follow
[Semantic Versioning (SemVer)](https://semver.org/). Given a version number
of the form MAJOR.MINOR.PATCH with MAJOR >= 1, increment the:

* MAJOR version when you make incompatible API changes,
* MINOR version when you add functionality in a backwards compatible manner, and
* PATCH version when you make backwards compatible bug fixes.

For packages in an early stage with version numbers 0.MINOR.PATCH, for example
0.6.2, MINOR takes over the role of MAJOR and should therefore only be increased
when introducing incompatible API changes.

Examples:

* 0.4.2 -> 0.4.3 (non-breaking)
* 0.4.2 -> 0.5.0 (breaking)
* 1.2.3 -> 1.3.0 (non-breaking)
* 1.3.0 -> 1.3.1 (non breaking)
* 1.9.2 -> 2.0.0 (breaking)

Semantic Versioning is a great way to indicate to users of your package whether
they should expect API breaking changes when switching to a new version.
Note that Julia itself respects Semantic Versioning as well. This means that
your package will be compatible with all newer Julia releases with the same
MAJOR version.

However, sometimes you might want to maintain compatibility across breaking
Julia releases or use features from a certain MINOR version in a different,
older MINOR version of Julia. To that end, use
[`Compat.jl`](https://github.com/JuliaLang/Compat.jl). For more information see
[this guide](https://github.com/JuliaLang/Compat.jl/#tagging-the-correct-minimum-version-of-compat).

### Writing tests

There are never enough tests. Track [code coverage at Coveralls](https://coveralls.io/r/JuliaLang/julia), and help improve it.

1. Go visit https://coveralls.io/r/JuliaLang/julia.

2. Browse through the source files and find some untested functionality (highlighted in red) that you think you might be able to write a test for.

3. Write a test that exercises this functionality---you can add your test to one of the existing files, or start a new one, whichever seems most appropriate to you. If you're adding a new test file, make sure you include it in the list of tests in `test/choosetests.jl`. https://docs.julialang.org/en/v1/stdlib/Test/ may be helpful in explaining how the testing infrastructure works.

4. Run `make test-all` to rebuild Julia and run your new test(s). If you had to fix a bug or add functionality in `base`, this will ensure that your test passes and that you have not introduced extraneous whitespace.

5. Submit the test as a pull request (PR).

* Code for the buildbot configuration is maintained at: https://github.com/staticfloat/julia-buildbot
* You can see the current buildbot setup at: https://build.julialang.org/builders
* [Issue 9493](https://github.com/JuliaLang/julia/issues/9493) and [issue 11885](https://github.com/JuliaLang/julia/issues/11885) have more detailed discussion on code coverage.

Coveralls shows functionality that still needs "proof of concept" tests. These are important, as are tests for tricky edge cases, such as converting between integer types when the number to convert is near the maximum of the range of one of the integer types. Even if a function already has some coverage on Coveralls, it may still benefit from tests for edge cases.

### Improving documentation

*By contributing documentation to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

Julia's documentation source files are stored in the `doc/` directory and all docstrings are found in `base/`. Like everything else these can be modified using `git`. Documentation is built with [Documenter.jl](https://github.com/JuliaDocs/Documenter.jl), which uses Markdown syntax. The HTML documentation can be built locally by running

```
make docs
```

from Julia's root directory. This will rebuild the Julia system image, then install or update the package dependencies required to build the documentation, and finally build the HTML documentation and place the resulting files in `doc/_build/html/`.

> **Note**
>
> When making changes to any of Julia's documentation it is recommended that you run `make docs` to check the your changes are valid and do not produce any errors before opening a pull request.

Below are outlined the three most common types of documentation changes and the steps required to perform them. Please note that the following instructions do not cover the full range of features provided by Documenter.jl. Refer to [Documenter's documentation](https://juliadocs.github.io/Documenter.jl/stable) if you encounter anything that is not covered by the sections below.

#### Modifying files in `doc/src/`

Most of the source text for the Julia Manual is located in `doc/src/`. To update or add new text to any one of the existing files the following steps should be followed:

1. update the text in whichever `.md` files are applicable;
2. run `make docs` from the root directory;
3. check the output in `doc/_build/html/` to make sure the changes are correct;
4. commit your changes and open a pull request.

> **Note**
>
> The contents of `doc/_build/` does **not** need to be committed when you make changes.

To add a **new file** to `doc/src/` rather than updating a file replace step `1` above with

1. add the file to the appropriate subdirectory in `doc/src/` and also add the file path to the `PAGES` vector in `doc/make.jl`.

#### Modifying an existing docstring in `base/`

All docstrings are written inline above the methods or types they are associated with and can be found by clicking on the `source` link that appears below each docstring in the HTML file. The steps needed to make a change to an existing docstring are listed below:

1. find the docstring in `base/`;
2. update the text in the docstring;
3. run `make docs` from the root directory;
4. check the output in `doc/_build/html/` to make sure the changes are correct;
5. commit your changes and open a pull request.

#### Adding a new docstring to `base/`

The steps required to add a new docstring are listed below:

1. find a suitable definition in `base/` that the docstring will be most applicable to;
2. add a docstring above the definition;
3. find a suitable `@docs` code block in one of the `doc/src/stdlib/` files where you would like the docstring to appear;
4. add the name of the definition to the `@docs` code block. For example, with a docstring added to a function `bar`

    ```julia
    "..."
    function bar(args...)
        # ...
    end
    ```

   you would add the name `bar` to a `@docs` block in `doc/src/stdlib/`

        ```@docs
        foo
        bar # <-- Added this one.
        baz
        ```

5. run `make docs` from the root directory;
6. check the output in `doc/_build/html` to make sure the changes are correct;
7. commit your changes and open a pull request.

#### Doctests

Examples written within docstrings can be used as testcases known as "doctests" by annotating code blocks with `jldoctest`.

    ```jldoctest
    julia> uppercase("Docstring test")
    "DOCSTRING TEST"
    ```

A doctest needs to match an interactive REPL including the `julia>` prompt. To run doctests you need to run `make -C doc doctest=true` from the root directory. It is recommended to add the header `# Examples` above the doctests.

#### News-worthy changes

For new functionality and other substantial changes, add a brief summary to `NEWS.md`. The news item should cross reference the pull request (PR) parenthetically, in the form `([#pr])`. To add the PR reference number, first create the PR, then push an additional commit updating `NEWS.md` with the PR reference number.  We periodically run `./julia doc/NEWS-update.jl` from the julia directory to update the cross-reference links, but this should not be done in a typical PR in order to avoid conflicting commits.

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

### Contributing to core functionality or base libraries

*By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

The Julia community uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests (PR). You can make pull requests for incomplete features to get code review. The convention is to prefix the pull request title with "WIP:" for Work In Progress, or "RFC:" for Request for Comments when work is completed and ready for merging. This will prevent accidental merging of work that is in progress.

Note: These instructions are for adding to or improving functionality in the base library. Before getting started, it can be helpful to discuss the proposed changes or additions on the [Julia Discourse forum](https://discourse.julialang.org) or in a GitHub issue---it's possible your proposed change belongs in a package rather than the core language. Also, keep in mind that changing stuff in the base can potentially break a lot of things. Finally, because of the time required to build Julia, note that it's usually faster to develop your code in stand-alone files, get it working, and then migrate it into the base libraries.

Add new code to Julia's base libraries as follows (this is the "basic" approach; see a more efficient approach in the next section):

 1. Edit the appropriate file in the `base/` directory, or add new files if necessary. Create tests for your functionality and add them to files in the `test/` directory. If you're editing C or Scheme code, most likely it lives in `src/` or one of its subdirectories, although some aspects of Julia's REPL initialization live in `ui/`.

 2. Add any new files to `sysimg.jl` in order to build them into the Julia system image.

 3. Add any necessary export symbols in `exports.jl`.

 4. Include your tests in `test/Makefile` and `test/choosetests.jl`.

Build as usual, and do `make clean testall` to test your contribution. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent (be careful – this command will delete any files lying around that aren't checked into git).

Note: You can run specific test files with `make`:

    make test-bitarray

or with the `runtests.jl` script, e.g. to run `test/bitarray.jl` and `test/math.jl`:

    ./usr/bin/julia test/runtests.jl bitarray math

Make sure that [Travis](https://www.travis-ci.org) greenlights the pull request with a [`Good to merge` message](https://blog.travis-ci.com/2012-09-04-pull-requests-just-got-even-more-awesome).

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

For convenience, there are also `test-revise-*` targets for every `test-*`
target that use Revise to load any modifications to Base into the current
process before running the corresponding test. This can be useful as a shortcut
on the command line (since tests aren't always designed to be run outside the
runtest harness).

### Code Formatting Guidelines

#### General Formatting Guidelines for Julia code contributions

 - 4 spaces per indentation level, no tabs
 - use whitespace to make the code more readable
 - no whitespace at the end of a line (trailing whitespace)
 - comments are good, especially when they explain the algorithm
 - try to adhere to a 92 character line length limit
 - use upper camel case convention for modules, type names
 - use lower case with underscores for method names
 - it is generally preferred to use ASCII operators and identifiers over
   Unicode equivalents whenever possible
 - in docstring refer to the language as "Julia" and the executable as "`julia`"

#### General Formatting Guidelines For C code contributions

 - 4 spaces per indentation level, no tabs
 - space between if and ( (if (x) ...)
 - newline before opening { in function definitions
 - f(void) for 0-argument function declarations
 - newline between } and else instead of } else {
 - if one part of an if..else chain uses { } then all should
 - no whitespace at the end of a line

### Git Recommendations For Pull Requests

 - Avoid working from the `master` branch of your fork, creating a new branch will make it easier if Julia's `master` changes and you need to update your pull request.
 - Try to [squash](http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html) together small commits that make repeated changes to the same section of code so your pull request is easier to review, and Julia's history won't have any broken intermediate commits. A reasonable number of separate well-factored commits is fine, especially for larger changes.
 - If any conflicts arise due to changes in Julia's `master`, prefer updating your pull request branch with `git rebase` versus `git merge` or `git pull`, since the latter will introduce merge commits that clutter the git history with noise that makes your changes more difficult to review.
 - If you see any unrelated changes to submodules like `deps/libuv`, `deps/openlibm`, etc., try running `git submodule update` first.
 - Descriptive commit messages are good.
 - Using `git add -p` or `git add -i` can be useful to avoid accidentally committing unrelated changes.
 - GitHub does not send notifications when you push a new commit to a pull request, so please add a comment to the pull request thread to let reviewers know when you've made changes.
 - When linking to specific lines of code in discussion of an issue or pull request, hit the `y` key while viewing code on GitHub to reload the page with a URL that includes the specific version that you're viewing. That way any lines of code that you refer to will still make sense in the future, even if the content of the file changes.
 - Whitespace can be automatically removed from existing commits with `git rebase`.
   - To remove whitespace for the previous commit, run
     `git rebase --whitespace=fix HEAD~1`.
   - To remove whitespace relative to the `master` branch, run
     `git rebase --whitespace=fix master`.

## Resources

* Julia
  - **Homepage:** <https://julialang.org>
  - **Community:** <https://julialang.org/community/>
  - **Source code:** <https://github.com/JuliaLang/julia>
  - **Documentation:** <https://docs.julialang.org/>
  - **Code coverage:** <https://coveralls.io/r/JuliaLang/julia>

* Design of Julia
  - [Julia: A Fresh Approach to Numerical Computing](https://julialang.org/research/julia-fresh-approach-BEKS.pdf)
  - [Julia: Dynamism and Performance Reconciled by Design](http://janvitek.org/pubs/oopsla18b.pdf)
  - [All Julia Publications](https://julialang.org/research)

* Using GitHub
  - [Using Julia with GitHub (video)](https://www.youtube.com/watch?v=wnFYV3ZKtOg)
  - [Using Julia on GitHub (notes for video)](https://gist.github.com/2712118#file_Julia_git_pull_request.md)
  - [General GitHub documentation](https://help.github.com)
  - [GitHub pull request documentation](https://help.github.com/articles/creating-a-pull-request/)
