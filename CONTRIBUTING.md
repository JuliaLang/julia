# Notes for Julia Contributors

Hi! If you are new to the Julia community: welcome, and thanks for trying Julia. Please be sure to respect our [community standards](https://julialang.org/community/standards) in all interactions.

If you are already familiar with Julia itself, this blog post by Katharine Hyatt on [Making your first Julia pull request](https://kshyatt.github.io/post/firstjuliapr/) is a great way to get started.

## Learning Julia

[The learning page](https://julialang.org/learning) has a great list of resources for new and experienced users alike.

## Filing an issue

### Before filing an issue

- Reporting a potential bug? Please read the "[How to file a bug report](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#how-to-file-a-bug-report)" section to make sure that all necessary information is included.

- Contributing code? Be sure to review the [contributor checklist](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md#contributor-checklist) for helpful tips on the tools we use to build Julia.

- Library feature requests are generally not accepted on this issue tracker. New libraries should be developed as [packages](https://julialang.github.io/Pkg.jl/v1/creating-packages/). Discuss ideas for libraries at the [Julia Discourse forum](https://discourse.julialang.org). Doing so will often lead to pointers to existing projects and bring together collaborators with common interests.

### How to file a bug report

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

### Contributor Checklist

* Create a [GitHub account](https://github.com/signup/free).

* [Fork Julia](https://github.com/JuliaLang/julia/fork).

* Build the software and libraries (the first time takes a while, but it's fast after that). Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.

* Keep Julia current. Julia is a fast-moving target, and many details of the language are still settling out. Keep the repository up-to-date and rebase work-in-progress frequently to make merges simpler.

* Learn to use [git](https://git-scm.com), the version control system used by GitHub and the Julia project. Try a tutorial such as the one [provided by GitHub](https://try.GitHub.io/levels/1/challenges/1).

* Review discussions on the [Julia Discourse forum](https://discourse.julialang.org).

* Relax and have fun!

### Guidance for specific changes

The julia project maintains a more in-depth `Contributor's Guide` as part of our
developer documentation. Here you can find more in-depth guidance for how to write
specific kinds of changes. In particular, you want want to read:

- [How to contribute code changes](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/contributing/code-changes.md)
- [How to contribute additional tests](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/contributing/tests.md)
- [How to work on documentation](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/contributing/documentation.md)
- [Workflow tips for working with git](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/contributing/git-workflow.md)

## Resources

* Julia
  - **Homepage:** <https://julialang.org>
  - **Community:** <https://julialang.org/community/>
  - **Source code:** <https://github.com/JuliaLang/julia>
  - **Documentation:** <https://docs.julialang.org>
  - **Code coverage:** <https://codecov.io/github/JuliaLang/julia>

* Design of Julia
  - [Julia: A Fresh Approach to Numerical Computing](https://julialang.org/assets/research/julia-fresh-approach-BEKS.pdf)
  - [Julia: Dynamism and Performance Reconciled by Design](http://janvitek.org/pubs/oopsla18b.pdf)
  - [All Julia Publications](https://julialang.org/research)

* Using GitHub
  - [Using Julia with GitHub (video)](https://www.youtube.com/watch?v=wnFYV3ZKtOg)
  - [Using Julia on GitHub (notes for video)](https://gist.github.com/2712118#file_Julia_git_pull_request.md)
  - [General GitHub documentation](https://help.github.com)
  - [GitHub pull request documentation](https://help.github.com/articles/creating-a-pull-request/)
