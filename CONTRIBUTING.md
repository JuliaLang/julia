# Notes for Julia Contributors

Hi! Thanks for checking out Julia. If you have questions or problems, the [Julia dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev) is a good place to post them, but you should check out the [online Julia docs](http://docs.Julialang.org/en/latest/) first. If you have changes you'd like to contribute, you'll need a [free GitHub account](https://github.com/signup/free). (If you're reading this on GitHub, you probably already have one.) You'll also want a working copy of Julia, built from source (you can still contribute with a binary install, but it's harder and not really recommended). In list form, here's what to do to become a contributor:

* Join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev).

* Create a [GitHub account](https://github.com/signup/free).

* [Fork Julia](https://github.com/JuliaLang/julia/fork).

* Build the software and libraries (the first time takes a while, but it's fast after that). Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.

* Keep Julia current. Julia is a fast-moving target, and many details of the language are still settling out. Keep your repository up-to-date and rebase your work in progress frequently.

* Learn to use [git](http://git-scm.com), the version control system used by GitHub and the Julia project. Try a tutorial such as the one [provided by GitHub](http://try.github.io/levels/1/challenges/1).

* Relax and have fun.

## How to file a bug report

A useful bug report filed as a Github issue provides information about how to reproduce the error.

1. Before opening a new [Github issue](https://github.com/JuliaLang/julia/issues):
  - Try searching the existing issues or the [`julia-users` mailing list](https://groups.google.com/forum/#!forum/julia-users) to see if someone else has already noticed the same problem.
  - Try some simple debugging techniques to help isolate the problem.
    - Try running the code with the debug build of Julia with `make debug`, which produces the `usr/bin/julia-debug`.
    - Consider running `julia-debug` with a debugger such as `gdb` or `lldb`. Obtaining even a simple [backtrace](http://www.unknownroad.com/rtfm/gdbtut/gdbsegfault.html) is very useful.
    - If Julia segfaults, try following [these debugging tips](https://gist.github.com/staticfloat/6188418#segfaults-during-bootstrap-sysimgjl) to help track down the specific origin of the bug.

2. If the problem is caused by a Julia package rather than core Julia, file a bug report with the relevant package author rather than here.

3. When filing a bug report, provide where possible:
  - The full error message, including the backtrace.
  - A minimal working example, i.e. the smallest chunk of code that triggers the error. Ideally, this should be code that can be pasted into a REPL or run from a source file. If the code is larger than (say) 50 lines, consider putting it in a [gist](https://gist.github.com).
  - The version of Julia you are using as provided by the `versioninfo()` command. Occasionally, the longer output produced by `versioninfo(true)` may be useful also, especially if the issue is related to a specific package.

4. When pasting code blocks or output, put triple backquotes (\`\`\`) around the text so Github will format it nicely. You can format code statements by surrounding it in single backquotes (\`). Be aware that the `@` sign tags users on GitHub, so references to macros should always be in single backquotes. See [GitHub's guide on Markdown](https://guides.github.com/features/mastering-markdown/) for more formatting tricks.

## Submitting your contributions

### Contributing a Julia package

Julia has a built-in [package manager](https://github.com/JuliaLang/METADATA.jl) based on git. A number of [packages](http://pkg.julialang.org/) across many domains are already available for Julia. Developers are encouraged to provide their libraries as a Julia package. The Julia manual provides instructions on [creating Julia packages](http://docs.julialang.org/en/latest/manual/packages/).

For developers who need to wrap C libraries so that they can be called from Julia, the [Clang.jl](https://github.com/ihnorton/Clang.jl) package can help generate the wrappers automatically from the C header files.

### Improving documentation

*By contributing documentation to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

Julia's documentation is stored in the `doc` directory, and like everything else can be modified using `git`. However, for small changes one can also use GitHub's web interface:

- Navigate to https://github.com/JuliaLang/julia 
- Click `doc` 
- If you want to modify an entry in the help for Julia's standard library, click `stdlib`
- Pick the file you want to edit (for example, `base.rst`)
- Select the `master` branch (if not browsing it already)
- Click "Edit" 
- Click on the icon that looks like a fullscreen symbol ("Zen" mode) 
- Search for the function you want to change
- Make your changes 
- Exit Zen mode 
- Provide a title, and optionally a longer description of your change 
- Submit your change


### Contributing to core functionality or base libraries

*By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

The Julia community uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests. You can make pull requests for incomplete features to get code review. The convention is to prefix the pull request title with "WIP:" for Work In Progress, or "RFC:" for Request for Comments when work is completed and ready for merging. This will prevent accidental merging of work that is in progress.

Note: These instructions are for adding to or improving functionality in the base library. Before getting started, it can be helpful to discuss the proposed changes or additions on the mailing list or in a github issue---it's possible your proposed change belongs in a package rather than the core language. Also, keep in mind that changing stuff in the base can potentially break a lot of things. Finally, because of the time required to build julia, note that it's usually faster to develop your code in stand-alone files, get it working, and then migrate it into the base libraries.

Add new code to Julia's base libraries as follows:

 1. Edit the appropriate file in the `base/` directory, or add new files if necessary. Create tests for your functionality and add them to files in the `test/` directory. If you're editing C or Scheme code, most likely it lives in `src/` or one of its subdirectories, although some aspects of julia's REPL initialization live in `ui/`.

 2. Add any new files to `sysimg.jl` in order to build them into the Julia system image.
 
 3. Add any necessary export symbols in `exports.jl`.

 4. Include your tests in `test/Makefile` and `test/runtests.jl`.

Build as usual, and do `make clean testall` to test your contribution. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent (be careful – this command will delete any files lying around that aren't checked into git). Make sure that [Travis](http://www.travis-ci.org) greenlights the pull request with a `Good to merge` message.

##### General Formatting Guidelines For Julia code contributions

 - 4 space indent
 - use whitespace to make the code more readable
 - comments are good, especially when they explain the algorithm

##### General Formatting Guidelines For C code contributions

 - 4 space indent
 - space between if and ( (if (x) ...)
 - newline before opening { in function definitions
 - f(void) for 0-argument function declarations
 - newline between } and else instead of } else {
 - if one part of an if..else chain uses { } then all should

## Getting help

While getting familiar with Julia, remember to check out [the docs](http://docs.Julialang.org/en/latest/), keeping in mind that they are [searchable](http://docs.Julialang.org/en/latest/search/). (If you use a script blocker then you'll have to unblock that page.) The [source code](https://github.com/JuliaLang/julia) is an excellent source of examples (and it's mostly pretty approachable). If you're still stumped, post something on [the dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev), but you may want to search the archives first to see if there's already been a discussion about what you're stuck on.

## Resources

* Julia
  - **Homepage:** <http://julialang.org>
  - **Mailing lists:** <http://julialang.org/community/>
  - **IRC:** <http://webchat.freenode.net/?channels=Julia>
  - **Source code:** <https://github.com/JuliaLang/julia>
  - **Git clone URL:** <git://github.com/JuliaLang/julia.git>
  - **Documentation:** <http://julialang.org/manual/>
  - **CodeSpeed:** <http://speed.julialang.org>
  - **Status:** <http://status.julialang.org>

* Design of Julia
  - [Julia: A Fast Dynamic Language for Technical Computing](http://julialang.org/images/julia-dynamic-2012-tr.pdf)

* Using GitHub
  - [Using Julia with GitHub (video)](http://www.youtube.com/watch?v=wnFYV3ZKtOg&feature=youtu.be)
  - [Using Julia on GitHub (notes for video)](https://gist.github.com/2712118#file_Julia_git_pull_request.md)
  - [General GitHub documentation](http://help.github.com/)
  - [GitHub pull request documentation](http://help.github.com/send-pull-requests/)
