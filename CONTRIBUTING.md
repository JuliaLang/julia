Hi! Thanks for checking out Julia. If you have questions or problems, the [Julia dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev) is a good place to post them, but you should check out the [online Julia docs](http://docs.Julialang.org/en/latest/) first. If you have changes you'd like to contribute, you'll need a [free GitHub account](https://github.com/signup/free). (If you're reading this on GitHub, you probably already have one.) You'll also want a working copy of Julia, built from source (you can still contribute with a binary install, but it's harder and not really recommended). In list form, here's what to do to become a contributor:

* Join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev).

* Create a [GitHub account](https://github.com/signup/free).

* [Fork Julia](https://github.com/JuliaLang/julia/fork_select).

* Build the software and libraries (the first time takes a while, but it's fast after that). Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.

* Keep Julia current. Julia is a fast-moving target, and many details of the language are still settling out. Keep your repository up-to-date and rebase your work in progress frequently.

* Learn to use [git](http://git-scm.com), the version control system used by GitHub and the Julia project.
  Try a tutorial such as the one [provided by GitHub](http://try.github.io/levels/1/challenges/1).

* Relax and have fun.

## Submitting your contributions

### Contributing to core functionality or base libraries

The Julia team uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests.

You can make pull requests for incomplete features to get code review. The convention is to prefix the pull request title with "WIP:" for Work In Progress, or "RFC:" for Request for Comments when work is completed and ready for merging. This will prevent accidental merging of work that is in progress.

Before submitting, make sure that all tests pass by running `make testall`. Add your own tests for the new functionality in `test/`. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent (be careful – this command will delete any files lying around that aren't checked into git). Make sure that [Travis](http://www.travis-ci.org) greenlights the pull request with a `Good to merge` message.

*By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

### Contributing a Julia package

Julia has a built-in [package manager](https://github.com/JuliaLang/METADATA.jl) based on git. A number of [packages](http://docs.julialang.org/en/latest/packages/packagelist/) across many domains are already available for julia. Developers are encouraged to provide their libraries as a Julia package. The Julia manual provides instructions on [creating julia packages](http://docs.julialang.org/en/latest/manual/packages/). 

For developers who need to wrap C libraries so that they can be called from Julia, the [Clang.jl](https://github.com/ihnorton/Clang.jl) package can help generate the wrappers automatically from the C header files.

## Getting help

While getting familiar with Julia, remember to check out [the docs](http://docs.Julialang.org/en/latest/), keeping in mind that they are [searchable](http://docs.Julialang.org/en/latest/search/). (If you use a script blocker then you'll have to unblock that page.) The [source code](https://github.com/JuliaLang/julia) is an excellent source of examples (and it's mostly pretty approachable). If you're still stumped, post something on [the dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev), but you may want to search the archives first to see if there's already been a discussion about what you're stuck on.

## Resources

* Julia
  - **Homepage:** <http://julialang.org>
  - **Mailing lists:** <http://julialang.org/mailing_lists>
  - **IRC:** <http://webchat.freenode.net/?channels=Julia>
  - **Source code:** <https://github.com/JuliaLang/julia>
  - **Git clone URL:** <git://github.com/JuliaLang/julia.git>
  - **Documentation:** <http://julialang.org/manual/>

* Design of Julia
  - [Julia: A Fast Dynamic Language for Technical Computing](http://julialang.org/images/julia-dynamic-2012-tr.pdf)

* Using GitHub
  - [Using Julia with GitHub (video)](http://www.youtube.com/watch?v=wnFYV3ZKtOg&feature=youtu.be)
  - [Using Julia on GitHub (notes for video)](https://gist.github.com/2712118#file_Julia_git_pull_request.md)
  - [General GitHub documentation](http://help.github.com/)
  - [GitHub pull request documentation](http://help.github.com/send-pull-requests/)
