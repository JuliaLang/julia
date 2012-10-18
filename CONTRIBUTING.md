Hi! Thanks for checking out Julia. If you have questions or problems, the [Julia dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev) is a good place to post them, but you should check out the [online Julia docs](http://docs.julialang.org/en/latest/) first. If you have changes you'd like to contribute, you'll need a [free GitHub account](https://github.com/signup/free). (If you're reading this on GitHub, you probably already have one.) You'll also want a working copy of Julia, built from source (you can still contribute with a binary install, but it's harder and not really recommended). In list form, here's what to do to become a contributor:

* Join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev).

* Create a [GitHub account](https://github.com/signup/free).

* [Fork Julia](https://github.com/JuliaLang/julia/fork_select).

* Build the software and libraries (the first time takes a while, but it's fast after that). Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.

* Keep Julia current. Julia is a fast-moving target, and many details of the language are still settling out. Keep your repository up-to-date and rebase your work in progress frequently.

* Relax and have fun.

## Submitting your contributions

The Julia team uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests.

You can make pull requests for incomplete features to get code review, in which case we have a convention of prefixing the pull request title with "RFC:" to indicate that the work is incomplete so it doesn't accidentally get merged into the master branch before it's ready.

Before submitting, make sure that all tests pass by running `make testall`. Even better, add your own tests for your change or feature to the test files in `tests/`. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent (be careful – this command will delete any files lying around that aren't checked into git).

*By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

## Getting help

While getting familiar with Julia, remember to check out [the docs](http://docs.julialang.org/en/latest/), keeping in mind that they are [searchable](http://docs.julialang.org/en/latest/search/). (If you use a script blocker then you'll have to unblock that page.) The [source code](https://github.com/JuliaLang/julia) is an excellent source of examples (and it's mostly pretty approachable). If you're still stumped, post something on [the dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev), but you may want to search the archives first to see if there's already been a discussion about what you're stuck on.

## Resources

* Julia
  - [Julia](http://julialang.org)
  - [Julia Manual and Library Guide](http://docs.julialang.org/en/latest/)
  - [Julia dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev)
* Working with Types
  - [Some Type Patterns](https://github.com/JuliaLang/julia/wiki/Code-Patterns)
  - [The Type Hierarchy](https://github.com/JuliaLang/julia/wiki/Types-Hierarchy)
* Using GitHub
  - [Using Julia with GitHub (video)](http://www.youtube.com/watch?v=wnFYV3ZKtOg&feature=youtu.be)
  - [Using Julia on GitHub (notes for video)](https://gist.github.com/2712118#file_julia_git_pull_request.md)
  - [General GitHub documentation](http://help.github.com/)
  - [GitHub pull request documentation](http://help.github.com/send-pull-requests/)
