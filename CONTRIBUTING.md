# Contributing

Hi! Thanks for checking out Julia and contributing. Please consider joining the [Julia dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev). If you have questions or problems, that's a good place to post them. The [online Julia docs](http://docs.julialang.org/en/latest/) might also be helpful. To contribute changes, you'll need a free GitHub account (if you're reading this on GitHub, you probably already have one). And of course, you'll want a working copy of Julia, preferably built from source. In list form, here are the recommended things to do:

* Join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev)

* Create a [GitHub account](https://github.com/signup/free)

* [Fork Julia](https://github.com/JuliaLang/julia/fork_select)

* Build the software and libraries (the first time takes time)
  <p>
  Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.
  </p>

* Keep Julia current
  <p>
  Julia is a fast-moving target, and many details of the language are still settling out. Keeping your repository up to date and rebasing your work in progress frequently 
  </p>

* Relax

## Submitting your contributions

The Julia team uses [GitHub issues](https://github.com/JuliaLang/julia/issues) to track and discuss problems, feature requests, and pull requests. By contributing code to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).

You can make pull requests for incomplete features to get code review, in which case we have a convention of prefixing the pull request title with "RFC:" to indicate that the work is incomplete so it doesn't accidentally get merged into the master branch before it's baked.

Before submitting, make sure that all tests pass by running `make testall`. Even better, add your own tests for your change or feature to the test files in `tests/`. If your contribution includes changes to Makefiles or external dependencies, do make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent.

## Getting help

While getting familiar with Julia, and using unfamiliar aspects of the language, remember to check out the [docs](http://docs.julialang.org/en/latest/). The docs are [searchable](http://docs.julialang.org/en/latest/search/), but if you use a script blocker then you'll have to unblock that page. The [source code](https://github.com/JuliaLang/julia) is an excellent source of examples and, of course, always the most definitive resource on how things work. If you're still stumped, post something on [the dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev), but it won't hurt to search first to see if there's already a discussion about what you're stuck on.

# Resources

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
