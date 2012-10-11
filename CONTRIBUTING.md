# Contributing

We welcome contributions to Julia and appreciate your interest. There are capabilities to grow, interfaces to write, and bugs to squish.  Area experts may contribute by writing software that supports work in their own field.  To keep things percolating as smoothly as possible, we ask that you follow the advice and guidelines given here.

## ready, get set

Before contributing to Julia, you should join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev), read the user manual and library guide, and create a free GitHub account.  Follow these steps to get Julia copied to your machine in a way that lets you to work locally, rewind your edits, keep a remote backup, and safely contribute to Julia.

* Join the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev)

* Create a [GitHub account](https://github.com/signup/free)

* [Fork Julia](https://github.com/JuliaLang/julia/fork_select)

* Build the software and libraries (the first time takes time)
  <p>
  Detailed build instructions are in the [README](https://github.com/JuliaLang/julia/tree/master/README.md). Julia depends on several external packages; most are automatically downloaded and installed, but are less frequently updated than Julia itself.
  </p>

* Keep Julia current
  <p>
  Julia is a fast-moving target, and many details of the language are still settling out. Keeping your repository up to date and rebasing your work in progress requently 
  </p>

* Relax

## submitting your contributions

The Julia team uses the [GitHub issue tracker](https://github.com/JuliaLang/julia/issues) for accepting pull requests. Note that by contributing, you are agreeing to release your code under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).

You can make pull requests for incomplete features to get code review. Please prefix your pull request title with "RFC:" to indicate that the work is incomplete so it doesn't accidentally get pulled early.

Before submitting, please make sure that all tests pass by running `make testall`. Even better, add your own tests for your change or feature to the test files in `tests/`. If your contribution includes changes to Makefiles or external dependencies, make sure you can build Julia from a clean tree using `git clean -fdx` or equivalent.

## getting help
While getting familiar with Julia, and when using unfamiliar aspects of the language, the first recourse is to take advantage of the [docs](http://docs.julialang.org/en/latest/).  When [searching the docs](http://docs.julialang.org/en/latest/search/), if you use a script blocker then you must unblock the page.  The [source code](https://github.com/JuliaLang/julia) is an excellent source of examples.  Use the [dev list](https://groups.google.com/forum/?fromgroups#!forum/julia-dev) as a resource whenever you are stumped or curious or in need of other feedback.  It is always a good idea to search the dev list before posting -- there is already guidance helpful for common questions.

# resources

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
