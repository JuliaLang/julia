### Improving documentation

*By contributing documentation to Julia, you are agreeing to release it under the [MIT License](https://github.com/JuliaLang/julia/tree/master/LICENSE.md).*

Julia's documentation source files are stored in the `doc/` directory and all docstrings are found in `base/`. Like everything else these can be modified using `git`. Documentation is built with [Documenter.jl](https://github.com/JuliaDocs/Documenter.jl), which uses Markdown syntax. The HTML documentation can be built locally by running

```
make docs
```

from Julia's root directory. This will rebuild the Julia system image, then install or update the package dependencies required to build the documentation, and finally build the HTML documentation and place the resulting files in `doc/_build/html/`.

> **Note**
>
> When making changes to any of Julia's documentation it is recommended that you run `make docs` to check that your changes are valid and do not produce any errors before opening a pull request.

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

A doctest needs to match an interactive REPL including the `julia>` prompt. It is recommended to add the header `# Examples` above the doctests.

See the documentation of [writing jldoctests](@ref writing-jldoctests) for best
practices on how to write doctests for common scenarios and the `doc/README.md`
file for how to run the doctests.
