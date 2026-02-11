# Julia Documentation README

Julia's documentation is written in Markdown. A reference of all supported syntax can be found in the [manual](https://docs.julialang.org/en/v1/stdlib/Markdown/). All documentation can be found in the Markdown files in `doc/src/` and the docstrings in Julia source files in `base/` and `stdlib/`.

## Requirements

This documentation is built using the [Documenter.jl](https://github.com/JuliaDocs/Documenter.jl) package.

All dependencies are automatically installed into a sandboxed package directory in `doc/deps/` to avoid interfering with user-installed packages.

## Building

To build Julia's documentation run

```sh
$ make docs
```

from the root directory. This will build the HTML documentation and output it to the `doc/_build/` folder.

## Testing

To run the doctests found in the manual run

```sh
$ make -C doc doctest=true
```

from the root directory.

## Customizing Doctest Execution

By default, doctests are run using the in-tree Julia executable.
This behavior can be changed by setting the `JULIA_EXECUTABLE` Makefile variable.

> [!WARNING]
> Using a custom `JULIA_EXECUTABLE` will not pick up changes to docstrings for Base or any standard library built into the system image. To see the list of standard libraries that are part of the system image, you can run the `contrib/print_sorted_stdlibs.jl` script (e.g., `julia contrib/print_sorted_stdlibs.jl --only-sysimg`).
