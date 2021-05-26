<a name="logo"/>
<div align="center">
<a href="https://julialang.org/" target="_blank">
<img src="doc/src/assets/logo.svg" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

Code coverage:
[![coveralls][coveralls-img]](https://coveralls.io/r/JuliaLang/julia?branch=master)
[![codecov][codecov-img]](https://codecov.io/github/JuliaLang/julia?branch=master)

Documentation:
[![version 1][docs-img]](https://docs.julialang.org)

[appveyor-img]: https://img.shields.io/appveyor/ci/JuliaLang/julia/master.svg?label=Windows
[coveralls-img]: https://img.shields.io/coveralls/github/JuliaLang/julia/master.svg?label=coveralls
[codecov-img]: https://img.shields.io/codecov/c/github/JuliaLang/julia/master.svg?label=codecov
[docs-img]: https://img.shields.io/badge/docs-v1-blue.svg

## The Julia Language

Julia is a high-level, high-performance dynamic language for technical
computing.  The main homepage for Julia can be found at
[julialang.org](https://julialang.org/).  This is the GitHub
repository of Julia source code, including instructions for compiling
and installing Julia, below.

## Resources

- **Homepage:** <https://julialang.org>
- **Binaries:** <https://julialang.org/downloads/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Documentation:** <https://docs.julialang.org>
- **Packages:** <https://julialang.org/packages/>
- **Discussion forum:** <https://discourse.julialang.org>
- **Slack:** <https://julialang.slack.com> (get an invite from <https://julialang.org/slack/>)
- **YouTube:** <https://www.youtube.com/user/JuliaLanguage>
- **Code coverage:** <https://coveralls.io/r/JuliaLang/julia>

New developers may find the notes in
[CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md)
helpful to start contributing to the Julia codebase.

### External Resources

- [**StackOverflow**](https://stackoverflow.com/questions/tagged/julia-lang)
- [**Twitter**](https://twitter.com/JuliaLanguage)
- [**Learning resources**](https://julialang.org/learning/)

## Binary Installation

If you would rather not compile the latest Julia from source,
platform-specific tarballs with pre-compiled binaries are also
[available for download](https://julialang.org/downloads/). The
downloads page also provides details on the
[different tiers of support](https://julialang.org/downloads/#support-tiers)
for OS and platform combinations.

If everything works correctly, you will see a Julia banner and an
interactive prompt into which you can enter expressions for
evaluation.  You can read about [getting
started](https://docs.julialang.org/en/v1/manual/getting-started/) in the manual.

**Note**: Although some system package managers provide Julia, such
installations are neither maintained nor endorsed by the Julia
project. They may be outdated, broken and/or unmaintained. We
recommend you use the official Julia binaries instead.

## Building Julia

First, make sure you have all the [required
dependencies](https://github.com/JuliaLang/julia/blob/master/doc/build/build.md#required-build-tools-and-external-libraries) installed.
Then, acquire the source code by cloning the git repository:

    git clone https://github.com/JuliaLang/julia.git

By default you will be building the latest unstable version of
Julia. However, most users should use the most recent stable version
of Julia. You can get this version by changing to the Julia directory
and running:

    git checkout v1.6.1

Now run `make` to build the `julia` executable.

Building Julia requires 2GiB of disk space and approximately 4GiB of virtual memory.

**Note:** The build process will fail badly if any of the build directory's parent directories have spaces or other shell meta-characters such as `$` or `:` in their names (this is due to a limitation in GNU make).

Once it is built, you can run the `julia` executable after you enter your julia directory and run

    ./julia

Your first test of Julia determines whether your build is working
properly. From the UNIX/Windows command prompt inside the `julia`
source directory, type `make testall`. You should see output that
lists a series of running tests; if they complete without error, you
should be in good shape to start using Julia.

You can read about [getting
started](https://docs.julialang.org/en/v1/manual/getting-started/)
in the manual.

In case this default build path did not work, detailed build instructions
are included in the [build documentation](https://github.com/JuliaLang/julia/blob/master/doc/build).

### Uninstalling Julia

Julia does not install anything outside the directory it was cloned
into. Julia can be completely uninstalled by deleting this
directory. Julia packages are installed in `~/.julia` by default, and
can be uninstalled by deleting `~/.julia`.

## Source Code Organization

The Julia source code is organized as follows:

| Directory         | Contents                                                           |
| -                 | -                                                                  |
| `base/`           | source code for the Base module (part of Julia's standard library) |
| `stdlib/`         | source code for other standard library packages                    |
| `cli/`            | source for the command line interface/REPL                         |
| `contrib/`        | editor support for Julia source, miscellaneous scripts             |
| `deps/`           | external dependencies                                              |
| `doc/src/manual/` | source for the user manual                                         |
| `doc/build/`      | detailed notes for building Julia                                  |
| `src/`            | source for Julia language core                                     |
| `test/`           | test suites                                                        |
| `usr/`            | binaries and shared libraries loaded by Julia's standard libraries |

## Terminal, Editors and IDEs

The Julia REPL is quite powerful. See the section in the manual on
[the Julia REPL](https://docs.julialang.org/en/v1/stdlib/REPL/)
for more details.

On Windows we highly recommend running Julia in a modern terminal,
such as [Windows Terminal from the Microsoft Store](https://aka.ms/terminal).

Support for editing Julia is available for many
[widely used editors](https://github.com/JuliaEditorSupport):
[Emacs](https://github.com/JuliaEditorSupport/julia-emacs),
[Vim](https://github.com/JuliaEditorSupport/julia-vim),
[Sublime Text](https://github.com/JuliaEditorSupport/Julia-sublime), and many
others.

Supported IDEs include: [julia-vscode](https://github.com/JuliaEditorSupport/julia-vscode) (VS
Code plugin), [Juno](http://junolab.org/) (Atom plugin). [Jupyter](https://jupyter.org/)
notebooks are available through the [IJulia](https://github.com/JuliaLang/IJulia.jl) package, and
[Pluto](https://github.com/fonsp/Pluto.jl) notebooks through the Pluto.jl package.
