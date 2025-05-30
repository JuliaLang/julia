# Information for AI agents

## Module Organization
- `base/` - Core standard library (loaded at startup)
- `stdlib/` - Standard library packages (can be loaded independently)
- `Compiler/` - Julia compiler as a separate module (can be swapped)
- `src/` - C/C++ runtime and LLVM codegen
- `cli/` - Command-line interface and loader
- `doc/` - Documentation and User Manual

## Running Julia

You should have a recent binary copy of julia in your `$HOME/.juliaup/bin` directory.
You may use this julia executable for validation.
If a built version of Julia exists in the current source tree (at `usr/bin/julia`),
prefer that version.
Note that any changes you make to the source code after the binary is built
will not be reflected, unless you use `Revise`.

## For all changes

1. Run `make test-whitespace` before creating the PR to make sure you're not committing any whitespace errors.

## Building Julia

If you made changes to the runtime (any files in `src/`), you will need to rebuild
julia. Run `make -j` to rebuild julia. This process may take up to 10 minutes
depending on your changes.

## Using Revise

If you have made changes to files included in the system image (base/ or stdlib/),
and need to run code with these changes included, you can use `Revise`.
To do so, run `using Revise; Revise.track(Base)` (or Revise.track with the stdlib you modified).
The test system supports doing this automatically (see below).

## Specific instructions for particular changes

### Doctests

If you have changed doctests (i.e. any `jldoctest` code block), you should run the doctests before
preparing a PR. See `doc/README.md` for how to do this. For instructions on writing doctests, see
`doc/src/devdocs/contributing/jldoctests.md`. Note that the doctests may take up to 15 minutes.
Do not terminate the doctests before completion. If you are ChatGPT, you may have to increase yield_timeout_ms.

### Test changes

If you have changed a test (e.g. `foo`), you should run `make test-revise-foo` for the
corresponding test to ensure that the test is still passing with your changes.
If you are adding a new test, add it to an existing test file. Do not
create a new test file unless explicitly instructed.

## Commit message formatting

When writing commit messages, follow the format "component: Brief summary" for
the title. In the body of the commit message, provide a brief prose summary
of the purpose of the changes made. Do not specifically mention added tests, comments,
documentation, etc., unless this is the main purpose of the change. Do not mention
the test plan, unless it differs from what you were instructed to do in AGENTS.md.
If your change fixes one or more issues, use the syntax "Fixes #" at the end of the commit message, but do not include it in the title.
