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

1. Run `make check-whitespace` before creating the PR to make sure you're not committing any whitespace errors.

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
- If you are adding a new test, add it to an existing test file. Do not create a new test file unless explicitly instructed.
- Write one comment at the top of the test to explain what is being tested.
  Otherwise keep comments minimal.
- Use the environment variable `JULIA_TEST_FAILFAST=1` to make tests fail fast.

### External dependencies

When modifying external dependencies (patches in `deps/patches/` or version updates in `deps/`):

1. Always test builds with `USE_BINARYBUILDER=0` to ensure source builds work correctly
2. For patches to external libraries:
   - Verify the patch applies cleanly by running the extraction and patch steps
   - Test the full build of the dependency: `make -C deps USE_BINARYBUILDER=0 compile-<depname>`
   - Prefer using the full upstream commit in `git am` format (e.g., `git format-patch`) which includes proper commit metadata
3. When updating dependency versions, ensure all associated patches still apply

### Writing code
After writing code, look up the docstring for each function you used. If there
are recommendations or additional considerations that apply to these functions,
make sure to take them into account.

#### Specific instructions
- Do not `ccall` runtime C functions directly if there are existing wrappers for the function.
- Do not explicitly add a module prefix if the code you're adding is in the same module. E.g. do not use `Base.` for code in Base unless required.

## Commit message formatting

When writing commit messages, follow the format "component: Brief summary" for
the title. In the body of the commit message, provide a brief prose summary
of the purpose of the changes made. Do not specifically mention added tests, comments,
documentation, etc., unless this is the main purpose of the change. Do not mention
the test plan, unless it differs from what you were instructed to do in AGENTS.md.
If your change fixes one or more issues, use the syntax "Fixes #" at the end of the commit message, but do not include it in the title.

When referencing external GitHub PRs or issues, use proper GitHub interlinking format (e.g., `owner/repo#123` for PRs/issues).
When fixing CI failures, include the link to the specific CI failure in the commit message.

When creating pull requests, if the pull request consists of one commit only,
use the body of the commit for the body of the pull request. If there are multiple
commits in the pull request, follow the same guidelines for the pull request
as for the commit body.
