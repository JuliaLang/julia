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

1. Run `make fix-whitespace` before creating the PR to make sure you're not committing any whitespace errors.
2. Add the AI tool as a Git co-author on all commits created by that tool.
3. Whenever a pull request is opened, you MUST disclose that the pull request was written with the assistance of generative AI.

## Building Julia

If you made changes to the runtime (any files in `src/`), you will need to rebuild
julia. Run `make -j` to rebuild julia. This process may take up to 10 minutes
depending on your changes.

After making changes, run static analysis checks:
  - First run `make -C src install-analysis-deps` to initialize dependencies (only needed once the first time).
  - Run `make -C src analyze-<filename> --output-sync -j8` (replace `<filename>` with the basename of any C or C++ file you modified, excluding headers).
  - Tests can also be rerun individually with `clang-sa-<filename>`, `clang-sagc-<filename>` or `clang-tidy-<filename>`.
  - If `clang-sagc-<filename>` fails, it may require adding `JL_GC_PUSH` statements, or `JL_GC_PROMISE_ROOTED` statements., or require fixing locks. Remember arguments are assumed rooted, so check the callers to make sure that is handled. If the value is being temporarily moved around in a struct or arraylist, `JL_GC_PROMISE_ROOTED(struct->field)` may be needed as a statement (it return void) immediately after reloading the struct before any use of struct. Put that promise as early in the code as is legal, near the definition not the use.

## Using Revise

If you have made changes to files included in the system image (base/ or stdlib/),
and need to run code with these changes included, you can use `Revise`.
To do so, run `using Revise; Revise.track(Base)` (or Revise.track with the stdlib you modified).
The test system supports doing this automatically (see below).

For instance testing Base changes without rebuilding, using failfast, you can run:
```
JULIA_TEST_FAILFAST=1 ./julia -e 'using Revise; Revise.track(Base); include("test.jl")'
```

## Specific instructions for particular changes

### Doctests

#### Writing doctests

If you are asked to write new doctests, first review `doc/src/devdocs/contributing/jldoctests.md`
for best practices.

#### Verifying doctests
If you have changed any `jldoctest` code blocks you should take
the following steps to verify your work:
- Review `doc/src/devdocs/contributing/jldoctests.md`. In particular, determine
  if any of the changed doctests require filters, labels or setup code.
- Run the doctests to verify that your change works:
    - To run doctest with the pre-built juliaup: `make -C doc doctest=true  revise=true JULIA_EXECUTABLE=$HOME/.juliaup/bin/julia`
    - To run doctest with in-trr julia (preferred): `make -C doc doctest=true revise=true`. Do not pass any other options.
    - IMPORTANT: The doctests may take up to 15 minutes. Do NOT terminate the doctests before completion. Do NOT use a timeout for doctests.
    - If you are ChatGPT, you may have to increase yield_timeout_ms.

Follow these steps for EVERY change you make in a doctest.

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

### External JLLs

To update a JLL to the latest version:
- Update the version number in the appropriate jll folder
- If the dependencies in the upstream jll changed, update the Project.toml
- Run `make -f contrib/refresh_checksums.mk <jll>` to update the checksums. This may take a few minutes.

### Writing code
After writing code, look up the docstring for each function you used. If there
are recommendations or additional considerations that apply to these functions,
make sure to take them into account.

#### Specific instructions
- Do not `ccall` runtime C functions directly if there are existing wrappers for the function.
- Do not explicitly add a module prefix if the code you're adding is in the same module. E.g. do not use `Base.` for code in Base unless required.

## Commit messages and pull requests

When writing commit messages, follow the format "component: Brief summary" for
the title. In the body of the commit message, provide a brief prose summary
of the purpose of the changes made. Do not specifically mention added tests, comments,
documentation, etc., unless this is the main purpose of the change. Do not mention
the test plan, unless it differs from what you were instructed to do in AGENTS.md.
If your change fixes one or more issues, use the syntax "Fixes #" at the end of the commit message, but do not include it in the title.

When referencing external GitHub PRs or issues, use proper GitHub interlinking format (e.g., `owner/repo#123` for PRs/issues).
When fixing CI failures, include the link to the specific CI failure in the commit message.

When creating pull requests:
1. If the pull request consists of one commit only, use the body of the commit for the body of the pull request.
2. If there are multiple commits in the pull request, follow the same guidelines for the pull request as for the commit body.
3. Make sure that the base commit of the pull request is recent (within the past two days) - if not rebase your changes first.
4. You MUST disclose that the pull request was written with the assistance of generative AI.
