# Information for AI agents

## Policy

- Sign every commit with an `Assisted-by: <tool> (<model>)` trailer, e.g.
  `Assisted-by: Claude Code (Opus 5)`, naming the model and not just the harness. It replaces
  `Co-authored-by:` and any co-author trailer your harness adds; never list a tool as an author.
- Open PRs or post comments only with the user's explicit permission.
- Do not add `JL_GC_PROMISE_ROOTED` without user or maintainer confirmation. It asserts that a
  value is rooted; it does not root it.

## Commits and pull requests

- Title: `component: Brief summary`. Body: brief prose on the purpose of the change. Don't
  mention added tests, comments, or docs unless they are the point, and don't describe the test plan.
- When fixing CI, link the specific failure.
- Quote macro names in backticks (`` `@inbounds` ``) in commits and PRs, so GitHub doesn't notify
  the user with that handle.
- Rebase if the base commit is more than two days old; CI tests the PR head, not a merge with master.
- Draft PR bodies for the human author to reword: reuse the commit body (or, for several commits,
  summarize them the same way), note any separate tool review, and state that the author has *not*
  read it yet, for them to update once they have.

## Before finishing

Run these once, at the end of a task, not after every edit:

- `make check-whitespace` (`make fix-whitespace` fixes it).
- Changed `src/*.c`/`*.cpp`: `make -C src analyze-<file>` for each changed file, without the
  extension (`make -C src install-analysis-deps` first if needed).
- Changed `jldoctest` blocks: `make -C doc doctest=true revise=true`.
- Changed `Compiler/`: rebuild and run `make test-Compiler`.
- Changed `JuliaSyntax/`: also run the JuliaLowering tests.
- Changed `deps/` patches: `make -C deps distclean-<dep>`, then
  `make -C deps USE_BINARYBUILDER_<DEP>=0 compile-<dep>`; the patch must apply without fuzz.

## Buildkite CI logs

PR builds run in `julialang/julia-pr`, master builds in `julialang/julia-ci`. These work without
signing in (send `Accept: application/json`):

- Jobs: `https://buildkite.com/julialang/<pipeline>/builds/<build>/data/jobs` lists every job under
  `records`. Not `builds/<build>.json`, whose `jobs` list is empty without sign-in.
- Log: `https://buildkite.com/organizations/julialang/pipelines/<pipeline>/builds/<build>/jobs/<job-id>/log`
  returns HTML in the `output` field, also for running jobs. In hung tests, search for `---- Task`,
  `Waiting for`, and `core dumped`.
- Artifacts: the same URL ending in `/artifacts` lists each `path` and `url`;
  `curl -L "https://buildkite.com<url>"` downloads one.
