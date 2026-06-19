# Information for AI agents

## Module Organization
- [`base/`](base/) - Core standard library (loaded at startup)
- [`stdlib/`](stdlib/) - Standard library packages (can be loaded independently)
- [`Compiler/`](Compiler/) - Julia compiler as a separate module (can be swapped)
- [`src/`](src/) - C/C++ runtime and LLVM codegen
- [`cli/`](cli/) - Command-line interface and loader
- [`doc/`](doc/) - Documentation and User Manual

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

After modifying any C/C++ file under `src/`, also run the Clang static analysis
checks — see the `c-static-analysis` skill ([`doc/src/devdocs/agents/skills/c-static-analysis/`](doc/src/devdocs/agents/skills/c-static-analysis/SKILL.md)).

## Using Revise

If you have made changes to files included in the system image (base/ or stdlib/),
and need to run code with these changes included, you can use `Revise`.
To do so, run `using Revise; Revise.track(Base)` (or Revise.track with the stdlib you modified).
The test system supports doing this automatically.

For instance testing Base changes without rebuilding, using failfast, you can run:
```
JULIA_TEST_FAILFAST=1 ./julia -e 'using Revise; Revise.track(Base); include("test.jl")'
```

## Writing code

After writing code, look up the docstring for each function you used. If there
are recommendations or additional considerations that apply to these functions,
make sure to take them into account.

- Do not `ccall` runtime C functions directly if there are existing wrappers for the function.
- Do not explicitly add a module prefix if the code you're adding is in the same module. E.g. do not use `Base.` for code in Base unless required.

## Task-specific skills

Detailed, situational procedures are provided as Agent Skills following the
[agentskills.io](https://agentskills.io) open standard. The canonical location is
`doc/src/devdocs/agents/skills/`; `.agents/skills/` and `.claude/skills/` hold
checked-in mirror copies so skills-aware agents can discover them automatically.
Agents that support the standard load each skill's `description` automatically
and pull in the full `SKILL.md` when relevant; otherwise read the canonical
`SKILL.md` directly.

- [`doc/src/devdocs/agents/skills/doctests/`](doc/src/devdocs/agents/skills/doctests/SKILL.md) — writing and verifying `jldoctest` code blocks.
- [`doc/src/devdocs/agents/skills/test-changes/`](doc/src/devdocs/agents/skills/test-changes/SKILL.md) — running and updating tests after changing them.
- [`doc/src/devdocs/agents/skills/c-static-analysis/`](doc/src/devdocs/agents/skills/c-static-analysis/SKILL.md) — Clang static analysis and GC-rooting for C/C++ changes under `src/`.
- [`doc/src/devdocs/agents/skills/external-deps/`](doc/src/devdocs/agents/skills/external-deps/SKILL.md) — modifying external dependencies (`deps/`, patches) and JLLs.
- [`doc/src/devdocs/agents/skills/buildkite-logs/`](doc/src/devdocs/agents/skills/buildkite-logs/SKILL.md) — fetching and inspecting Buildkite CI logs without web sign-in.

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
