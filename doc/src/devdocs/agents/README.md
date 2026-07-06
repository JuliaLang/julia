# Agent Skills

Julia keeps canonical project-local Agent Skills under
`doc/src/devdocs/agents/skills/`. These files follow the
[Agent Skills](https://agentskills.io) `SKILL.md` format.

The `.agents/skills/` and `.claude/skills/` entries are symlinks to the canonical
skills directory for automatic discovery by skills-aware agents. Do not edit
through those discovery paths directly; edit the canonical `SKILL.md` under
`doc/src/devdocs/agents/skills/`.

The documentation build renders each canonical `SKILL.md` with its Agent Skill metadata:

- [`doctests`](skills/doctests/index.md) — writing and verifying `jldoctest` code blocks.
- [`test-changes`](skills/test-changes/index.md) — running and updating tests after changing them.
- [`c-static-analysis`](skills/c-static-analysis/index.md) — Clang static analysis and GC-rooting for C/C++ changes under `src/`.
- [`external-deps`](skills/external-deps/index.md) — modifying external dependencies (`deps/`, patches) and JLLs.
- [`buildkite-logs`](skills/buildkite-logs/index.md) — fetching and inspecting Buildkite CI logs without web sign-in.
- [`compiler-jl`](skills/compiler-jl/index.md) — developing and testing Compiler.jl.
- [`julia-syntax-lowering`](skills/julia-syntax-lowering/index.md) — developing and testing JuliaSyntax and JuliaLowering.
