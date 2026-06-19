# Agent Skills

Julia keeps canonical project-local Agent Skills under
`doc/src/devdocs/agents/skills/`. These files follow the
[Agent Skills](https://agentskills.io) `SKILL.md` format.

The `.agents/skills/` and `.claude/skills/` directories contain checked-in
mirror copies for automatic discovery by skills-aware agents.
Do not edit those copies directly. To update a skill, edit the canonical
`SKILL.md` under `doc/src/devdocs/agents/skills/`, then refresh and check the
mirrors:

```sh
make sync-agent-skills
make check-agent-skills
```

CI also checks that the mirror copies match the canonical files.

The documentation build renders each canonical `SKILL.md` with its Agent Skill metadata:

- [`doctests`](skills/doctests/index.md) — writing and verifying `jldoctest` code blocks.
- [`test-changes`](skills/test-changes/index.md) — running and updating tests after changing them.
- [`c-static-analysis`](skills/c-static-analysis/index.md) — Clang static analysis and GC-rooting for C/C++ changes under `src/`.
- [`external-deps`](skills/external-deps/index.md) — modifying external dependencies (`deps/`, patches) and JLLs.
- [`buildkite-logs`](skills/buildkite-logs/index.md) — fetching and inspecting Buildkite CI logs without web sign-in.
