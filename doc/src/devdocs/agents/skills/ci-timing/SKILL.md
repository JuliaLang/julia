---
name: ci-timing
description: Compare a Julia PR's Buildkite job durations against recent CI history (per-job mean/min/max over the last N days). Use when asked whether a PR slows down or speeds up CI, or to check a build's timings against the weekly baseline.
---

# CI timing analysis

```sh
julia --startup-file=no --project=contrib/ci-timing contrib/ci-timing/ci_timing_compare.jl <PR-number>
```

Prints a markdown table, slowest job first, plus a TOTAL row: each job's duration
in the PR's `julia-pr` build vs. the mean/min/max of the same job name over the
last 7 days of [julia-ci-timing](https://github.com/JuliaCI/julia-ci-timing)
history. `⚠️` = above the weekly max, `✅` = below the weekly min. No Buildkite
token needed; `gh` is only needed to turn a PR number into a build.

Flags: `--build julia-pr/853`, `--days N`, `--include-failed`, `--min-seconds N`
(hides short jobs).

Caveats when reading it:

- One build vs. a week of samples — anything inside the min/max band is agent
  variance, and even ⚠️/✅ rows need a second build to mean anything.
- `juliasyntax`/`Launch *` jobs are seconds long, so their percentages are noise;
  `--min-seconds 120` drops them.
- A fleet-wide shift in one direction skews TOTAL and is not a PR effect.
- Broken (never run), running, and superseded retry jobs are excluded; a failed
  job's time is cut short or stretched by the failure.

For logs behind a slow or failed job, see the `buildkite-logs` skill.
