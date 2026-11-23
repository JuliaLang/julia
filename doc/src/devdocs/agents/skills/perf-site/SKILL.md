---
name: perf-site
description: Get data from perf.julialang.org - CI job and build times, Nanosoldier benchmark history, PkgEval status and failure reasons of a package, TTFX, runtime benchmarks and binary size of master commits and open PRs, package downloads. Use whenever a question needs that data, including whether a PR slows CI down, before curling the site or downloading anything.
---

# perf.julialang.org data

Read <https://perf.julialang.org/llms.txt> first: it says how to get the data, and
which way suits which question. Don't scrape the site's pages.

- What a PR does to TTFX, binary size and runtime benchmarks: `/api/commit/<PR number>`.
- Whether a PR slows CI down: the site has master builds only. Compare the PR build's
  jobs from `https://buildkite.com/julialang/julia-pr/builds/<n>/data/jobs` (public)
  with the same job names in `/api/timing/runs?since=`. One build against a week of
  runs is mostly agent variance.
- Before blaming a commit for a step in a series, check the matching `/api/notes/*`
  route. A machine or methodology change can cause a step too.
- A 429 from `/db/` usually means other clients have used up its shared limit. Use
  the API, the extracts or the snapshot instead of retrying.
- Download the snapshot or extracts into a temporary directory, not the source tree.
