---
name: buildkite-logs
description: Fetch and inspect Julia Buildkite CI logs without web sign-in. Use when debugging Julia CI failures, reviewing Buildkite jobs, or when the Buildkite MCP is unavailable.
---

# Reviewing Buildkite CI logs

Use this when investigating Julia Buildkite CI failures, especially if the
Buildkite MCP is unavailable. The recipe requires `gh`, `curl`, `python3`, and
network access to GitHub and Buildkite.

Julia's CI runs on Buildkite. PR builds run in the `julialang/julia-pr`
pipeline, post-merge master builds in `julialang/julia-ci`, and scheduled runs
in `julialang/julia-master-scheduled` (the old `julia-master` pipeline no
longer exists). The public web UI requires sign-in to download `raw_log`, but
two frontend JSON endpoints are anonymously accessible for public pipelines.
Recipe:

1. Find the build number. `gh pr checks <PR-number>` gives launcher-job URLs
   like `https://buildkite.com/julialang/julia-pr/builds/<BUILD>#<uuid>`; for a
   master commit, use the commit statuses
   (`gh api repos/JuliaLang/julia/commits/<sha>/status`). The `#<uuid>`
   fragments there are only the top-level launcher jobs (Build/Check/Test/…),
   not the per-platform jobs.

2. List all jobs in the build, with names, states, exit statuses, and UUIDs:

   ```sh
   curl -sS -H "Accept: application/json" \
     "https://buildkite.com/julialang/<PIPELINE>/builds/<BUILD>/data/jobs" \
     -o /tmp/bkjobs.json
   python3 -c "import json; [print(j['state'],'|',j.get('exit_status'),'|',j['name'],'|',j['id']) \
   for j in json.load(open('/tmp/bkjobs.json'))['records']]"
   ```

   This `/data/jobs` endpoint is what the build page's frontend uses; it
   returns every job in one page (`records`, plus `has_next_page`). Do NOT use
   `builds/<BUILD>.json` for job discovery — anonymously it returns build
   metadata with an *empty* `jobs` array (the `statistics` field still shows
   the true job count).

3. Fetch a job's log JSON (replace `<PIPELINE>`, `<BUILD>`, `<JOB-UUID>`):

   ```sh
   curl -sS -H "Accept: application/json" \
     "https://buildkite.com/organizations/julialang/pipelines/<PIPELINE>/builds/<BUILD>/jobs/<JOB-UUID>/log" \
     -o /tmp/bk.json
   ```

   The log text lives under the JSON `output` field, with embedded HTML
   (`<time>` timestamps, ANSI-as-`<span>` colour) and entity-encoded shell
   output. Strip with e.g.:

   ```sh
   python3 -c "import json,re,html; s=json.load(open('/tmp/bk.json'))['output']; \
   s=re.sub(r'<[^>]+>','',s); print(html.unescape(s))" > /tmp/bk.txt
   ```

   Write the stripped log to a file and search it (logs can be hundreds of KB);
   piping the whole thing into context wastes tokens.

The log endpoint also serves still-running jobs (partial output). For a test
job that hung, the in-tree watchdog (`.buildkite/utilities/timeout.jl`,
`JL_TERM_TIMEOUT`) prints per-task Julia backtraces of every worker before
killing it, and core dumps are uploaded as artifacts with an `lldb bt all`
summary in the log — search the log for `---- Task`, `Waiting for`, and
`core dumped`.
