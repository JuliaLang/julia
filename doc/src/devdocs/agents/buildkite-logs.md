# Reviewing Buildkite CI logs

Julia's CI runs on Buildkite (pipeline `julialang/julia-master`). The public web
UI requires sign-in to download `raw_log`, but the Buildkite frontend's JSON log
endpoint is anonymously accessible for public pipelines. Recipe:

1. List failing jobs and their UUIDs (the fragment after `#` in the URL):

   ```sh
   gh pr checks <PR-number> | grep -E "fail|pending"
   ```

2. Fetch the log JSON (replace `<BUILD>` and `<JOB-UUID>`):

   ```sh
   curl -sS -H "Accept: application/json" \
     "https://buildkite.com/organizations/julialang/pipelines/julia-master/builds/<BUILD>/jobs/<JOB-UUID>/log" \
     -o /tmp/bk.json
   ```

   The log text lives under the JSON `output` field, with embedded HTML
   (`<time>` timestamps, ANSI-as-`<span>` colour) and entity-encoded shell
   output. Strip with e.g.:

   ```sh
   python3 -c "import json,re,html; s=json.load(open('/tmp/bk.json'))['output']; \
   s=re.sub(r'<[^>]+>','',s); print(html.unescape(s))" | tail -200
   ```

The same JSON endpoint also serves still-running jobs (partial output). Note
that the top-level Build/Check/Test jobs on a PR are pipeline launchers — the
actual per-platform builds are spawned as child jobs whose UUIDs only appear
once they start.
