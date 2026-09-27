---
name: doctests
description: Write and verify Julia `jldoctest` code blocks. Use whenever adding or changing a `jldoctest` block in a docstring (base/, stdlib/, Compiler/) or under doc/, and before opening a PR that touches doctests.
---

# Julia doctests

Use this whenever you add or modify any `jldoctest` code block.

## Writing

Before writing new doctests, review `doc/src/devdocs/contributing/jldoctests.md`
for best practices (filters, labels, setup code).

## Verifying

If you changed any `jldoctest` block you MUST verify it. Follow these steps for
EVERY doctest change:

1. Review `doc/src/devdocs/contributing/jldoctests.md`. In particular, determine
   if any of the changed doctests require filters, labels or setup code.
2. Run the doctests to verify that your change works:
   - To run doctest with the pre-built juliaup: `make -C doc doctest=true revise=true JULIA_EXECUTABLE=$HOME/.juliaup/bin/julia`
   - To run doctest with in-tree julia (preferred): `make -C doc doctest=true revise=true`. Do not pass any other options.
   - IMPORTANT: The doctests may take up to 15 minutes. Do NOT terminate the doctests before completion. Do NOT use a timeout for doctests.
   - If you are ChatGPT, you may have to increase yield_timeout_ms.
