# Contributing to patch releases

The process of [creating a patch release](https://docs.julialang.org/en/v1/devdocs/build/distributing/#Point-releasing-101) is roughly as follows:

1. Create a new branch (e.g. `backports-release-1.10`) against the relevant minor release
   branch (e.g. `release-1.10`). Usually a corresponding pull request is created as well.

2. Add commits, nominally from `master` (hence "backports"), to that branch.
   See below for more information on this process.

3. Run the [BaseBenchmarks.jl](https://github.com/JuliaCI/BaseBenchmarks.jl) benchmark
   suite and [PkgEval.jl](https://github.com/JuliaCI/PkgEval.jl) package ecosystem
   exerciser against that branch. Nominally BaseBenchmarks.jl and PkgEval.jl are
   invoked via [Nanosoldier.jl](https://github.com/JuliaCI/Nanosoldier.jl) from
   the pull request associated with the backports branch. Fix any issues.

4. Once all test and benchmark reports look good, merge the backports branch into
   the corresponding release branch (e.g. merge `backports-release-1.10` into
   `release-1.10`).

5. Open a pull request that bumps the version of the relevant minor release to the
   next patch version, e.g. as in [this pull request](https://github.com/JuliaLang/julia/pull/37718).

6. Ping `@JuliaLang/releases` to tag the patch release and update the website.

7. Open a pull request that bumps the version of the relevant minor release to the
   next prerelease patch version, e.g. as in [this pull request](https://github.com/JuliaLang/julia/pull/37724).

Step 2 above, i.e. backporting commits to the `backports-release-X.Y` branch, has largely
been automated via [`Backporter`](https://github.com/KristofferC/Backporter). A pull request
is marked for backporting by adding the `backport X.Y` label. Backporter searches for merged
pull requests carrying that label and cherry-picks their commits onto `backports-release-X.Y`.
Some commits apply cleanly without intervention, others do not. The latter require "manual"
backporting, with which help is generally much appreciated.

Backporter maintains a checklist in the first post of the pull request associated with
`backports-release-X.Y` (between `<!-- BACKPORTER:BEGIN -->` and `<!-- BACKPORTER:END -->`
markers), listing the pull requests it backported, those that need a manual backport, and
labeled pull requests that are not merged yet. The checklist is regenerated from the state
of the branch on every run, so it does not need to be edited by hand.

## Manual backports

Backporter recognizes a pull request as already backported when a commit on
`backports-release-X.Y`

- has the pull request number in its subject line, e.g. `Fix foo (#12345)`, or
- has a `(cherry picked from commit <sha>)` trailer naming the merge commit or one of the
  pull request's commits, or
- carries the same patch as one of the pull request's commits.

A manual backport made with `git cherry-pick -x <sha>` that keeps the original subject
line satisfies the first two, whether it reaches the branch by direct push or by merging a
separate pull request.

If you have the necessary permissions, push the manual backport directly to the
`backports-release-X.Y` branch. Otherwise open a pull request against
`backports-release-X.Y`, based on its current tip (the branch may be rebased or
force-pushed while backports are collected), and mention the original pull request in
the title, e.g. `[release-X.Y] Fix foo (#12345)`. Nothing else is required: no comment on
the backports pull request, and no edit of its checklist; Backporter picks up the change
the next time it runs.

The `backport X.Y` label is removed once the backport has been released, i.e. once the
commit is on `release-X.Y`. Backporter's audit mode (`--audit --apply`) does this, and is
available to maintainers as the manually triggered `Backport Label Audit` GitHub Actions
workflow. Removing the label by hand after the manual backport is on
`backports-release-X.Y` is harmless but not necessary.
