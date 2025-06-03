
### Contributing to patch releases

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
been automated via [`Backporter`](https://github.com/KristofferC/Backporter): Backporter
searches for merged pull requests with the relevant `backport-X.Y` tag, and attempts to
cherry-pick the commits from those pull requests onto the `backports-release-X.Y` branch.
Some commits apply successfully without intervention, others not so much. The latter
commits require "manual" backporting, with which help is generally much appreciated.
Backporter generates a report identifying those commits it managed to backport automatically
and those that require manual backporting; this report is usually copied into the first
post of the pull request associated with `backports-release-X.Y` and maintained as
additional commits are automatically and/or manually backported.

When contributing a manual backport, if you have the necessary permissions, please push the
backport directly to the `backports-release-X.Y` branch. If you lack the relevant
permissions, please open a pull request against the `backports-release-X.Y` branch with the
manual backport. Once the manual backport is live on the `backports-release-X.Y` branch,
please remove the `backport-X.Y` tag from the originating pull request for the commits.
