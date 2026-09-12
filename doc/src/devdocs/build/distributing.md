Binary distributions
=======================================

These notes are for those wishing to compile a binary distribution of Julia
for distribution on various platforms. We love users spreading Julia as
far and wide as they can, trying it out on as wide an array of
operating systems and hardware configurations as possible. As each
platform has specific gotchas and processes that must be followed in
order to create a portable, working Julia distribution, we have
separated most of the notes by OS.

Note that while the code for Julia is
[MIT-licensed, with a few exceptions](https://github.com/JuliaLang/julia/blob/master/LICENSE.md),
the distribution created by the techniques described herein will be
GPL licensed, as various dependent libraries such as `SuiteSparse` are
GPL licensed. We do hope to have a non-GPL distribution of Julia in the future.

Versioning and Git
------------------
The Makefile uses both the `VERSION` file and commit hashes and tags from the
git repository to generate the `base/version_git.jl` with information we use to
fill the splash screen and the `versioninfo()` output. If you for some reason
don't want to have the git repository available when building you should
pregenerate the `base/version_git.jl` file with:

    make -C base version_git.jl.phony

Julia has lots of build dependencies where we use patched versions that have not
yet been included by the popular package managers. These dependencies will usually
be automatically downloaded when you build, but if you want to be able to build
Julia on a computer without internet access you should create a full-source-dist
archive with the special make target

    make full-source-dist

that creates a julia-version-commit.tar.gz archive with all required dependencies.

When compiling a tagged release in the git repository, we don't display the
branch/commit hash info in the splash screen. You can use this line to show
a release description of up to 45 characters. To set this line you have
to create a Make.user file containing:

    override TAGGED_RELEASE_BANNER = "my-package-repository build"

The official release builds are not built from the `vx.y.z` tag: the tag is
created only after the artifacts have been built, validated, and published
(see [Tagging the release](#tagging-the-release)). Instead, passing
`JULIA_RELEASE_BUILD=1` to `make` declares the build to be the release build
of the version in the `VERSION` file, producing the same artifacts that
building from the tag would (release naming, `tagged_commit` set). It refuses
to run when `VERSION` carries a `-DEV` (or legacy `-pre`) suffix.

Target Architectures
--------------------

By default, Julia optimizes its system image to the native architecture of
the build machine. This is usually not what you want when building packages,
as it will make Julia fail at startup on any machine with incompatible CPUs
(in particular older ones with more restricted instruction sets).

We therefore recommend that you pass the `MARCH` variable when calling `make`,
setting it to the baseline target you intend to support. This will determine
the target CPU for both the Julia executable and libraries, and the system
image (the latter can also be set using [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET)). Typically useful
values for x86 CPUs are `x86-64` and `core2` (for 64-bit builds) and
`pentium4` (for 32-bit builds). Unfortunately, CPUs older than Pentium 4
are currently not supported (see
[this issue](https://github.com/JuliaLang/julia/issues/7185)).

The full list of CPU targets supported by LLVM can be obtained by running
`llc -mattr=help`.

Linux
-----

On Linux, `make binary-dist` creates a tarball that contains a fully
functional Julia installation. If you wish to create a distribution
package such as a `.deb`, or `.rpm`, some extra effort is needed. See the
[julia-debian](https://github.com/staticfloat/julia-debian) repository
for an example of what metadata is needed for creating `.deb` packages
for Debian and Ubuntu-based systems. See the
[Fedora package](https://src.fedoraproject.org/rpms/julia)
for RPM-based distributions. Although we have not yet experimented
with it, [Alien](https://wiki.debian.org/Alien) could be used to
generate Julia packages for various Linux distributions.

Julia supports overriding standard installation directories via `prefix`
and other environment variables you can pass when calling `make` and
`make install`. See Make.inc for their list. `DESTDIR` can also be used
to force the installation into a temporary directory.

By default, Julia loads `$prefix/etc/julia/startup.jl` as an
installation-wide initialization file. This file can be used by
distribution managers to set up custom paths or initialization code.
For Linux distribution packages, if `$prefix` is
set to `/usr`, there is no `/usr/etc` to look into. This requires
the path to Julia's private `etc` directory to be changed. This can
be done via the `sysconfdir` make variable when building. Simply
pass `sysconfdir=/etc` to `make` when building and Julia will first
check `/etc/julia/startup.jl` before trying
`$prefix/etc/julia/startup.jl`.

OS X
----

To create a binary distribution on OSX, build Julia first, then cd to
`contrib/mac/app`, and run `make` with the same makevars that were used
with `make` when building Julia proper. This will then
create a `.dmg` file in the `contrib/mac/app` directory holding a
completely self-contained Julia.app.

Alternatively, Julia may be built as a framework by invoking `make` with the
`darwinframework` target and `DARWIN_FRAMEWORK=1` set. For example,
`make DARWIN_FRAMEWORK=1 darwinframework`.

Windows
-------

Instructions for creating a Julia distribution on Windows are described in the
[build devdocs for Windows](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/build/windows.md).

Notes on BLAS and LAPACK
------------------------

Julia builds OpenBLAS by default, which includes the BLAS and LAPACK
libraries. On 32-bit architectures, Julia builds OpenBLAS to use
32-bit integers, while on 64-bit architectures, Julia builds OpenBLAS
to use 64-bit integers (ILP64). It is essential that all Julia functions
that call BLAS and LAPACK API routines use integers of the correct width.

Most BLAS and LAPACK distributions provided on linux distributions,
and even commercial implementations ship libraries that use 32-bit
APIs. In many cases, a 64-bit API is provided as a separate library.

When using vendor provided or OS provided libraries, a `make` option
called `USE_BLAS64` is available as part of the Julia build. When doing
`make USE_BLAS64=0`, Julia will call BLAS and LAPACK assuming a 32-bit
API, where all integers are 32-bit wide, even on a 64-bit architecture.

Other libraries that Julia uses, such as SuiteSparse also
use BLAS and LAPACK internally. The APIs need to be consistent across
all libraries that depend on BLAS and LAPACK. The Julia build process
will build all these libraries correctly, but when overriding defaults
and using system provided libraries, this consistency must be ensured.

Also note that Linux distributions sometimes ship several versions of
OpenBLAS, some of which enable multithreading, and others only working
in a serial fashion. For example, in Fedora, `libopenblasp.so` is threaded,
but `libopenblas.so` is not. We recommend using the former for optimal
performance. To choose an OpenBLAS library whose name is different from
the default `libopenblas.so`, pass `LIBBLAS=-l$(YOURBLAS)` and
`LIBBLASNAME=lib$(YOURBLAS)` to `make`, replacing `$(YOURBLAS)` with the
name of your library. You can also add `.so.0` to the name of the library
if you want your package to work without requiring the unversioned `.so`
symlink.

Finally, OpenBLAS includes its own optimized version of LAPACK. If you
set `USE_SYSTEM_BLAS=1` and `USE_SYSTEM_LAPACK=1`, you should also set
`LIBLAPACK=-l$(YOURBLAS)` and `LIBLAPACKNAME=lib$(YOURBLAS)`. Else, the
reference LAPACK will be used and performance will typically be much lower.

Starting with Julia 1.7, Julia uses [libblastrampoline](https://github.com/JuliaLinearAlgebra/libblastrampoline)
to pick a different BLAS at runtime.

# Point releasing 101

Creating a point/patch release consists of several distinct steps.

## Backporting commits

Some pull requests are labeled "backport pending x.y", e.g. "backport pending 0.6".
This designates that the next subsequent release tagged from the release-x.y branch
should include the commit(s) in that pull request.
Once the pull request is merged into master, each of the commits should be [cherry
picked](https://git-scm.com/docs/git-cherry-pick) to a dedicated branch that will
ultimately be merged into release-x.y.

### Creating a backports branch

First, create a new branch based on release-x.y.
The typical convention for Julia branches is to prefix the branch name with your
initials if it's intended to be a personal branch.
For the sake of example, we'll say that the author of the branch is Jane Smith.

```
git fetch origin
git checkout release-x.y
git rebase origin/release-x.y
git checkout -b js/backport-x.y
```

This ensures that your local copy of release-x.y is up to date with origin before
you create a new branch from it.

### Cherry picking commits

Now we do the actual backporting.
Find all merged pull requests labeled "backport pending x.y" in the GitHub web UI.
For each of these, scroll to the bottom where it says "someperson merged commit
`123abc` into `master` XX minutes ago".
Note that the commit name is a link; if you click it, you'll be shown the contents
of the commit.
If this page shows that `123abc` is a merge commit, go back to the PR page---we
don't want merge commits, we want the actual commits.
However, if this does not show a merge commit, it means that the PR was squash-merged.
In that case, use the git SHA of the commit, listed next to commit on this page.

Once you have the SHA of the commit, cherry-pick it onto the backporting branch:

```
git cherry-pick -x -e <sha>
```

There may be conflicts which need to be resolved manually.
Once conflicts are resolved (if applicable), add a reference to the GitHub pull
request that introduced the commit in the body of the commit message.

After all of the relevant commits are on the backports branch, push the branch to
GitHub.

## Checking for performance regressions

Point releases should never introduce performance regressions.
Luckily the Julia benchmarking bot, Nanosoldier, can run benchmarks against any
branch, not just master.
In this case we want to check the benchmark results of js/backport-x.y against
release-x.y.
To do this, awaken the Nanosoldier from his robotic slumber using a comment on
your backporting pull request:

```markdown
@nanosoldier `runbenchmarks(ALL, vs=":release-x.y")`
```

This will run all registered benchmarks on release-x.y and js/backport-x.y and
produce a summary of results, marking all improvements and regressions.

If Nanosoldier finds any regressions, try verifying locally and rerun Nanosoldier
if necessary.
If the regressions are deemed to be real rather than just noise, you'll have to
find a commit on master to backport that fixes it if one exists, otherwise you
should determine what caused the regression and submit a patch (or get someone who
knows the code to submit a patch) to master, then backport the commit once that's
merged.
(Or submit a patch directly to the backport branch if appropriate.)

## Building test binaries

There is no manual step here anymore: every push to the backports PR (and to the
`release-x.y` branch) is built and tested by the `julia-ci` Buildkite pipeline for
all supported platforms, and the binaries are downloadable from the build's
artifacts.

## Checking for package breakages

Point releases should never break packages, with the possible exception of packages
that are doing some seriously questionable hacks using Base internals that are
not intended to be user-facing.
(In those cases, maybe have a word with the package author.)

Whether the forthcoming release breaks packages is checked with
[PkgEval](https://github.com/JuliaCI/PkgEval.jl), driven by
[Nanosoldier](https://github.com/JuliaCI/Nanosoldier.jl): comment on the backports
PR with something like

```
@nanosoldier `runtests(ALL, vs = ":release-x.y")`
```

to test every registered package against the backports branch, compared to the
current release branch.
Nanosoldier replies with a report of newly failing packages; investigate those
(locally or by reading their test logs) and distinguish real regressions caused by
the backports from pre-existing failures and flaky tests.
If a backported commit turns out to cause breakage, drop or fix it and rerun.

## Merging backports into the release branch

After you have ensured that

* the backported commits pass all of Julia's unit tests,
* there are no performance regressions introduced by the backported commits as compared
  to the release branch, and
* the backported commits do not break any registered packages,

then the backport branch is ready to be merged into release-x.y.
Once it's merged, go through and remove the "backport pending x.y" label from all pull
requests containing the commits that have been backported.
Do not remove the label from PRs that have not been backported.

The release-x.y branch should now contain all of the new commits.
The last thing we want to do to the branch is to adjust the version number.
To do this, submit a PR against release-x.y that edits the VERSION file to remove `-DEV`
from the version number.
Once that's merged, the branch is ready to release.

The release is built and published from that commit on release-x.y; the
`vx.y.z` tag is created only afterwards, as the last step (see
[Tagging the release](#tagging-the-release)). If a problem is found after the
version bump has been merged, simply merge the fix and release from the new
tip of the branch — nothing has been tagged or published yet, so nothing
needs to be moved or deleted.

## Publishing the release

Building, signing, and uploading release artifacts is fully automated in CI; no
binaries are built or signed on anyone's machine, and no signing key material
exists outside of a cloud KMS. The authoritative runbook, including recovery
procedures, lives in `ops/README.md` of
[JuliaCI/julia-buildkite](https://github.com/JuliaCI/julia-buildkite); in outline:

1. The release manager starts a release `julia-ci` build (New Build with
   branch = `release-x.y` at the release commit, with `RELEASE_VERSION=x.y.z`
   in the environment). The pipeline checks `RELEASE_VERSION` against the
   `VERSION` file and builds with `JULIA_RELEASE_BUILD=1`, producing the same
   artifacts a build of the (future) `vx.y.z` tag would: binaries for every
   platform, plus the light and full source tarballs (with the bundled HTML
   documentation). The tests run as usual.
2. When the tests pass, `julia-ci` automatically triggers the trusted
   `julia-publish` pipeline, which signs everything (GPG for the Linux/FreeBSD
   and source tarballs, notarization for macOS, Authenticode for Windows),
   publishes version-named artifacts to the `julialangnightlies` bucket, and
   deploys the documentation. Nothing user-facing points at this bucket, so
   this is the moment to download and sanity-check the artifacts.
3. The release manager then starts a `julia-promote` build for the same
   commit and version, which copies everything into the release bucket layout
   served at `julialang-s3.julialang.org` — including the source tarballs under
   `bin/src/x.y/` — repoints the `julia-x.y-latest-*` files, generates and
   uploads the `bin/checksums/julia-x.y.z.{sha256,md5}` files, and purges the
   CDN cache.

## Tagging the release

The tag is created last, once the artifacts are published, so that it
immutably records exactly what was released and never needs to be moved or
deleted. Check out release-x.y at the exact commit that was built and
promoted, then run

```
git tag v$(cat VERSION)
git push --tags
```

The tag does not trigger any builds. What remains:

1. Dispatch the CI workflow of
   [VersionsJSONUtil.jl](https://github.com/JuliaLang/VersionsJSONUtil.jl) to
   regenerate `versions.json`, then the "Update Version DB" workflow of
   [juliaup](https://github.com/JuliaLang/juliaup) (its upload jobs need manual
   approval) so `juliaup` picks up the release.
2. Create the GitHub release with the CI-signed source tarballs attached by
   running `contrib/github_source_release.sh x.y.z` — it downloads them from the
   release bucket, verifies the signatures and layout, and shows what it would
   do; rerun with `--execute` to create the (pre)release and upload.
3. Update [the website](https://github.com/JuliaLang/www.julialang.org): bump
   the version in `config.md` and regenerate the old releases page with
   `downloads/oldreleases.jl`. Finally, announce the release on Discourse.

After the release, submit another PR to release-x.y to bump the patch number
and add `-DEV` back to the end, denoting that the branch state reflects a
development version of the next point release in the x.y series.


## Verifying signatures

Signing is performed automatically by the buildbots. The commands below verify
that the published binaries are correctly signed; run each on the relevant
platform.

### GPG (Linux, FreeBSD, and source tarballs)

Each tarball ships with a detached `.asc` signature. Import the Julia release
signing public key (published at <https://julialang.org/assets/juliareleases.asc>)
and verify:

```
gpg --verify julia-x.y.z-linux-x86_64.tar.gz.asc julia-x.y.z-linux-x86_64.tar.gz
```

A "Good signature" line means the tarball is authentic and intact.

### macOS

The `.dmg` is signed and carries a stapled notarization ticket. Verify the disk
image itself -- run these against the `.dmg` file, not the mounted volume (see
the note below):

```
xcrun stapler validate julia-x.y.z-macos-x86_64.dmg
spctl --assess --type open --context context:primary-signature --verbose \
    julia-x.y.z-macos-x86_64.dmg
```

`stapler` should report that the validate action worked, and `spctl` should
report `accepted` with `source=Notarized Developer ID`.

To check the app bundle, first copy it out of the mounted image to a writable
location (for example, drag it to `/Applications`), then assess it:

```
spctl --assess --type install --verbose /Applications/Julia-x.y.app
```

This should likewise report `accepted` with `source=Notarized Developer ID`.

!!! note
    Run these checks against the `.dmg` file or a copied-out app bundle, never
    against the app inside the mounted disk image. The image mounts read-only,
    and `codesign --verify` errors out on it with "internal error in Code
    Signing subsystem".

### Windows

Check the Authenticode signature on the installer:

```
signtool verify /pa julia-x.y.z-win64.exe
```
