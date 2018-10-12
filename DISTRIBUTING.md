Notes for building binary distributions
=======================================

These notes are for those wishing to compile a binary distribution of Julia
for distribution on various platforms.  We love users spreading Julia as
far and wide as they can, trying it out on as wide an array of
operating systems and hardware configurations as possible.  As each
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

Julia has lots of build dependencies where we use patched versions that has not
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

Target Architectures
--------------------

By default, Julia optimizes its system image to the native architecture of
the build machine. This is usually not what you want when building packages,
as it will make Julia fail at startup on any machine with incompatible CPUs
(in particular older ones with more restricted instruction sets).

We therefore recommend that you pass the `MARCH` variable when calling `make`,
setting it to the baseline target you intend to support. This will determine
the target CPU for both the Julia executable and libraries, and the system
image (the latter can also be set using `JULIA_CPU_TARGET`). Typically useful
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
[Fedora package](https://admin.fedoraproject.org/pkgdb/package/julia/)
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
the path to Julia's private `etc` directory to be changed.  This can
be done via the `sysconfdir` make variable when building.  Simply
pass `sysconfdir=/etc` to `make` when building and Julia will first
check `/etc/julia/startup.jl` before trying
`$prefix/etc/julia/startup.jl`.

OS X
----

To create a binary distribution on OSX, build Julia first, then cd to
`contrib/mac/app`, and run `make` with the same makevars that were used
with `make` when building Julia proper.  This will then
create a `.dmg` file in the `contrib/mac/app` directory holding a
completely self-contained Julia.app.

Note that if you want your `.app` to be able to run on OSX 10.6 Snow
Leopard, you must pass `USE_SYSTEM_LIBUNWIND=1` as one of the make
variables passed to both `make` processes. This disables the use of
`libosxunwind`, a more modern libunwind that relies on OS features
available only in 10.7+. Furthermore, support for OSX 10.6 and 10.7
requires that Julia is built with `USE_LIBCPP=0`.

Windows
-------

The best supported method of creating a Julia distribution on Windows
is to cross-compile from a Linux distribution such as Ubuntu. In-depth
compilation instructions [are
available](https://github.com/JuliaLang/julia/blob/master/README.windows.md).
However the important steps for redistribution are to ensure to `make
win-extras` in between `make` and `make binary-dist`.  After that process is
completed, the `.zip` file created in the head Julia directory will
hold a completely self-contained Julia.

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

Other libraries that Julia uses, such as ARPACK and SuiteSparse also
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

Compilation scripts
===================

The [julia-nightly-packaging](https://github.com/staticfloat/julia-nightly-packaging)
repository contains multiple example scripts to ease the creation of
binary packages. It also includes miscellaneous tools to do things such as
fetching the last good commit that passed the
[Travis](https://travis-ci.org/JuliaLang/julia/builds) tests.


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

After the backport PR has been merged into the `release-x.y` branch, update your local
clone of Julia, then get the SHA of the branch using

```
git rev-parse origin/release-x.y
```

Keep that handy, as it's what you'll enter in the "Revision" field in the buildbot UI.

For now, all you need are binaries for Linux x86-64, since this is what's used for
running PackageEvaluator.
Go to https://buildog.julialang.org, submit a job for `nuke_linux64`, then queue up a
job for `package_linux64`, providing the SHA as the revision.
When the packaging job completes, it will upload the binary to the `julialang2` bucket
on AWS.
Retrieve the URL, as it will be used for PackageEvaluator.

## Checking for package breakages

Point releases should never break packages, with the possible exception of packages
that are doing some seriously questionable hacks using Base internals that are
not intended to be user-facing.
(In those cases, maybe have a word with the package author.)

Checking whether changes made in the forthcoming new version will break packages can
be accomplished using [PackageEvaluator](https://github.com/JuliaCI/PackageEvaluator.jl),
often called "PkgEval" for short.
PkgEval is what populates the status badges on GitHub repos and on pkg.julialang.org.
It typically runs on one of the non-benchmarking nodes of Nanosoldier and uses Vagrant
to perform its duties in separate, parallel VirtualBox virtual machines.

### Setting up PackageEvaluator

Clone PackageEvaluator and create a branch called `backport-x.y.z`, and check it out.
Note that the required changes are a little hacky and confusing, and hopefully that will
be addressed in a future version of PackageEvaluator.
The changes to make will be modeled off of
[this commit](https://github.com/JuliaCI/PackageEvaluator.jl/commit/5ba6a3b000e7a3793391d16f695c8704b91d6016).

The setup script takes its first argument as the version of Julia to run and the second
as the range of package names (AK for packages named A-K, LZ for L-Z).
The basic idea is that we're going to tweak that a bit to run only two versions of Julia,
the current x.y release and our backport version, each with three ranges of packages.

In the linked diff, we're saying that if the second argument is LZ, use the binaries
built from our backport branch, otherwise (AK) use the release binaries.
Then we're using the first argument to run a section of the package list: A-F for input
0.4, G-N for 0.5, and O-Z for 0.6.

### Running PackageEvaluator

To run PkgEval, find a hefty enough machine (such as Nanosoldier node 1), then run

```
git clone https://github.com/JuliaCI/PackageEvaluator.jl.git
cd PackageEvaluator.jl/scripts
git checkout backport-x.y.z
./runvagrant.sh
```

This produces some folders in the scripts/ directory.
The folder names and their contents are decoded below:

| Folder name | Julia version | Package range |
| :---------: | :-----------: | :-----------: |
| 0.4AK       | Release       | A-F           |
| 0.4LZ       | Backport      | A-F           |
| 0.5AK       | Release       | G-N           |
| 0.5LZ       | Backport      | G-N           |
| 0.6AK       | Release       | O-Z           |
| 0.6LZ       | Backport      | O-Z           |

### Investigating results

Once that's done, you can use `./summary.sh` from that same directory to produce
a summary report of the findings.
We'll do so for each of the folders to aggregate overall results by version.

```
./summary.sh 0.4AK/*.json > summary_release.txt
./summary.sh 0.5AK/*.json >> summary_release.txt
./summary.sh 0.6AK/*.json >> summary_release.txt
./summary.sh 0.4LZ/*.json > summary_backport.txt
./summary.sh 0.5LZ/*.json >> summary_backport.txt
./summary.sh 0.6LZ/*.json >> summary_backport.txt
```

Now we have two files, `summary_release.txt` and `summary_backport.txt`, containing
the PackageEvaluator test results (pass/fail) for each package for the two versions.

To make these easier to ingest into a Julia, we'll convert them into CSV files then
use the DataFrames package to process the results.
To convert to CSV, copy each .txt file to a corresponding .csv file, then enter Vim
and execute `ggVGI"<esc>` then `:%s/\.json /",/g`.
(You don't have to use Vim; this just is one way to do it.)
Now process the results with Julia code similar to the following.

```julia
using DataFrames

release = readtable("summary_release.csv", header=false, names=[:package, :release])
backport = readtable("summary_backport.csv", header=false, names=[:package, :backport])

results = join(release, backport, on=:package, kind=:outer)

for result in eachrow(results)
    a = result[:release]
    b = result[:backport]
    if (isna(a) && !isna(b)) || (isna(b) && !isna(a))
        color = :yellow
    elseif a != b && occursin("pass", b)
        color = :green
    elseif a != b
        color = :red
    else
        continue
    end
    printstyled(result[:package], ": Release ", a, " -> Backport ", b, "\n", color=color)
end
```

This will write color-coded lines to `stdout`.
All lines in red must be investigated as they signify potential breakages caused by the
backport version.
Lines in yellow should be looked into since it means a package ran on one version but
not on the other for some reason.
If you find that your backported branch is causing breakages, use `git bisect` to
identify the problematic commits, `git revert` those commits, and repeat the process.

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
To do this, submit a PR against release-x.y that edits the VERSION file to remove `-pre`
from the version number.
Once that's merged, we're ready to tag.

## Tagging the release

It's time!
Check out the release-x.y branch and make sure that your local copy of the branch is
up to date with the remote branch.
At the command line, run

```
git tag v$(cat VERSION)
git push --tags
```

This creates the tag locally and pushes it to GitHub.

After tagging the release, submit another PR to release-x.y to bump the patch number
and add `-pre` back to the end.
This denotes that the branch state reflects a prerelease version of the next point
release in the x.y series.

Follow the remaining directions in the Makefile.

## Signing binaries

Some of these steps will require secure passwords.
To obtain the appropriate passwords, contact Elliot Saba (staticfloat) or Alex Arslan
(ararslan).
Note that code signing for each platform must be performed on that platform (e.g. Windows
signing must be done on Windows, etc.).

### Linux

Code signing must be done manually on Linux, but it's quite simple.
First obtain the file `julia.key` from the CodeSigning folder in the `juliasecure` AWS
bucket.
Add this to your GnuPG keyring using

```
gpg --import julia.key
```

This will require entering a password that you must obtain from Elliot or Alex.
Next, set the trust level for the key to maximum.
Start by entering a `gpg` session:

```
gpg --edit-key julia
```

At the prompt, type `trust`, then when asked for a trust level, provide the maximum
available (likely 5).
Exit GnuPG.

Now, for each of the Linux tarballs that were built on the buildbots, enter

```
gpg -u julia --armor --detach-sig julia-x.y.z-linux-<arch>.tar.gz
```

This will produce a corresponding .asc file for each tarball.
And that's it!

### macOS

Code signing should happen automatically on the macOS buildbots.
However, it's important to verify that it was successful.
On a system or virtual machine running macOS, download the .dmg file that was built on
the buildbots.
For the sake of example, say that the .dmg file is called `julia-x.y.z-osx.dmg`.
Run

```
mkdir ./jlmnt
hdiutil mount -readonly -mountpoint ./jlmnt julia-x.y.z-osx.dmg
codesign -v jlmnt/Julia-x.y.app
```

Be sure to note the name of the mounted disk listed when mounting!
For the sake of example, we'll assume this is `disk3`.
If the code signing verification exited successfully, there will be no output from the
`codesign` step.
If it was indeed successful, you can detach the .dmg now:

```
hdiutil eject /dev/disk3
rm -rf ./jlmnt
```

If you get a message like

> Julia-x.y.app: code object is not signed at all

then you'll need to sign manually.

To sign manually, first retrieve the OS X certificates from the CodeSigning folder
in the `juliasecure` bucket on AWS.
Add the .p12 file to your keychain using Keychain.app.
Ask Elliot Saba (staticfloat) or Alex Arslan (ararslan) for the password for the key.
Now run

```
hdiutil convert julia-x.y.z-osx.dmg -format UDRW -o julia-x.y.z-osx_writable.dmg
mkdir ./jlmnt
hdiutil mount -mountpoint julia-x.y.z-osx_writable.dmg
codesign -s "AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE" --deep jlmnt/Julia-x.y.app
```

This may fail with a message like

> Julia-x.y.app: resource fork, Finder information, or similar detritus not allowed

If that's the case, you'll need to remove extraneous attributes:

```
xattr -cr jlmnt/Julia-x.y.app
```

Then retry code signing.
If that produces no errors, retry verification.
If all is now well, unmount the writable .dmg and convert it back to read-only:

```
hdiutil eject /dev/disk3
rm -rf ./jlmnt
hdiutil convert julia-x.y.z-osx_writable.dmg -format UDZO -o julia-x.y.z-osx_fixed.dmg
```

Verify that the resulting .dmg is in fact fixed by double clicking it.
If everything looks good, eject it then drop the `_fixed` suffix from the name.
And that's it!

### Windows

Signing must be performed manually on Windows.
First obtain the Windows 10 SDK, which contains the necessary signing utilities, from
the Microsoft website.
We need the `SignTool` utility which should have been installed somewhere like
`C:\Program Files (x86)\Windows Kits\10\App Certification Kit`.
Grab the Windows certificate files from CodeSigning on `juliasecure` and put them
in the same directory as the executables.
Open a Windows CMD window, `cd` to where all the files are, and run

```
set PATH=%PATH%;C:\Program Files (x86)\Windows Kits\10\App Certification Kit;
signtool sign /f julia-windows-code-sign_2017.p12 /p "PASSWORD" ^
   /t http://timestamp.verisign.com/scripts/timstamp.dll ^
   /v julia-x.y.z-win32.exe
```

Note that `^` is a line continuation character in Windows CMD and `PASSWORD` is a
placeholder for the password for this certificate.
As usual, contact Elliot or Alex for passwords.
If there are no errors, we're all good!

## Uploading binaries

Now that everything is signed, we need to upload the binaries to AWS.
You can use a program like Cyberduck or the `aws` command line utility.
The binaries should go in the `julialang2` bucket in the appropriate folders.
For example, Linux x86-64 goes in `julialang2/bin/linux/x.y`.
Be sure to delete the current `julia-x.y-latest-linux-<arch>.tar.gz` file and replace
it with a duplicate of `julia-x.y.z-linux-<arch>.tar.gz`.

We also need to upload the checksums for everything we've built, including the source
tarballs and all release binaries.
This is simple:

```
shasum -a 256 julia-x.y.z* | grep -v -e sha256 -e md5 -e asc > julia-x.y.z.sha256
md5sum julia-x.y.z* | grep -v -e sha256 -e md5 -e asc > julia-x.y.z.md5
```

Note that if you're running those commands on macOS, you'll get very slightly different
output, which can be reformatted by looking at an existing file.
Mac users will also need to use `md5 -r` instead of `md5sum`.
Upload the .md5 and .sha256 files to `julialang2/bin/checksums` on AWS.

Ensure that the permissions on AWS for all uploaded files are set to "Everyone: READ."

For each file we've uploaded, we need to purge the Fastly cache so that the links on
the website point to the updated files.
As an example:

```
curl -X PURGE https://julialang-s3.julialang.org/bin/checksums/julia-x.y.z.sha256
```

Sometimes this isn't necessary but it's good to do anyway.
