# Packages

Julia has a built-in package manager for installing add-on functionality written in Julia. It
can also install external libraries using your operating system's standard system for doing so,
or by compiling from source. The list of registered Julia packages can be found at [http://pkg.julialang.org](http://pkg.julialang.org).
All package manager commands are found in the `Pkg` module, included in Julia's `Base`
install.

First we'll go over the mechanics of the `Pkg` family of commands and then we'll provide some
guidance on how to get your package registered. Be sure to read the section below on package naming
conventions, tagging versions and the importance of a `REQUIRE` file for when you're ready to
add your code to the curated METADATA repository.

## Package Status

The [`Pkg.status()`](@ref) function prints out a summary of the state of packages you have installed.
Initially, you'll have no packages installed:

```julia
julia> Pkg.status()
INFO: Initializing package repository /Users/stefan/.julia/v0.6
INFO: Cloning METADATA from git://github.com/JuliaLang/METADATA.jl
No packages installed.
```

Your package directory is automatically initialized the first time you run a `Pkg` command
that expects it to exist – which includes  [`Pkg.status()`](@ref). Here's an example non-trivial
set of required and additional packages:

```julia
julia> Pkg.status()
Required packages:
 - Distributions                 0.2.8
 - UTF16                         0.2.0
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.6
```

These packages are all on registered versions, managed by `Pkg`. Packages can be in more
complicated states, indicated by annotations to the right of the installed package version; we
will explain these states and annotations as we encounter them. For programmatic usage, [`Pkg.installed()`](@ref)
returns a dictionary, mapping installed package names to the version of that package which is
installed:

```julia
julia> Pkg.installed()
Dict{String,VersionNumber} with 4 entries:
"Distributions"     => v"0.2.8"
"Stats"             => v"0.2.6"
"UTF16"             => v"0.2.0"
"NumericExtensions" => v"0.2.17"
```

## Adding and Removing Packages

Julia's package manager is a little unusual in that it is declarative rather than imperative.
This means that you tell it what you want and it figures out what versions to install (or remove)
to satisfy those requirements optimally – and minimally. So rather than installing a package,
you just add it to the list of requirements and then "resolve" what needs to be installed. In
particular, this means that if some package had been installed because it was needed by a previous
version of something you wanted, and a newer version doesn't have that requirement anymore, updating
will actually remove that package.

Your package requirements are in the file `~/.julia/v0.6/REQUIRE`. You can edit this file by hand
and then call [`Pkg.resolve()`](@ref) to install, upgrade or remove packages to optimally satisfy
the requirements, or you can do [`Pkg.edit()`](@ref), which will open `REQUIRE` in your editor
(configured via the `EDITOR` or `VISUAL` environment variables), and then automatically call
[`Pkg.resolve()`](@ref) afterwards if necessary. If you only want to add or remove the requirement
for a single package, you can also use the non-interactive [`Pkg.add()`](@ref) and [`Pkg.rm()`](@ref)
commands, which add or remove a single requirement to `REQUIRE` and then call [`Pkg.resolve()`](@ref).

You can add a package to the list of requirements with the [`Pkg.add()`](@ref) function, and the
package and all the packages that it depends on will be installed:

```julia
julia> Pkg.status()
No packages installed.

julia> Pkg.add("Distributions")
INFO: Cloning cache of Distributions from git://github.com/JuliaStats/Distributions.jl.git
INFO: Cloning cache of NumericExtensions from git://github.com/lindahua/NumericExtensions.jl.git
INFO: Cloning cache of Stats from git://github.com/JuliaStats/Stats.jl.git
INFO: Installing Distributions v0.2.7
INFO: Installing NumericExtensions v0.2.17
INFO: Installing Stats v0.2.6
INFO: REQUIRE updated.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.7
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.6
```

What this is doing is first adding `Distributions` to your `~/.julia/v0.6/REQUIRE` file:

```
$ cat ~/.julia/v0.6/REQUIRE
Distributions
```

It then runs [`Pkg.resolve()`](@ref) using these new requirements, which leads to the conclusion
that the `Distributions` package should be installed since it is required but not installed. As
stated before, you can accomplish the same thing by editing your `~/.julia/v0.6/REQUIRE` file
by hand and then running [`Pkg.resolve()`](@ref) yourself:

```julia
$ echo UTF16 >> ~/.julia/v0.6/REQUIRE

julia> Pkg.resolve()
INFO: Cloning cache of UTF16 from git://github.com/nolta/UTF16.jl.git
INFO: Installing UTF16 v0.2.0

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.7
 - UTF16                         0.2.0
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.6
```

This is functionally equivalent to calling [`Pkg.add("UTF16")`](@ref), except that [`Pkg.add()`](@ref)
doesn't change `REQUIRE` until *after* installation has completed, so if there are problems,
`REQUIRE` will be left as it was before calling [`Pkg.add()`](@ref). The format of the `REQUIRE`
file is described in [Requirements Specification](@ref); it allows, among other things, requiring
specific ranges of versions of packages.

When you decide that you don't want to have a package around any more, you can use [`Pkg.rm()`](@ref)
to remove the requirement for it from the `REQUIRE` file:

```julia
julia> Pkg.rm("Distributions")
INFO: Removing Distributions v0.2.7
INFO: Removing Stats v0.2.6
INFO: Removing NumericExtensions v0.2.17
INFO: REQUIRE updated.

julia> Pkg.status()
Required packages:
 - UTF16                         0.2.0

julia> Pkg.rm("UTF16")
INFO: Removing UTF16 v0.2.0
INFO: REQUIRE updated.

julia> Pkg.status()
No packages installed.
```

Once again, this is equivalent to editing the `REQUIRE` file to remove the line with each package
name on it then running [`Pkg.resolve()`](@ref) to update the set of installed packages to match.
While [`Pkg.add()`](@ref) and [`Pkg.rm()`](@ref) are convenient for adding and removing requirements
for a single package, when you want to add or remove multiple packages, you can call [`Pkg.edit()`](@ref)
to manually change the contents of `REQUIRE` and then update your packages accordingly. [`Pkg.edit()`](@ref)
does not roll back the contents of `REQUIRE` if [`Pkg.resolve()`](@ref) fails – rather, you
have to run [`Pkg.edit()`](@ref) again to fix the files contents yourself.

Because the package manager uses libgit2 internally to manage the package git repositories, users
may run into protocol issues (if behind a firewall, for example), when running [`Pkg.add()`](@ref).
By default, all GitHub-hosted packages wil be accessed via 'https'; this default can be modified
by calling [`Pkg.setprotocol!()`](@ref).  The following command can be run from the command line
in order to tell git to use 'https' instead of the 'git' protocol when cloning all repositories,
wherever they are hosted:

```
git config --global url."https://".insteadOf git://
```

However, this change will be system-wide and thus the use of [`Pkg.setprotocol!()`](@ref) is preferable.

## Offline Installation of Packages

For machines with no Internet connection, packages may be installed by copying the package root
directory (given by [`Pkg.dir()`](@ref)) from a machine with the same operating system and environment.

[`Pkg.add()`](@ref) does the following within the package root directory:

1. Adds the name of the package to `REQUIRE`.
2. Downloads the package to `.cache`, then copies the package to the package root directory.
3. Recursively performs step 2 against all the packages listed in the package's `REQUIRE` file.
4. Runs [`Pkg.build()`](@ref)

!!! warning
    Copying installed packages from a different machine is brittle for packages requiring binary external
    dependencies. Such packages may break due to differences in operating system versions, build environments,
    and/or absolute path dependencies.

## Installing Unregistered Packages

Julia packages are simply git repositories, clonable via any of the [protocols](https://www.kernel.org/pub/software/scm/git/docs/git-clone.html#URLS)
that git supports, and containing Julia code that follows certain layout conventions. Official
Julia packages are registered in the [METADATA.jl](https://github.com/JuliaLang/METADATA.jl) repository,
available at a well-known location [^1]. The [`Pkg.add()`](@ref) and [`Pkg.rm()`](@ref) commands
in the previous section interact with registered packages, but the package manager can install
and work with unregistered packages too. To install an unregistered package, use [`Pkg.clone(url)`](@ref),
where `url` is a git URL from which the package can be cloned:

```julia
julia> Pkg.clone("git://example.com/path/to/Package.jl.git")
INFO: Cloning Package from git://example.com/path/to/Package.jl.git
Cloning into 'Package'...
remote: Counting objects: 22, done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 22 (delta 8), reused 22 (delta 8)
Receiving objects: 100% (22/22), 2.64 KiB, done.
Resolving deltas: 100% (8/8), done.
```

By convention, Julia repository names end with `.jl` (the additional `.git` indicates a "bare"
git repository), which keeps them from colliding with repositories for other languages, and also
makes Julia packages easy to find in search engines. When packages are installed in your `.julia/v0.6`
directory, however, the extension is redundant so we leave it off.

If unregistered packages contain a `REQUIRE` file at the top of their source tree, that file will
be used to determine which registered packages the unregistered package depends on, and they will
automatically be installed. Unregistered packages participate in the same version resolution logic
as registered packages, so installed package versions will be adjusted as necessary to satisfy
the requirements of both registered and unregistered packages.

[^1]:
    The official set of packages is at [https://github.com/JuliaLang/METADATA.jl](https://github.com/JuliaLang/METADATA.jl),
    but individuals and organizations can easily use a different metadata repository. This allows
    control which packages are available for automatic installation. One can allow only audited and
    approved package versions, and make private packages or forks available. See [Custom METADATA Repository](@ref)
    for details.

## Updating Packages

When package developers publish new registered versions of packages that you're using, you will,
of course, want the new shiny versions. To get the latest and greatest versions of all your packages,
just do [`Pkg.update()`](@ref):

```julia
julia> Pkg.update()
INFO: Updating METADATA...
INFO: Computing changes...
INFO: Upgrading Distributions: v0.2.8 => v0.2.10
INFO: Upgrading Stats: v0.2.7 => v0.2.8
```

The first step of updating packages is to pull new changes to `~/.julia/v0.6/METADATA` and see
if any new registered package versions have been published. After this, [`Pkg.update()`](@ref)
attempts to update packages that are checked out on a branch and not dirty (i.e. no changes have
been made to files tracked by git) by pulling changes from the package's upstream repository.
Upstream changes will only be applied if no merging or rebasing is necessary – i.e. if the branch
can be ["fast-forwarded"](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging).
If the branch cannot be fast-forwarded, it is assumed that you're working on it and will update
the repository yourself.

Finally, the update process recomputes an optimal set of package versions to have installed to
satisfy your top-level requirements and the requirements of "fixed" packages. A package is considered
fixed if it is one of the following:

1. **Unregistered:** the package is not in `METADATA` – you installed it with [`Pkg.clone()`](@ref).
2. **Checked out:** the package repo is on a development branch.
3. **Dirty:** changes have been made to files in the repo.

If any of these are the case, the package manager cannot freely change the installed version of
the package, so its requirements must be satisfied by whatever other package versions it picks.
The combination of top-level requirements in `~/.julia/v0.6/REQUIRE` and the requirement of fixed
packages are used to determine what should be installed.

You can also update only a subset of the installed packages, by providing arguments to the [`Pkg.update`](@ref)
function. In that case, only the packages provided as arguments and their dependencies will be
updated:

```julia
julia> Pkg.update("Example")
INFO: Updating METADATA...
INFO: Computing changes...
INFO: Upgrading Example: v0.4.0 => 0.4.1
```

This partial update process still computes the new set of package versions according to top-level
requirements and "fixed" packages, but it additionally considers all other packages except those
explicitly provided, and their dependencies, as fixed.

## Checkout, Pin and Free

You may want to use the `master` version of a package rather than one of its registered versions.
There might be fixes or functionality on master that you need that aren't yet published in any
registered versions, or you may be a developer of the package and need to make changes on `master`
or some other development branch. In such cases, you can do [`Pkg.checkout(pkg)`](@ref) to checkout
the `master` branch of `pkg` or [`Pkg.checkout(pkg,branch)`](@ref) to checkout some other branch:

```julia
julia> Pkg.add("Distributions")
INFO: Installing Distributions v0.2.9
INFO: Installing NumericExtensions v0.2.17
INFO: Installing Stats v0.2.7
INFO: REQUIRE updated.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.7

julia> Pkg.checkout("Distributions")
INFO: Checking out Distributions master...
INFO: No packages to install, update or remove.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9+             master
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.7
```

Immediately after installing `Distributions` with [`Pkg.add()`](@ref) it is on the current most
recent registered version – `0.2.9` at the time of writing this. Then after running [`Pkg.checkout("Distributions")`](@ref),
you can see from the output of [`Pkg.status()`](@ref) that `Distributions` is on an unregistered
version greater than `0.2.9`, indicated by the "pseudo-version" number `0.2.9+`.

When you checkout an unregistered version of a package, the copy of the `REQUIRE` file in the
package repo takes precedence over any requirements registered in `METADATA`, so it is important
that developers keep this file accurate and up-to-date, reflecting the actual requirements of
the current version of the package. If the `REQUIRE` file in the package repo is incorrect or
missing, dependencies may be removed when the package is checked out. This file is also used to
populate newly published versions of the package if you use the API that `Pkg` provides
for this (described below).

When you decide that you no longer want to have a package checked out on a branch, you can "free"
it back to the control of the package manager with [`Pkg.free(pkg)`](@ref):

```julia
julia> Pkg.free("Distributions")
INFO: Freeing Distributions...
INFO: No packages to install, update or remove.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.7
```

After this, since the package is on a registered version and not on a branch, its version will
be updated as new registered versions of the package are published.

If you want to pin a package at a specific version so that calling [`Pkg.update()`](@ref) won't
change the version the package is on, you can use the [`Pkg.pin()`](@ref) function:

```julia
julia> Pkg.pin("Stats")
INFO: Creating Stats branch pinned.47c198b1.tmp

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.7              pinned.47c198b1.tmp
```

After this, the `Stats` package will remain pinned at version `0.2.7` – or more specifically,
at commit `47c198b1`, but since versions are permanently associated a given git hash, this is
the same thing. [`Pkg.pin()`](@ref) works by creating a throw-away branch for the commit you want
to pin the package at and then checking that branch out. By default, it pins a package at the
current commit, but you can choose a different version by passing a second argument:

```julia
julia> Pkg.pin("Stats",v"0.2.5")
INFO: Creating Stats branch pinned.1fd0983b.tmp
INFO: No packages to install, update or remove.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.5              pinned.1fd0983b.tmp
```

Now the `Stats` package is pinned at commit `1fd0983b`, which corresponds to version `0.2.5`.
When you decide to "unpin" a package and let the package manager update it again, you can use
[`Pkg.free()`](@ref) like you would to move off of any branch:

```julia
julia> Pkg.free("Stats")
INFO: Freeing Stats...
INFO: No packages to install, update or remove.

julia> Pkg.status()
Required packages:
 - Distributions                 0.2.9
Additional packages:
 - NumericExtensions             0.2.17
 - Stats                         0.2.7
```

After this, the `Stats` package is managed by the package manager again, and future calls to
[`Pkg.update()`](@ref) will upgrade it to newer versions when they are published. The throw-away
`pinned.1fd0983b.tmp` branch remains in your local `Stats` repo, but since git branches are extremely
lightweight, this doesn't really matter; if you feel like cleaning them up, you can go into the
repo and delete those branches [^2].

[^2]:
    Packages that aren't on branches will also be marked as dirty if you make changes in the repo,
    but that's a less common thing to do.

## Custom METADATA Repository

By default, Julia assumes you will be using the [official METADATA.jl](https://github.com/JuliaLang/METADATA.jl)
repository for downloading and installing packages. You can also provide a different metadata
repository location. A common approach is to keep your `metadata-v2` branch up to date with the
Julia official branch and add another branch with your custom packages. You can initialize your
local metadata repository using that custom location and branch and then periodically rebase your
custom branch with the official `metadata-v2` branch. In order to use a custom repository and
branch, issue the following command:

```julia
julia> Pkg.init("https://me.example.com/METADATA.jl.git", "branch")
```

The branch argument is optional and defaults to `metadata-v2`. Once initialized, a file named
`META_BRANCH` in your `~/.julia/vX.Y/` path will track the branch that your METADATA repository
was initialized with. If you want to change branches, you will need to either modify the `META_BRANCH`
file directly (be careful!) or remove the `vX.Y` directory and re-initialize your METADATA repository
using the `Pkg.init` command.

# Package Development

Julia's package manager is designed so that when you have a package installed, you are already
in a position to look at its source code and full development history. You are also able to make
changes to packages, commit them using git, and easily contribute fixes and enhancements upstream.
Similarly, the system is designed so that if you want to create a new package, the simplest way
to do so is within the infrastructure provided by the package manager.

## [Initial Setup](@id man-initial-setup)

Since packages are git repositories, before doing any package development you should setup the
following standard global git configuration settings:

```
$ git config --global user.name "FULL NAME"
$ git config --global user.email "EMAIL"
```

where `FULL NAME` is your actual full name (spaces are allowed between the double quotes) and
`EMAIL` is your actual email address. Although it isn't necessary to use [GitHub](https://github.com/)
to create or publish Julia packages, most Julia packages as of writing this are hosted on GitHub
and the package manager knows how to format origin URLs correctly and otherwise work with the
service smoothly. We recommend that you create a [free account](https://github.com/join) on GitHub
and then do:

```
$ git config --global github.user "USERNAME"
```

where `USERNAME` is your actual GitHub user name. Once you do this, the package manager knows
your GitHub user name and can configure things accordingly. You should also [upload](https://github.com/login?return_to=https%3A%2F%2Fgithub.com%2Fsettings%2Fssh)
your public SSH key to GitHub and set up an [SSH agent](https://linux.die.net/man/1/ssh-agent)
on your development machine so that you can push changes with minimal hassle. In the future, we
will make this system extensible and support other common git hosting options like [BitBucket](https://bitbucket.org)
and allow developers to choose their favorite. Since the package development functions has been
moved to the [PkgDev](https://github.com/JuliaLang/PkgDev.jl) package, you need to run `Pkg.add("PkgDev"); import PkgDev`
to access the functions starting with `PkgDev.` in the document below.

## Making changes to an existing package

### Documentation changes

If you want to improve the online documentation of a package, the easiest approach (at least for
small changes) is to use GitHub's online editing functionality. First, navigate to the repository's
GitHub "home page," find the file (e.g., `README.md`) within the repository's folder structure,
and click on it. You'll see the contents displayed, along with a small "pencil" icon in the upper
right hand corner. Clicking that icon opens the file in edit mode. Make your changes, write a
brief summary describing the changes you want to make (this is your *commit message*), and then
hit "Propose file change."  Your changes will be submitted for consideration by the package owner(s)
and collaborators.

For larger documentation changes--and especially ones that you expect to have to update in response
to feedback--you might find it easier to use the procedure for code changes described below.

### Code changes

#### Executive summary

Here we assume you've already set up git on your local machine and have a GitHub account (see
above).  Let's imagine you're fixing a bug in the Images package:

```
Pkg.checkout("Images")           # check out the master branch
<here, make sure your bug is still a bug and hasn't been fixed already>
cd(Pkg.dir("Images"))
;git checkout -b myfixes         # create a branch for your changes
<edit code>                      # be sure to add a test for your bug
Pkg.test("Images")               # make sure everything works now
;git commit -a -m "Fix foo by calling bar"   # write a descriptive message
using PkgDev
PkgDev.submit("Images")
```

The last line will present you with a link to submit a pull request to incorporate your changes.

#### Detailed description

If you want to fix a bug or add new functionality, you want to be able to test your changes before
you submit them for consideration. You also need to have an easy way to update your proposal in
response to the package owner's feedback. Consequently, in this case the strategy is to work locally
on your own machine; once you are satisfied with your changes, you submit them for consideration.
 This process is called a *pull request* because you are asking to "pull" your changes into the
project's main repository. Because the online repository can't see the code on your private machine,
you first *push* your changes to a publicly-visible location, your own online *fork* of the package
(hosted on your own personal GitHub account).

Let's assume you already have the `Foo` package installed.  In the description below, anything
starting with `Pkg.` or `PkgDev.` is meant to be typed at the Julia prompt; anything starting
with `git` is meant to be typed in [julia's shell mode](@ref man-shell-mode) (or using the shell that comes with
your operating system).  Within Julia, you can combine these two modes:

```julia
julia> cd(Pkg.dir("Foo"))          # go to Foo's folder

shell> git command arguments...    # command will apply to Foo
```

Now suppose you're ready to make some changes to `Foo`.  While there are several possible approaches,
here is one that is widely used:

  * From the Julia prompt, type [`Pkg.checkout("Foo")`](@ref). This ensures you're running the latest
    code (the `master` branch), rather than just whatever "official release" version you have installed.
    (If you're planning to fix a bug, at this point it's a good idea to check again whether the bug
    has already been fixed by someone else. If it has, you can request that a new official release
    be tagged so that the fix gets distributed to the rest of the community.) If you receive an error
    `Foo is dirty, bailing`, see [Dirty packages](@ref) below.
  * Create a branch for your changes: navigate to the package folder (the one that Julia reports from
    [`Pkg.dir("Foo")`](@ref)) and (in shell mode) create a new branch using `git checkout -b <newbranch>`,
    where `<newbranch>` might be some descriptive name (e.g., `fixbar`). By creating a branch, you
    ensure that you can easily go back and forth between your new work and the current `master` branch
    (see [https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell)).

    If you forget to do this step until after you've already made some changes, don't worry: see
    [more detail about branching](@ref man-branch-post-hoc) below.
  * Make your changes. Whether it's fixing a bug or adding new functionality, in most cases your change
    should include updates to both the `src/` and `test/` folders.  If you're fixing a bug, add your
    minimal example demonstrating the bug (on the current code) to the test suite; by contributing
    a test for the bug, you ensure that the bug won't accidentally reappear at some later time due
    to other changes.  If you're adding new functionality, creating tests demonstrates to the package
    owner that you've made sure your code works as intended.
  * Run the package's tests and make sure they pass. There are several ways to run the tests:

      * From Julia, run [`Pkg.test("Foo")`](@ref): this will run your tests in a separate (new) `julia`
        process.
      * From Julia, `include("runtests.jl")` from the package's `test/` folder (it's possible the file
        has a different name, look for one that runs all the tests): this allows you to run the tests
        repeatedly in the same session without reloading all the package code; for packages that take
        a while to load, this can be much faster. With this approach, you do have to do some extra work
        to make [changes in the package code](@ref man-workflow-tips).
      * From the shell, run `julia ../test/runtests.jl` from within the package's `src/` folder.
  * Commit your changes: see [https://git-scm.com/book/en/v2/Git-Basics-Recording-Changes-to-the-Repository](https://git-scm.com/book/en/v2/Git-Basics-Recording-Changes-to-the-Repository).
  * Submit your changes: From the Julia prompt, type `PkgDev.submit("Foo")`. This will push your changes
    to your GitHub fork, creating it if it doesn't already exist. (If you encounter an error, [make sure you've set up your SSH keys](@ref man-initial-setup).)
    Julia will then give you a hyperlink; open that link, edit the message, and then click "submit."
    At that point, the package owner will be notified of your changes and may initiate discussion.
    (If you are comfortable with git, you can also do these steps manually from the shell.)
  * The package owner may suggest additional improvements. To respond to those suggestions, you can
    easily update the pull request (this only works for changes that have not already been merged;
    for merged pull requests, make new changes by starting a new branch):

      * If you've changed branches in the meantime, make sure you go back to the same branch with `git checkout fixbar`
        (from shell mode) or [`Pkg.checkout("Foo", "fixbar")`](@ref) (from the Julia prompt).
      * As above, make your changes, run the tests, and commit your changes.
      * From the shell, type `git push`.  This will add your new commit(s) to the same pull request; you
        should see them appear automatically on the page holding the discussion of your pull request.

    One potential type of change the owner may request is that you squash your commits.  See [Squashing](@ref man-squashing-and-rebasing)
    below.

### Dirty packages

If you can't change branches because the package manager complains that your package is dirty,
it means you have some changes that have not been committed. From the shell, use `git diff` to
see what these changes are; you can either discard them (`git checkout changedfile.jl`) or commit
them before switching branches.  If you can't easily resolve the problems manually, as a last
resort you can delete the entire `"Foo"` folder and reinstall a fresh copy with [`Pkg.add("Foo")`](@ref).
Naturally, this deletes any changes you've made.

### [Making a branch *post hoc*](@id man-branch-post-hoc)

Especially for newcomers to git, one often forgets to create a new branch until after some changes
have already been made.  If you haven't yet staged or committed your changes, you can create a
new branch with `git checkout -b <newbranch>` just as usual--git will kindly show you that some
files have been modified and create the new branch for you.  *Your changes have not yet been committed to this new branch*,
so the normal work rules still apply.

However, if you've already made a commit to `master` but wish to go back to the official `master`
(called `origin/master`), use the following procedure:

  * Create a new branch. This branch will hold your changes.
  * Make sure everything is committed to this branch.
  * `git checkout master`. If this fails, *do not* proceed further until you have resolved the problems,
    or you may lose your changes.
  * *Reset*`master` (your current branch) back to an earlier state with `git reset --hard origin/master`
    (see [https://git-scm.com/blog/2011/07/11/reset.html](https://git-scm.com/blog/2011/07/11/reset.html)).

This requires a bit more familiarity with git, so it's much better to get in the habit of creating
a branch at the outset.

### [Squashing and rebasing](@id man-squashing-and-rebasing)

Depending on the tastes of the package owner (s)he may ask you to "squash" your commits. This
is especially likely if your change is quite simple but your commit history looks like this:

```
WIP: add new 1-line whizbang function (currently breaks package)
Finish whizbang function
Fix typo in variable name
Oops, don't forget to supply default argument
Split into two 1-line functions
Rats, forgot to export the second function
...
```

This gets into the territory of more advanced git usage, and you're encouraged to do some reading
([https://git-scm.com/book/en/v2/Git-Branching-Rebasing](https://git-scm.com/book/en/v2/Git-Branching-Rebasing)).
 However, a brief summary of the procedure is as follows:

  * To protect yourself from error, start from your `fixbar` branch and create a new branch with
    `git checkout -b fixbar_backup`.  Since you started from `fixbar`, this will be a copy. Now go
    back to the one you intend to modify with `git checkout fixbar`.
  * From the shell, type `git rebase -i origin/master`.
  * To combine commits, change `pick` to `squash` (for additional options, consult other sources).
    Save the file and close the editor window.
  * Edit the combined commit message.

If the rebase goes badly, you can go back to the beginning to try again like this:

```
git checkout fixbar
git reset --hard fixbar_backup
```

Now let's assume you've rebased successfully. Since your `fixbar` repository has now diverged
from the one in your GitHub fork, you're going to have to do a *force push*:

  * To make it easy to refer to your GitHub fork, create a "handle" for it with `git remote add myfork https://github.com/myaccount/Foo.jl.git`,
    where the URL comes from the "clone URL" on your GitHub fork's page.
  * Force-push to your fork with `git push myfork +fixbar`. The `+` indicates that this should replace
    the `fixbar` branch found at `myfork`.

## Creating a new Package

### REQUIRE speaks for itself

You should have a `REQUIRE` file in your package repository, with a bare minimum directive of
what Julia version you expect your users to be running for the package to work. Putting a floor
on what Julia version your package supports is done by simply adding `julia 0.x` in this file.
While this line is partly informational, it also has the consequence of whether `Pkg.update()`
will update code found in `.julia` version directories. It will not update code found in version
directories beneath the floor of what's specified in your `REQUIRE`.

As the development version `0.y` matures, you may find yourself using it more frequently, and
wanting your package to support it. Be warned, the development branch of Julia is the land of
breakage, and you can expect things to break. When you go about fixing whatever broke your package
in the development `0.y` branch, you will likely find that you just broke your package on the
stable version.

There is a mechanism found in the [Compat](https://github.com/JuliaLang/Compat.jl) package that
will enable you to support both the stable version and breaking changes found in the development
version. Should you decide to use this solution, you will need to add `Compat` to your `REQUIRE`
file. In this case, you will still have `julia 0.x` in your `REQUIRE`. The `x` is the floor version
of what your package supports.

You might also have no interest in supporting the development version of Julia. Just as you can
add a floor to the version you expect your users to be on, you can set an upper bound. In this
case, you would put `julia 0.x 0.y-` in your `REQUIRE` file.  The `-` at the end of the version
number means pre-release versions of that specific version from the very first commit. By setting
it as the ceiling, you mean the code supports everything up to but not including the ceiling version.

Another scenario is that you are writing the bulk of the code for your package with Julia `0.y`
and do not want to support the current stable version of Julia. If you choose to do this, simply
add `julia 0.y-` to your `REQUIRE`. Just remember to change the `julia 0.y-` to `julia 0.y` in
your `REQUIRE` file once `0.y` is officially released. If you don't edit the dash cruft you are
suggesting that you support both the development and stable versions of the same version number!
That would be madness.  See the [Requirements Specification](@ref) for the full format of `REQUIRE`.

Lastly, in many cases you may need extra packages for testing. Additional packages which
are only required for tests should be specified in the `test/REQUIRE` file. This `REQUIRE`
file has the same specification as the standard `REQUIRE` file.

### Guidelines for naming a package

Package names should be sensible to most Julia users, *even to those who are not domain experts*.
When you submit your package to METADATA, you can expect a little back and forth about the package
name with collaborators, especially if it's ambiguous or can be confused with something other
than what it is. During this bike-shedding, it's not uncommon to get a range of *different* name
suggestions. These are only suggestions though, with the intent being to keep a tidy namespace
in the curated METADATA repository. Since this repository belongs to the entire community, there
will likely be a few collaborators who care about your package name. Here are some guidelines
to follow in naming your package:

1. Avoid jargon. In particular, avoid acronyms unless there is minimal possibility of confusion.

     * It's ok to say `USA` if you're talking about the USA.
     * It's not ok to say `PMA`, even if you're talking about positive mental attitude.
2. Avoid using `Julia` in your package name.

     * It is usually clear from context and to your users that the package is a Julia package.
     * Having Julia in the name can imply that the package is connected to, or endorsed by, contributors
       to the Julia language itself.
3. Packages that provide most of their functionality in association with a new type should have pluralized
   names.

     * `DataFrames` provides the `DataFrame` type.
     * `BloomFilters` provides the `BloomFilter` type.
     * In contrast, `JuliaParser` provides no new type, but instead new functionality in the `JuliaParser.parse()`
       function.
4. Err on the side of clarity, even if clarity seems long-winded to you.

     * `RandomMatrices` is a less ambiguous name than `RndMat` or `RMT`, even though the latter are shorter.
5. A less systematic name may suit a package that implements one of several possible approaches to
   its domain.

     * Julia does not have a single comprehensive plotting package. Instead, `Gadfly`, `PyPlot`, `Winston`
       and other packages each implement a unique approach based on a particular design philosophy.
     * In contrast, `SortingAlgorithms` provides a consistent interface to use many well-established
       sorting algorithms.
6. Packages that wrap external libraries or programs should be named after those libraries or programs.

     * `CPLEX.jl` wraps the `CPLEX` library, which can be identified easily in a web search.
     * `MATLAB.jl` provides an interface to call the MATLAB engine from within Julia.

### Generating the package

Suppose you want to create a new Julia package called `FooBar`.  To get started, do `PkgDev.generate(pkg,license)`
where `pkg` is the new package name and `license` is the name of a license that the package generator
knows about:

```julia
julia> PkgDev.generate("FooBar","MIT")
INFO: Initializing FooBar repo: /Users/stefan/.julia/v0.6/FooBar
INFO: Origin: git://github.com/StefanKarpinski/FooBar.jl.git
INFO: Generating LICENSE.md
INFO: Generating README.md
INFO: Generating src/FooBar.jl
INFO: Generating test/runtests.jl
INFO: Generating REQUIRE
INFO: Generating .travis.yml
INFO: Generating appveyor.yml
INFO: Generating .gitignore
INFO: Committing FooBar generated files
```

This creates the directory `~/.julia/v0.6/FooBar`, initializes it as a git repository, generates
a bunch of files that all packages should have, and commits them to the repository:

```
$ cd ~/.julia/v0.6/FooBar && git show --stat

commit 84b8e266dae6de30ab9703150b3bf771ec7b6285
Author: Stefan Karpinski <stefan@karpinski.org>
Date:   Wed Oct 16 17:57:58 2013 -0400

    FooBar.jl generated files.

        license: MIT
        authors: Stefan Karpinski
        years:   2013
        user:    StefanKarpinski

    Julia Version 0.3.0-prerelease+3217 [5fcfb13*]

 .gitignore       |  2 ++
 .travis.yml      | 13 +++++++++++++
 LICENSE.md       | 22 +++++++++++++++++++++++
 README.md        |  3 +++
 REQUIRE          |  1 +
 appveyor.yml     | 34 ++++++++++++++++++++++++++++++++++
 src/FooBar.jl    |  5 +++++
 test/runtests.jl |  5 +++++
 8 files changed, 85 insertions(+)
```

At the moment, the package manager knows about the MIT "Expat" License, indicated by `"MIT"`,
the Simplified BSD License, indicated by `"BSD"`, and version 2.0 of the Apache Software License,
indicated by `"ASL"`.  If you want to use a different license, you can ask us to add it to the
package generator, or just pick one of these three and then modify the `~/.julia/v0.6/PACKAGE/LICENSE.md`
file after it has been generated.

If you created a GitHub account and configured git to know about it, `PkgDev.generate()` will
set an appropriate origin URL for you.  It will also automatically generate a `.travis.yml` file
for using the [Travis](https://travis-ci.org) automated testing service, and an `appveyor.yml`
file for using [AppVeyor](https://www.appveyor.com).  You will have to enable testing on the Travis
and AppVeyor websites for your package repository, but once you've done that, it will already
have working tests. Of course, all the default testing does is verify that `using FooBar` in Julia
works.

### Loading Static Non-Julia Files

If your package code needs to load static files which are not Julia code, e.g. an external library
or data files, and are located within the package directory, use the `@__DIR__` macro to determine
the directory of the current source file. For example if `FooBar/src/FooBar.jl` needs to load
`FooBar/data/foo.csv`, use the following code:

```julia
datapath = joinpath(@__DIR__, "..", "data")
foo = readcsv(joinpath(datapath, "foo.csv"))
```

### Making Your Package Available

Once you've made some commits and you're happy with how `FooBar` is working, you may want to get
some other people to try it out.  First you'll need to create the remote repository and push your
code to it; we don't yet automatically do this for you, but we will in the future and it's not
too hard to figure out [^3].  Once you've done this, letting people try out your code is as simple
as sending them the URL of the published repo – in this case:

```
git://github.com/StefanKarpinski/FooBar.jl.git
```

For your package, it will be your GitHub user name and the name of your package, but you get the
idea. People you send this URL to can use [`Pkg.clone()`](@ref) to install the package and try
it out:

```julia
julia> Pkg.clone("git://github.com/StefanKarpinski/FooBar.jl.git")
INFO: Cloning FooBar from git@github.com:StefanKarpinski/FooBar.jl.git
```

[^3]:
    Installing and using GitHub's ["hub" tool](https://github.com/github/hub) is highly recommended.
    It allows you to do things like run `hub create` in the package repo and have it automatically
    created via GitHub's API.

### Tagging and Publishing Your Package

!!! tip
    If you are hosting your package on GitHub, you can use the [attobot integration](https://github.com/attobot/attobot)
    to handle package registration, tagging and publishing.

Once you've decided that `FooBar` is ready to be registered as an official package, you can add
it to your local copy of `METADATA` using `PkgDev.register()`:

```julia
julia> PkgDev.register("FooBar")
INFO: Registering FooBar at git://github.com/StefanKarpinski/FooBar.jl.git
INFO: Committing METADATA for FooBar
```

This creates a commit in the `~/.julia/v0.6/METADATA` repo:

```
$ cd ~/.julia/v0.6/METADATA && git show

commit 9f71f4becb05cadacb983c54a72eed744e5c019d
Author: Stefan Karpinski <stefan@karpinski.org>
Date:   Wed Oct 16 18:46:02 2013 -0400

    Register FooBar

diff --git a/FooBar/url b/FooBar/url
new file mode 100644
index 0000000..30e525e
--- /dev/null
+++ b/FooBar/url
@@ -0,0 +1 @@
+git://github.com/StefanKarpinski/FooBar.jl.git
```

This commit is only locally visible, however.  To make it visible to the Julia community, you
need to merge your local `METADATA` upstream into the official repo.  The `PkgDev.publish()` command
will fork the `METADATA` repository on GitHub, push your changes to your fork, and open a pull
request:

```julia
julia> PkgDev.publish()
INFO: Validating METADATA
INFO: No new package versions to publish
INFO: Submitting METADATA changes
INFO: Forking JuliaLang/METADATA.jl to StefanKarpinski
INFO: Pushing changes as branch pull-request/ef45f54b
INFO: To create a pull-request open:

  https://github.com/StefanKarpinski/METADATA.jl/compare/pull-request/ef45f54b
```

!!! tip
    If `PkgDev.publish()` fails with error:

    ```
    ERROR: key not found: "token"
    ```

    then you may have encountered an issue from using the GitHub API on multiple systems. The solution
    is to delete the "Julia Package Manager" personal access token [from your Github account](https://github.com/login?return_to=https%3A%2F%2Fgithub.com%2Fsettings%2Ftokens)
    and try again.

    Other failures may require you to circumvent `PkgDev.publish()` by [creating a pull request on GitHub](https://help.github.com/articles/creating-a-pull-request).
    See: [Publishing METADATA manually](@ref) below.

Once the package URL for `FooBar` is registered in the official `METADATA` repo, people know where
to clone the package from, but there still aren't any registered versions available. You can tag
and register it with the `PkgDev.tag()` command:

```julia
julia> PkgDev.tag("FooBar")
INFO: Tagging FooBar v0.0.1
INFO: Committing METADATA for FooBar
```

This tags `v0.0.1` in the `FooBar` repo:

```
$ cd ~/.julia/v0.6/FooBar && git tag
v0.0.1
```

It also creates a new version entry in your local `METADATA` repo for `FooBar`:

```
$ cd ~/.julia/v0.6/FooBar && git show
commit de77ee4dc0689b12c5e8b574aef7f70e8b311b0e
Author: Stefan Karpinski <stefan@karpinski.org>
Date:   Wed Oct 16 23:06:18 2013 -0400

    Tag FooBar v0.0.1

diff --git a/FooBar/versions/0.0.1/sha1 b/FooBar/versions/0.0.1/sha1
new file mode 100644
index 0000000..c1cb1c1
--- /dev/null
+++ b/FooBar/versions/0.0.1/sha1
@@ -0,0 +1 @@
+84b8e266dae6de30ab9703150b3bf771ec7b6285
```

The `PkgDev.tag()` command takes an optional second argument that is either an explicit version
number object like `v"0.0.1"` or one of the symbols `:patch`, `:minor` or `:major`.  These increment
the patch, minor or major version number of your package intelligently.

Adding a tagged version of your package will expedite the official registration into METADATA.jl
by collaborators. It is strongly recommended that you complete this process, regardless if your
package is completely ready for an official release.

As a general rule, packages should be tagged `0.0.1` first. Since Julia itself hasn't achieved
`1.0` status, it's best to be conservative in your package's tagged versions.

As with `PkgDev.register()`, these changes to `METADATA` aren't available to anyone else until
they've been included upstream. Again, use the `PkgDev.publish()` command, which first makes sure
that individual package repos have been tagged, pushes them if they haven't already been, and
then opens a pull request to `METADATA`:

```julia
julia> PkgDev.publish()
INFO: Validating METADATA
INFO: Pushing FooBar permanent tags: v0.0.1
INFO: Submitting METADATA changes
INFO: Forking JuliaLang/METADATA.jl to StefanKarpinski
INFO: Pushing changes as branch pull-request/3ef4f5c4
INFO: To create a pull-request open:

  https://github.com/StefanKarpinski/METADATA.jl/compare/pull-request/3ef4f5c4
```

#### Publishing METADATA manually

If `PkgDev.publish()` fails you can follow these instructions to manually publish your package.

By "forking" the main METADATA repository, you can create a personal copy (of METADATA.jl) under
your GitHub account. Once that copy exists, you can push your local changes to your copy (just
like any other GitHub project).

1. go to [https://github.com/login?return_to=https%3A%2F%2Fgithub.com%2FJuliaLang%2FMETADATA.jl%2Ffork](https://github.com/login?return_to=https%3A%2F%2Fgithub.com%2FJuliaLang%2FMETADATA.jl%2Ffork)
and create your own fork.

2. add your fork as a remote repository for the METADATA repository on your local computer (in
the terminal where USERNAME is your github username):

```
cd ~/.julia/v0.6/METADATA
git remote add USERNAME https://github.com/USERNAME/METADATA.jl.git
```

1. push your changes to your fork:

   ```
   git push USERNAME metadata-v2
   ```

4. If all of that works, then go back to the GitHub page for your fork, and click the "pull request"
link.

## Fixing Package Requirements

If you need to fix the registered requirements of an already-published package version, you can
do so just by editing the metadata for that version, which will still have the same commit hash
– the hash associated with a version is permanent:

```
$ cd ~/.julia/v0.6/METADATA/FooBar/versions/0.0.1 && cat requires
julia 0.3-
$ vi requires
```

Since the commit hash stays the same, the contents of the `REQUIRE` file that will be checked
out in the repo will **not** match the requirements in `METADATA` after such a change; this is
unavoidable. When you fix the requirements in `METADATA` for a previous version of a package,
however, you should also fix the `REQUIRE` file in the current version of the package.

## Requirements Specification

The `~/.julia/v0.6/REQUIRE` file, the `REQUIRE` file inside packages, and the `METADATA` package
`requires` files use a simple line-based format to express the ranges of package versions which
need to be installed.  Package `REQUIRE` and `METADATA requires` files should also include the
range of versions of `julia` the package is expected to work with. Additionally, packages can
include a `test/REQUIRE` file to specify additional packages which are only required for testing.

Here's how these files are parsed and interpreted.

  * Everything after a `#` mark is stripped from each line as a comment.
  * If nothing but whitespace is left, the line is ignored.
  * If there are non-whitespace characters remaining, the line is a requirement and the is split on
    whitespace into words.

The simplest possible requirement is just the name of a package name on a line by itself:

```julia
Distributions
```

This requirement is satisfied by any version of the `Distributions` package. The package name
can be followed by zero or more version numbers in ascending order, indicating acceptable intervals
of versions of that package. One version opens an interval, while the next closes it, and the
next opens a new interval, and so on; if an odd number of version numbers are given, then arbitrarily
large versions will satisfy; if an even number of version numbers are given, the last one is an
upper limit on acceptable version numbers. For example, the line:

```
Distributions 0.1
```

is satisfied by any version of `Distributions` greater than or equal to `0.1.0`. Suffixing a version
with `-` allows any pre-release versions as well. For example:

```
Distributions 0.1-
```

is satisfied by pre-release versions such as `0.1-dev` or `0.1-rc1`, or by any version greater
than or equal to `0.1.0`.

This requirement entry:

```
Distributions 0.1 0.2.5
```

is satisfied by versions from `0.1.0` up to, but not including `0.2.5`. If you want to indicate
that any `0.1.x` version will do, you will want to write:

```
Distributions 0.1 0.2-
```

If you want to start accepting versions after `0.2.7`, you can write:

```
Distributions 0.1 0.2- 0.2.7
```

If a requirement line has leading words that begin with `@`, it is a system-dependent requirement.
If your system matches these system conditionals, the requirement is included, if not, the requirement
is ignored. For example:

```julia
@osx Homebrew
```

will require the `Homebrew` package only on systems where the operating system is OS X. The system
conditions that are currently supported are (hierarchically):

  * `@unix`

      * `@linux`
      * `@bsd`

          * `@osx`
  * `@windows`

The `@unix` condition is satisfied on all UNIX systems, including Linux and BSD. Negated system
conditionals are also supported by adding a `!` after the leading `@`. Examples:

```julia
@!windows
@unix @!osx
```

The first condition applies to any system but Windows and the second condition applies to any
UNIX system besides OS X.

Runtime checks for the current version of Julia can be made using the built-in `VERSION` variable,
which is of type `VersionNumber`. Such code is occasionally necessary to keep track of new or
deprecated functionality between various releases of Julia. Examples of runtime checks:

```julia
VERSION < v"0.3-" #exclude all pre-release versions of 0.3

v"0.2-" <= VERSION < v"0.3-" #get all 0.2 versions, including pre-releases, up to the above

v"0.2" <= VERSION < v"0.3-" #To get only stable 0.2 versions (Note v"0.2" == v"0.2.0")

VERSION >= v"0.2.1" #get at least version 0.2.1
```

See the section on [version number literals](@ref man-version-number-literals) for a more complete description.
