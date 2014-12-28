.. _man-packages:

.. currentmodule:: Base

**********
 Packages
**********

Julia has a built-in package manager for installing add-on functionality written in Julia.
It can also install external libraries using your operating system's standard system for doing so, or by compiling from source.
The list of registered Julia packages can be found at `<http://pkg.julialang.org>`_.
All package manager commands are found in the :mod:`Pkg <Base.Pkg>` module, included in Julia's :mod:`Base` install.

Package Status
--------------

The :func:`Pkg.status` function prints out a summary of the state of packages you have installed.
Initially, you'll have no packages installed::

    julia> Pkg.status()
    INFO: Initializing package repository /Users/stefan/.julia/v0.3
    INFO: Cloning METADATA from git://github.com/JuliaLang/METADATA.jl
    No packages installed.

Your package directory is automatically initialized the first time you run a :mod:`Pkg <Base.Pkg>` command that expects it to exist – which includes  :func:`Pkg.status`.
Here's an example non-trivial set of required and additional packages::

    julia> Pkg.status()
    Required packages:
     - Distributions                 0.2.8
     - UTF16                         0.2.0
    Additional packages:
     - NumericExtensions             0.2.17
     - Stats                         0.2.6

These packages are all on registered versions, managed by :mod:`Pkg <Base.Pkg>`.
Packages can be in more complicated states, indicated by annotations to the right of the installed package version; we will explain these states and annotations as we encounter them.
For programmatic usage, :func:`Pkg.installed` returns a dictionary, mapping installed package names to the version of that package which is installed::

    julia> Pkg.installed()
    Dict{ASCIIString,VersionNumber} with 4 entries:
    "Distributions"     => v"0.2.8"
    "Stats"             => v"0.2.6"
    "UTF16"             => v"0.2.0"
    "NumericExtensions" => v"0.2.17"

Adding and Removing Packages
----------------------------

Julia's package manager is a little unusual in that it is declarative rather than imperative.
This means that you tell it what you want and it figures out what versions to install (or remove) to satisfy those requirements optimally – and minimally.
So rather than installing a package, you just add it to the list of requirements and then "resolve" what needs to be installed.
In particular, this means that if some package had been installed because it was needed by a previous version of something you wanted, and a newer version doesn't have that requirement anymore, updating will actually remove that package.

Your package requirements are in the file ``~/.julia/v0.3/REQUIRE``.
You can edit this file by hand and then call :func:`Pkg.resolve` to install, upgrade or remove packages to optimally satisfy the requirements, or you can do :func:`Pkg.edit`, which will open ``REQUIRE`` in your editor (configured via the ``EDITOR`` or ``VISUAL`` environment variables), and then automatically call :func:`Pkg.resolve` afterwards if necessary.
If you only want to add or remove the requirement for a single package, you can also use the non-interactive :func:`Pkg.add` and :func:`Pkg.rm` commands, which add or remove a single requirement to ``REQUIRE`` and then call :func:`Pkg.resolve`.

You can add a package to the list of requirements with the :func:`Pkg.add` function, and the package and all the packages that it depends on will be installed::

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

What this is doing is first adding ``Distributions`` to your ``~/.julia/v0.3/REQUIRE`` file::

    $ cat ~/.julia/v0.3/REQUIRE
    Distributions

It then runs :func:`Pkg.resolve` using these new requirements, which leads to the conclusion that the ``Distributions`` package should be installed since it is required but not installed.
As stated before, you can accomplish the same thing by editing your ``~/.julia/v0.3/REQUIRE`` file by hand and then running :func:`Pkg.resolve` yourself::

    $ echo UTF16 >> ~/.julia/v0.3/REQUIRE

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

This is functionally equivalent to calling :func:`Pkg.add("UTF16") <Pkg.add>`, except that :func:`Pkg.add` doesn't change ``REQUIRE`` until *after* installation has completed, so if there are problems, ``REQUIRE`` will be left as it was before calling :func:`Pkg.add`.
The format of the ``REQUIRE`` file is described in `Requirements Specification`_;
it allows, among other things, requiring specific ranges of versions of packages.

When you decide that you don't want to have a package around any more, you can use :func:`Pkg.rm` to remove the requirement for it from the ``REQUIRE`` file::

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

Once again, this is equivalent to editing the ``REQUIRE`` file to remove the line with each package name on it then running :func:`Pkg.resolve` to update the set of installed packages to match.
While :func:`Pkg.add` and :func:`Pkg.rm` are convenient for adding and removing requirements for a single package, when you want to add or remove multiple packages, you can call :func:`Pkg.edit` to manually change the contents of ``REQUIRE`` and then update your packages accordingly.
:func:`Pkg.edit` does not roll back the contents of ``REQUIRE`` if :func:`Pkg.resolve` fails – rather, you have to run :func:`Pkg.edit` again to fix the files contents yourself.

Because the package manager uses git internally to manage the package git repositories, users may run into protocol issues (if behind a firewall, for example), when running :func:`Pkg.add`. The following command can be run from the command line to tell git to use 'https' instead of the 'git' protocol when cloning repositories::

    git config --global url."https://".insteadOf git://

Installing Unregistered Packages
--------------------------------

Julia packages are simply git repositories, clonable via any of the `protocols <https://www.kernel.org/pub/software/scm/git/docs/git-clone.html#URLS>`_ that git supports, and containing Julia code that follows certain layout conventions.
Official Julia packages are registered in the `METADATA.jl <https://github.com/JuliaLang/METADATA.jl>`_ repository, available at a well-known location [1]_.
The :func:`Pkg.add` and :func:`Pkg.rm` commands in the previous section interact with registered packages, but the package manager can install and work with unregistered packages too.
To install an unregistered package, use :func:`Pkg.clone(url) <Pkg.clone>`, where ``url`` is a git URL from which the package can be cloned::

    julia> Pkg.clone("git://example.com/path/to/Package.jl.git")
    INFO: Cloning Package from git://example.com/path/to/Package.jl.git
    Cloning into 'Package'...
    remote: Counting objects: 22, done.
    remote: Compressing objects: 100% (10/10), done.
    remote: Total 22 (delta 8), reused 22 (delta 8)
    Receiving objects: 100% (22/22), 2.64 KiB, done.
    Resolving deltas: 100% (8/8), done.

By convention, Julia repository names end with ``.jl`` (the additional ``.git`` indicates a "bare" git repository), which keeps them from colliding with repositories for other languages, and also makes Julia packages easy to find in search engines.
When packages are installed in your ``.julia/v0.3`` directory, however, the extension is redundant so we leave it off.

If unregistered packages contain a ``REQUIRE`` file at the top of their source tree, that file will be used to determine which registered packages the unregistered package depends on, and they will automatically be installed.
Unregistered packages participate in the same version resolution logic as registered packages, so installed package versions will be adjusted as necessary to satisfy the requirements of both registered and unregistered packages.

.. [1] The official set of packages is at https://github.com/JuliaLang/METADATA.jl, but individuals and organizations can easily use a different metadata repository. This allows control which packages are available for automatic installation. One can allow only audited and approved package versions, and make private packages or forks available.

Updating Packages
-----------------

When package developers publish new registered versions of packages that you're using, you will, of course, want the new shiny versions.
To get the latest and greatest versions of all your packages, just do :func:`Pkg.update`::

    julia> Pkg.update()
    INFO: Updating METADATA...
    INFO: Computing changes...
    INFO: Upgrading Distributions: v0.2.8 => v0.2.10
    INFO: Upgrading Stats: v0.2.7 => v0.2.8

The first step of updating packages is to pull new changes to ``~/.julia/v0.3/METADATA`` and see if any new registered package versions have been published.
After this, :func:`Pkg.update` attempts to update packages that are checked out on a branch and not dirty (i.e. no changes have been made to files tracked by git) by pulling changes from the package's upstream repository.
Upstream changes will only be applied if no merging or rebasing is necessary – i.e. if the branch can be `"fast-forwarded" <http://git-scm.com/book/en/Git-Branching-Basic-Branching-and-Merging>`_.
If the branch cannot be fast-forwarded, it is assumed that you're working on it and will update the repository yourself.

Finally, the update process recomputes an optimal set of package versions to have installed to satisfy your top-level requirements and the requirements of "fixed" packages.
A package is considered fixed if it is one of the following:

1. **Unregistered:** the package is not in ``METADATA`` – you installed it with :func:`Pkg.clone`.
2. **Checked out:** the package repo is on a development branch.
3. **Dirty:** changes have been made to files in the repo.

If any of these are the case, the package manager cannot freely change the installed version of the package, so its requirements must be satisfied by whatever other package versions it picks.
The combination of top-level requirements in ``~/.julia/v0.3/REQUIRE`` and the requirement of fixed packages are used to determine what should be installed.

Checkout, Pin and Free
----------------------

You may want to use the ``master`` version of a package rather than one of its registered versions.
There might be fixes or functionality on master that you need that aren't yet published in any registered versions, or you may be a developer of the package and need to make changes on ``master`` or some other development branch.
In such cases, you can do :func:`Pkg.checkout(pkg) <Pkg.checkout>` to checkout the ``master`` branch of ``pkg`` or :func:`Pkg.checkout(pkg,branch) <Pkg.checkout>` to checkout some other branch::

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

Immediately after installing ``Distributions`` with :func:`Pkg.add` it is on the current most recent registered version – ``0.2.9`` at the time of writing this.
Then after running :func:`Pkg.checkout("Distributions") <Pkg.checkout>`, you can see from the output of :func:`Pkg.status` that ``Distributions`` is on an unregistered version greater than ``0.2.9``, indicated by the "pseudo-version" number ``0.2.9+``.

When you checkout an unregistered version of a package, the copy of the ``REQUIRE`` file in the package repo takes precedence over any requirements registered in ``METADATA``, so it is important that developers keep this file accurate and up-to-date, reflecting the actual requirements of the current version of the package.
If the ``REQUIRE`` file in the package repo is incorrect or missing, dependencies may be removed when the package is checked out.
This file is also used to populate newly published versions of the package if you use the API that :mod:`Pkg <Base.Pkg>` provides for this (described below).

When you decide that you no longer want to have a package checked out on a branch, you can "free" it back to the control of the package manager with :func:`Pkg.free(pkg) <Pkg.free>`::

    julia> Pkg.free("Distributions")
    INFO: Freeing Distributions...
    INFO: No packages to install, update or remove.

    julia> Pkg.status()
    Required packages:
     - Distributions                 0.2.9
    Additional packages:
     - NumericExtensions             0.2.17
     - Stats                         0.2.7

After this, since the package is on a registered version and not on a branch, its version will be updated as new registered versions of the package are published.

If you want to pin a package at a specific version so that calling :func:`Pkg.update` won't change the version the package is on, you can use the :func:`Pkg.pin` function::

    julia> Pkg.pin("Stats")
    INFO: Creating Stats branch pinned.47c198b1.tmp

    julia> Pkg.status()
    Required packages:
     - Distributions                 0.2.9
    Additional packages:
     - NumericExtensions             0.2.17
     - Stats                         0.2.7              pinned.47c198b1.tmp

After this, the ``Stats`` package will remain pinned at version ``0.2.7`` – or more specifically, at commit ``47c198b1``, but since versions are permanently associated a given git hash, this is the same thing.
:func:`Pkg.pin` works by creating a throw-away branch for the commit you want to pin the package at and then checking that branch out.
By default, it pins a package at the current commit, but you can choose a different version by passing a second argument::

    julia> Pkg.pin("Stats",v"0.2.5")
    INFO: Creating Stats branch pinned.1fd0983b.tmp
    INFO: No packages to install, update or remove.

    julia> Pkg.status()
    Required packages:
     - Distributions                 0.2.9
    Additional packages:
     - NumericExtensions             0.2.17
     - Stats                         0.2.5              pinned.1fd0983b.tmp

Now the ``Stats`` package is pinned at commit ``1fd0983b``, which corresponds to version ``0.2.5``.
When you decide to "unpin" a package and let the package manager update it again, you can use :func:`Pkg.free` like you would to move off of any branch::

    julia> Pkg.free("Stats")
    INFO: Freeing Stats...
    INFO: No packages to install, update or remove.

    julia> Pkg.status()
    Required packages:
     - Distributions                 0.2.9
    Additional packages:
     - NumericExtensions             0.2.17
     - Stats                         0.2.7

After this, the ``Stats`` package is managed by the package manager again, and future calls to :func:`Pkg.update` will upgrade it to newer versions when they are published.
The throw-away ``pinned.1fd0983b.tmp`` branch remains in your local ``Stats`` repo, but since git branches are extremely lightweight, this doesn't really matter;
if you feel like cleaning them up, you can go into the repo and delete those branches.

.. [2] Packages that aren't on branches will also be marked as dirty if you make changes in the repo, but that's a less common thing to do.

*******************
Package Development
*******************

Julia's package manager is designed so that when you have a package installed, you are already in a position to look at its source code and full development history.
You are also able to make changes to packages, commit them using git, and easily contribute fixes and enhancements upstream.
Similarly, the system is designed so that if you want to create a new package, the simplest way to do so is within the infrastructure provided by the package manager.

Initial Setup
-------------

Since packages are git repositories, before doing any package development you should setup the following standard global git configuration settings::

    $ git config --global user.name "FULL NAME"
    $ git config --global user.email "EMAIL"

where ``FULL NAME`` is your actual full name (spaces are allowed between the double quotes) and ``EMAIL`` is your actual email address.
Although it isn't necessary to use `GitHub <https://github.com/>`_ to create or publish Julia packages, most Julia packages as of writing this are hosted on GitHub and the package manager knows how to format origin URLs correctly and otherwise work with the service smoothly.
We recommend that you create a `free account <https://github.com/join>`_ on GitHub and then do::

    $ git config --global github.user "USERNAME"

where ``USERNAME`` is your actual GitHub user name.
Once you do this, the package manager knows your GitHub user name and can configure things accordingly.
You should also `upload <https://github.com/settings/ssh>`_ your public SSH key to GitHub and set up an `SSH agent <http://linux.die.net/man/1/ssh-agent>`_ on your development machine so that you can push changes with minimal hassle.
In the future, we will make this system extensible and support other common git hosting options like `BitBucket <https://bitbucket.org>`_ and allow developers to choose their favorite.

Generating a New Package
------------------------

Suppose you want to create a new Julia package called ``FooBar``.
To get started, do :func:`Pkg.generate(pkg,license) <Pkg.generate>` where ``pkg`` is the new package name and ``license`` is the name of a license that the package generator knows about::

    julia> Pkg.generate("FooBar","MIT")
    INFO: Initializing FooBar repo: /Users/stefan/.julia/v0.3/FooBar
    INFO: Origin: git://github.com/StefanKarpinski/FooBar.jl.git
    INFO: Generating LICENSE.md
    INFO: Generating README.md
    INFO: Generating src/FooBar.jl
    INFO: Generating test/runtests.jl
    INFO: Generating .travis.yml
    INFO: Committing FooBar generated files

This creates the directory ``~/.julia/v0.3/FooBar``, initializes it as a git repository, generates a bunch of files that all packages should have, and commits them to the repository::

    $ cd ~/.julia/v0.3/FooBar && git show --stat

    commit 84b8e266dae6de30ab9703150b3bf771ec7b6285
    Author: Stefan Karpinski <stefan@karpinski.org>
    Date:   Wed Oct 16 17:57:58 2013 -0400

        FooBar.jl generated files.

            license: MIT
            authors: Stefan Karpinski
            years:   2013
            user:    StefanKarpinski

        Julia Version 0.3.0-prerelease+3217 [5fcfb13*]

     .travis.yml      | 16 +++++++++++++
     LICENSE.md       | 22 +++++++++++++++++++++++
     README.md        |  3 +++
     src/FooBar.jl    |  5 +++++
     test/runtests.jl |  5 +++++
     5 files changed, 51 insertions(+)

At the moment, the package manager knows about the MIT "Expat" License, indicated by ``"MIT"``, the Simplified BSD License, indicated by ``"BSD"``, and version 2.0 of the Apache Software License, indicated by ``"ASL"``.
If you want to use a different license, you can ask us to add it to the package generator, or just pick one of these three and then modify the ``~/.julia/v0.3/PACKAGE/LICENSE.md`` file after it has been generated.

If you created a GitHub account and configured git to know about it, :func:`Pkg.generate` will set an appropriate origin URL for you.
It will also automatically generate a ``.travis.yml`` file for using the `Travis <https://travis-ci.org>`_ automated testing service.
You will have to enable testing on the Travis website for your package repository, but once you've done that, it will already have working tests.
Of course, all the default testing does is verify that ``using FooBar`` in Julia works.

Making Your Package Available
-----------------------------

Once you've made some commits and you're happy with how ``FooBar`` is working, you may want to get some other people to try it out.
First you'll need to create the remote repository and push your code to it;
we don't yet automatically do this for you, but we will in the future and it's not too hard to figure out [3]_.
Once you've done this, letting people try out your code is as simple as sending them the URL of the published repo – in this case::

    git://github.com/StefanKarpinski/FooBar.jl.git

For your package, it will be your GitHub user name and the name of your package, but you get the idea.
People you send this URL to can use :func:`Pkg.clone` to install the package and try it out::

    julia> Pkg.clone("git://github.com/StefanKarpinski/FooBar.jl.git")
    INFO: Cloning FooBar from git@github.com:StefanKarpinski/FooBar.jl.git

.. [3] Installing and using GitHub's `"hub" tool <https://github.com/github/hub>`_ is highly recommended. It allows you to do things like run ``hub create`` in the package repo and have it automatically created via GitHub's API.

Publishing Your Package
-----------------------

Once you've decided that ``FooBar`` is ready to be registered as an official package, you can add it to your local copy of ``METADATA`` using :func:`Pkg.register`::

    julia> Pkg.register("FooBar")
    INFO: Registering FooBar at git://github.com/StefanKarpinski/FooBar.jl.git
    INFO: Committing METADATA for FooBar

This creates a commit in the ``~/.julia/v0.3/METADATA`` repo::

    $ cd ~/.julia/v0.3/METADATA && git show

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

This commit is only locally visible, however.  In order to make it visible to
the world, you need to merge your local ``METADATA`` upstream into the official
repo.  The :func:`Pkg.publish` command will fork the ``METADATA`` repository on
GitHub, push your changes to your fork, and open a pull request::

    julia> Pkg.publish()
    INFO: Validating METADATA
    INFO: No new package versions to publish
    INFO: Submitting METADATA changes
    INFO: Forking JuliaLang/METADATA.jl to StefanKarpinski
    INFO: Pushing changes as branch pull-request/ef45f54b
    INFO: To create a pull-request open:

      https://github.com/StefanKarpinski/METADATA.jl/compare/pull-request/ef45f54b

For various reasons :func:`Pkg.publish` sometimes does not succeed.  
In those cases, you may make a pull request on GitHub, which is `not difficult <https://help.github.com/articles/creating-a-pull-request>`_.

Once the package URL for ``FooBar`` is registered in the official ``METADATA`` repo, people know where to clone the package from, but there still aren't any registered versions available.
This means that :func:`Pkg.add("FooBar") <Pkg.add>` won't work yet since it only installs official versions.
:func:`Pkg.clone("FooBar") <Pkg.clone>` without having to specify a URL for it.
Moreover, when they run :func:`Pkg.update`, they will get the latest version of ``FooBar`` that you've pushed to the repo.
This is a good way to have people test out your packages as you work on them, before they're ready for an official release.

Tagging Package Versions
------------------------

Once you are ready to make an official version your package, you can tag and register it with the :func:`Pkg.tag` command::

    julia> Pkg.tag("FooBar")
    INFO: Tagging FooBar v0.0.1
    INFO: Committing METADATA for FooBar

This tags ``v0.0.1`` in the ``FooBar`` repo::

    $ cd ~/.julia/v0.3/FooBar && git tag
    v0.0.1

It also creates a new version entry in your local ``METADATA`` repo for ``FooBar``::

    $ cd ~/.julia/v0.3/FooBar && git show
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

If there is a ``REQUIRE`` file in your package repo, it will be copied into the appropriate spot in ``METADATA`` when you tag a version.
Package developers should make sure that the ``REQUIRE`` file in their package correctly reflects the requirements of their package, which will automatically flow into the official metadata if you're using :func:`Pkg.tag`.
See the `Requirements Specification <#man-package-requirements>`_ for the full format of ``REQUIRE``.

The :func:`Pkg.tag` command takes an optional second argument that is either an explicit version number object like ``v"0.0.1"`` or one of the symbols ``:patch``, ``:minor`` or ``:major``.
These increment the patch, minor or major version number of your package intelligently.

As with :func:`Pkg.register`, these changes to ``METADATA`` aren't
available to anyone else until they've been included upstream.  Again,
use the :func:`Pkg.publish` command, which first makes sure that
individual package repos have been tagged, pushes them if they haven't
already been, and then opens a pull request to ``METADATA``::

    julia> Pkg.publish()
    INFO: Validating METADATA
    INFO: Pushing FooBar permanent tags: v0.0.1
    INFO: Submitting METADATA changes
    INFO: Forking JuliaLang/METADATA.jl to StefanKarpinski
    INFO: Pushing changes as branch pull-request/3ef4f5c4
    INFO: To create a pull-request open:

      https://github.com/StefanKarpinski/METADATA.jl/compare/pull-request/3ef4f5c4

Fixing Package Requirements
---------------------------

If you need to fix the registered requirements of an already-published package version, you can do so just by editing the metadata for that version, which will still have the same commit hash – the hash associated with a version is permanent::

    $ cd ~/.julia/v0.3/METADATA/FooBar/versions/0.0.1 && cat requires
    julia 0.3-
    $ vi requires

Since the commit hash stays the same, the contents of the ``REQUIRE`` file that will be checked out in the repo will **not** match the requirements in ``METADATA`` after such a change;
this is unavoidable.
When you fix the requirements in ``METADATA`` for a previous version of a package, however, you should also fix the ``REQUIRE`` file in the current version of the package.

.. _man-package-requirements:

Requirements Specification
--------------------------

The ``~/.julia/v0.3/REQUIRE`` file, the ``REQUIRE`` file inside packages, and the ``METADATA`` package ``requires`` files use a simple line-based format to express the ranges of package versions which need to be installed.  Package ``REQUIRE`` and ``METADATA requires`` files should also include the range of versions of ``julia`` the package is expected to work with.

Here's how these files are parsed and interpreted.

* Everything after a ``#`` mark is stripped from each line as a comment.
* If nothing but whitespace is left, the line is ignored.
* If there are non-whitespace characters remaining, the line is a requirement and the is split on whitespace into words.

The simplest possible requirement is just the name of a package name on a line by itself::

    Distributions

This requirement is satisfied by any version of the ``Distributions`` package.
The package name can be followed by zero or more version numbers in ascending order, indicating acceptable intervals of versions of that package.
One version opens an interval, while the next closes it, and the next opens a new interval, and so on;
if an odd number of version numbers are given, then arbitrarily large versions will satisfy;
if an even number of version numbers are given, the last one is an upper limit on acceptable version numbers.
For example, the line::

    Distributions 0.1

is satisfied by any version of ``Distributions`` greater than or equal to ``0.1.0``.
Suffixing a version with `-` allows any pre-release versions as well. For example::

    Distributions 0.1-

is satisfied by pre-release versions such as ``0.1-dev`` or ``0.1-rc1``, or by any version greater than or equal to ``0.1.0``.

This requirement entry::

    Distributions 0.1 0.2.5

is satisfied by versions from ``0.1.0`` up to, but not including ``0.2.5``.
If you want to indicate that any ``0.1.x`` version will do, you will want to write::

    Distributions 0.1 0.2-

If you want to start accepting versions after ``0.2.7``, you can write::

    Distributions 0.1 0.2- 0.2.7

If a requirement line has leading words that begin with ``@``, it is a system-dependent requirement.
If your system matches these system conditionals, the requirement is included, if not, the requirement is ignored.
For example::

    @osx Homebrew

will require the ``Homebrew`` package only on systems where the operating system is OS X.
The system conditions that are currently supported are::

    @windows
    @unix
    @osx
    @linux

The ``@unix`` condition is satisfied on all UNIX systems, including OS X, Linux and FreeBSD.
Negated system conditionals are also supported by adding a ``!`` after the leading ``@``.
Examples::

    @!windows
    @unix @!osx

The first condition applies to any system but Windows and the second condition applies to any UNIX system besides OS X.

Runtime checks for the current version of Julia can be made using the built-in
``VERSION`` variable, which is of type :class:`VersionNumber`. Such code is
occasionally necessary to keep track of new or deprecated functionality between
various releases of Julia. Examples of runtime checks::

    VERSION < v"0.3-" #exclude all pre-release versions of 0.3

    v"0.2-" <= VERSION < v"0.3-" #get all 0.2 versions, including pre-releases, up to the above

    v"0.2" <= VERSION < v"0.3-" #To get only stable 0.2 versions (Note v"0.2" == v"0.2.0")

    VERSION >= v"0.2.1" #get at least version 0.2.1

See the section on :ref:`version number literals <man-version-number-literals>` for a more complete description.

