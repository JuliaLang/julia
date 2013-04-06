==============
Julia Packages
==============

Where to find Julia packages
----------------------------

- An official list of packages is available, see :ref:`available-packages`.

- Announcements of new packages can also be found in the `julia-users Google Groups <https://groups.google.com/forum/?fromgroups=#!forum/julia-users>`_.

Installing a new Julia package
------------------------------

The `Pkg` module in julia provides tools for installing and managing third party packages. It also manages the dependencies, while installing packages. Get the updated list of packages with::

    Pkg.update()

In order to install a package, use ``Pkg.add()``, where ``MY_PACKAGE_NAME`` is replaced with the actual package name::

   Pkg.add("MY_PACKAGE_NAME")

This installs the package to ``$HOME/.julia/MY_PACKAGE_NAME`` . In order to remove a package, do::

   Pkg.rm("MY_PACKAGE_NAME")

Internally, every Julia package is a ``git`` repository, and Julia uses ``git`` for its package management.

Contributing a new Julia package
--------------------------------

In the following, replace ``MY_PACKAGE_NAME``, ``MY_GITHUB_USER``, etc. with
the actual desired names.

Creating a new Julia package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Initialize your package in Julia by running::

    Pkg.new("MY_PACKAGE_NAME")

This will initialize a skeleton for a new package in ``$HOME/.julia/MY_PACKAGE_NAME``.

.. note::
   This will overwrite any existing files and git repository in ``$HOME/.julia/MY_PACKAGE_NAME``.

2. If you have already created a repository for your package, overwrite the
skeleton by copying or symlinking over it. For example,::

    rm -r $HOME/.julia/MY_PACKAGE_NAME
    ln -s /path/to/existing/repo/MY_PACKAGE_NAME $HOME/.julia/MY_PACKAGE_NAME

3. In ``REQUIRE``, list the names of all packages used by your new package. One
package per line.

4. Populate the package by filling out ``README.md`` and ``LICENSE.md``, source
code in ``src/``, and tests in ``test/``. Ensure that each test file contains these
lines near the beginning::

    using Test
    using MY_PACKAGE_NAME

5. Add a publicly accessible remote repository URL, if your package doesn't
already have one. For example, create a new repository called
``MY_PACKAGE_NAME.jl`` on Github and then run::

    cd $HOME/.julia/MY_PACKAGE_NAME
    git remote add github https://github.com/MY_GITHUB_USER/MY_PACKAGE_NAME.jl
 
6. Add at least one git commit and push it to the remote repository::

    # Do some stuff
    git add #new files
    git commit
    git push remote github

Distributing a Julia package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One-time setup (once per user)
------------------------------
1. Fork a copy of METADATA.jl, if you haven't done so already. The forked
repository URL should look like `https://github.com/MY_GITHUB_USER/METADATA.jl`.

2. Update the local METADATA with the URL of your forked repository.::

    cd $HOME/.julia/METADATA
    git remote add github https://github.com/MY_GITHUB_USER/METADATA.jl

Distributing a new package or new version of an existing package
----------------------------------------------------------------

0. Ensure that both your forked METADATA.jl on Github and your local METADATA
   repository are current. The latter should be checked out to the `devel`
   branch.::

    cd $HOME/.julia/METADATA
    git fetch --all
    git checkout devel
    git rebase origin/devel
    git push github devel

1. Populate the local METADATA by running in Julia: ::

    Pkg.pkg_origin("MY_PACKAGE_NAME")
    Pkg.patch("MY_PACKAGE_NAME")

2. Update the local METADATA with the URL of your forked repository and
create a new branch with your package in it.::

    cd $HOME/.julia/METADATA
    git branch MY_PACKAGE_NAME
    git checkout MY_PACKAGE_NAME
    git add MY_PACKAGE_NAME #Ensure that only the latest hash is committed
    git commit

3. Push to the remote METADATA repository::

    git push github MY_PACKAGE_NAME

4. Go to `https://github.com/MY_GITHUB_USER/METADATA.jl/tree/MY_PACKAGE_NAME`
in your web browser. Click the 'Pull Request' button.

.. image:: ../images/github_metadata_pullrequest.png

5. Submit a new pull request. Ensure that the pull request goes to the
devel branch and not master.

.. image:: ../images/github_metadata_develbranch.png

6. When the pull request is accepted, announce your new package to the
Julia community on the `julia-users Google Groups <https://groups.google.com/forum/?fromgroups=#!forum/julia-users>`_.
 
