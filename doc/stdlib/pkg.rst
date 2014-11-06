.. module:: Base.Pkg

Package Manager Functions
-------------------------

All package manager functions are defined in the ``Pkg`` module. None of the ``Pkg`` module's functions are exported;
to use them, you'll need to prefix each function call with an explicit ``Pkg.``, e.g. ``Pkg.status()`` or ``Pkg.dir()``.

.. function:: dir() -> AbstractString

   Returns the absolute path of the package directory.
   This defaults to ``joinpath(homedir(),".julia")`` on all platforms (i.e. ``~/.julia`` in UNIX shell syntax).
   If the ``JULIA_PKGDIR`` environment variable is set, that path is used instead.
   If ``JULIA_PKGDIR`` is a relative path, it is interpreted relative to whatever the current working directory is.

.. function:: dir(names...) -> AbstractString

   Equivalent to ``normpath(Pkg.dir(),names...)`` – i.e. it appends path components to the package directory and normalizes the resulting path.
   In particular, ``Pkg.dir(pkg)`` returns the path to the package ``pkg``.

.. function:: init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

   Initialize ``Pkg.dir()`` as a package directory.  
   This will be done automatically when the ``JULIA_PKGDIR`` is not set and ``Pkg.dir()`` uses its default value.
   As part of this process, clones a local METADATA git repository from the site and branch specified by its arguments, which
   are typically not provided.  Explicit (non-default) arguments can be used to support a custom METADATA setup.

.. function:: resolve()

   Determines an optimal, consistent set of package versions to install or upgrade to.
   The optimal set of package versions is based on the contents of ``Pkg.dir("REQUIRE")`` and the state of installed packages in ``Pkg.dir()``,
   Packages that are no longer required are moved into ``Pkg.dir(".trash")``.

.. function:: edit()

   Opens ``Pkg.dir("REQUIRE")`` in the editor specified by the ``VISUAL`` or ``EDITOR`` environment variables;
   when the editor command returns, it runs ``Pkg.resolve()`` to determine and install a new optimal set of installed package versions.

.. function:: add(pkg, vers...)

   Add a requirement entry for ``pkg`` to ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``.
   If ``vers`` are given, they must be ``VersionNumber`` objects and they specify acceptable version intervals for ``pkg``.

.. function:: rm(pkg)

   Remove all requirement entries for ``pkg`` from ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``.

.. function:: clone(url, [pkg])

   Clone a package directly from the git URL ``url``.
   The package does not need to be a registered in ``Pkg.dir("METADATA")``.
   The package repo is cloned by the name ``pkg`` if provided;
   if not provided, ``pkg`` is determined automatically from ``url``.

.. function:: clone(pkg)

   If ``pkg`` has a URL registered in ``Pkg.dir("METADATA")``, clone it from that URL on the default branch.
   The package does not need to have any registered versions.

.. function:: available() -> Vector{ASCIIString}

   Returns the names of available packages.

.. function:: available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package ``pkg``.

.. function:: installed() -> Dict{ASCIIString,VersionNumber}

   Returns a dictionary mapping installed package names to the installed version number of each package.

.. function:: installed(pkg) -> Nothing | VersionNumber

   If ``pkg`` is installed, return the installed version number, otherwise return ``nothing``.

.. function:: status()

   Prints out a summary of what packages are installed and what version and state they're in.

.. function:: update()

   Update package the metadata repo – kept in ``Pkg.dir("METADATA")`` – then update any fixed packages that can safely be pulled from their origin;
   then call ``Pkg.resolve()`` to determine a new optimal set of packages versions.

.. function:: checkout(pkg, [branch="master"])

   Checkout the ``Pkg.dir(pkg)`` repo to the branch ``branch``.
   Defaults to checking out the "master" branch.
   To go back to using the newest compatible released version, use ``Pkg.free(pkg)``

.. function:: pin(pkg)

   Pin ``pkg`` at the current version.
   To go back to using the newest compatible released version, use ``Pkg.free(pkg)``

.. function:: pin(pkg, version)

   Pin ``pkg`` at registered version ``version``.

.. function:: free(pkg)

   Free the package ``pkg`` to be managed by the package manager again.
   It calls ``Pkg.resolve()`` to determine optimal package versions after.
   This is an inverse for both ``Pkg.checkout`` and ``Pkg.pin``.

.. function:: build()

   Run the build scripts for all installed packages in depth-first recursive order.

.. function:: build(pkgs...)

   Run the build script in "deps/build.jl" for each package in ``pkgs`` and all of their dependencies in depth-first recursive order.
   This is called automatically by ``Pkg.resolve()`` on all installed or updated packages.

.. function:: generate(pkg,license)

   Generate a new package named ``pkg`` with one of these license keys: ``"MIT"`` or ``"BSD"``.
   If you want to make a package with a different license, you can edit it afterwards.
   Generate creates a git repo at ``Pkg.dir(pkg)`` for the package and inside it ``LICENSE.md``, ``README.md``, the julia entrypoint ``$pkg/src/$pkg.jl``, and a travis test file, ``.travis.yml``.

.. function:: register(pkg, [url])

   Register ``pkg`` at the git URL ``url``, defaulting to the configured origin URL of the git repo ``Pkg.dir(pkg)``.

.. function:: tag(pkg, [ver, [commit]])

   Tag ``commit`` as version ``ver`` of package ``pkg`` and create a version entry in ``METADATA``.
   If not provided, ``commit`` defaults to the current commit of the ``pkg`` repo.
   If ``ver`` is one of the symbols ``:patch``, ``:minor``, ``:major`` the next patch, minor or major version is used.
   If ``ver`` is not provided, it defaults to ``:patch``.

.. function:: publish()

   For each new package version tagged in ``METADATA`` not already published, make sure that the tagged package commits have been pushed to the repo at the registered URL for the package and if they all have, open a pull request to ``METADATA``.

.. function:: test()

   Run the tests for all installed packages ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its ``test/runtests.jl`` file and test dependencies are specified in ``test/REQUIRE``.

.. function:: test(pkgs...)

   Run the tests for each package in ``pkgs`` ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its ``test/runtests.jl`` file and test dependencies are specified in ``test/REQUIRE``.

