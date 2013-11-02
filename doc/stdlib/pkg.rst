.. module:: Base.Pkg

Package Manager Functions
-------------------------

.. function:: Pkg.dir() -> String

   Returns the absolute path of the package directory.
   This defaults to ``joinpath(home(),".julia")`` on all platforms (i.e. ``~/.julia`` in UNIX shell syntax).
   If the ``JULIA_PKGDIR`` environment variable is set, that path is used instead.
   If ``JULIA_PKGDIR`` is a relative path, it is interpreted relative to whatever the current working directory is.

.. function:: Pkg.dir(names...) -> String

   Equivalent to ``normpath(Pkg.dir(),names...)`` – i.e. it appends path components to the package directory and normalizes the resulting path.
   In particular, ``Pkg.dir(pkg)`` returns the path to the package ``pkg``.

.. function:: Pkg.init()

   Initialize ``Pkg.dir()`` as a package directory.
   This will be done automatically when the ``JULIA_PKGDIR`` is not set and ``Pkg.dir()`` uses its default value.

.. function:: Pkg.resolve()

   Determines an optimal, consistent set of package versions to install or upgrade to.
   The optimal set of package versions is based on the contents of ``Pkg.dir("REQUIRE")`` and the state of installed packages in ``Pkg.dir()``,
   Packages that are no longer required are moved into ``Pkg.dir(".trash")``.

.. function:: Pkg.edit()

   Opens ``Pkg.dir("REQUIRE")`` in the editor specified by the ``VISUAL`` or ``EDITOR`` environment variables;
   when the editor command returns, it runs ``Pkg.resolve()`` to determine and install a new optimal set of installed package versions.

.. function:: Pkg.add(pkg, vers...)

   Add a requirement entry for ``pkg`` to ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``.
   If ``vers`` are given, they must be ``VersionNumber`` objects and they specify acceptable version intervals for ``pkg``.

.. function:: Pkg.rm(pkg)

   Remove all requirement entries for ``pkg`` from ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``.

.. function:: Pkg.clone(url, [pkg])

   Clone a package directly from the git URL ``url``.
   The package does not need to be a registered in ``Pkg.dir("METADATA")``.
   The package repo is cloned by the name ``pkg`` if provided;
   if not provided, ``pkg`` is determined automatically from ``url``.

.. function:: Pkg.clone(pkg)

   If ``pkg`` has a URL registered in ``Pkg.dir("METADATA")``, clone it from that URL on the default branch.
   The package does not need to have any registered versions.

.. function:: Pkg.available() -> Vector{ASCIIString}

   Returns the names of available packages.

.. function:: Pkg.available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package ``pkg``.

.. function:: Pkg.installed() -> Dict{ASCIIString,VersionNumber}

   Returns a dictionary mapping installed package names to the installed version number of each package.

.. function:: Pkg.installed(pkg) -> Nothing | VersionNumber

   If ``pkg`` is installed, return the installed version number, otherwise return ``nothing``.

.. function:: Pkg.status()

   Prints out a summary of what packages are installed and what version and state they're in.

.. function:: Pkg.update()

   Update package the metadata repo – kept in ``Pkg.dir("METADATA")`` – then update any fixed packages that can safely be pulled from their origin;
   then call ``Pkg.resolve()`` to determine a new optimal set of packages versions.

.. function:: Pkg.checkout(pkg, [branch="master"])

   Checkout the ``Pkg.dir(pkg)`` repo to the branch ``branch``.
   Defaults to checking out the "master" branch.

.. function:: Pkg.pin(pkg)

   Pin ``pkg`` at the current version.

.. function:: Pkg.pin(pkg, version)

   Pin ``pkg`` at registered version ``version``.

.. function:: Pkg.free(pkg)

   Free the package ``pkg`` to be managed by the package manager again.
   It calls ``Pkg.resolve()`` to determine optimal package versions after.
   This is an inverse for both ``Pkg.checkout`` and ``Pkg.pin``.

.. function:: Pkg.build()

   Run the build scripts for all installed packages in depth-first recursive order.

.. function:: Pkg.build(pkgs...)

   Run the build scripts for each package in ``pkgs`` and all of their dependencies in depth-first recursive order.
   This is called automatically by ``Pkg.resolve()`` on all installed or updated packages.

.. function:: Pkg.generate(pkg,license)

   Generate a new package named ``pkg`` with one of these license keys: ``"MIT"`` or ``"BSD"``.
   If you want to make a package with a different license, you can edit it afterwards.
   Generate creates a git repo at ``Pkg.dir(pkg)`` for the package and inside it ``LICENSE.md``, ``README.md``, the julia entrypoint ``$pkg/src/$pkg.jl``, and a travis test file, ``.travis.yml``.

.. function:: Pkg.register(pkg, [url])

   Register ``pkg`` at the git URL ``url``, defaulting to the configured origin URL of the git repo ``Pkg.dir(pkg)``.

.. function:: Pkg.tag(pkg, [ver, [commit]])

   Tag ``commit`` as version ``ver`` of package ``pkg`` and create a version entry in ``METADATA``.
   If not provided, ``commit`` defaults to the current commit of the ``pkg`` repo.
   If ``ver`` is one of the symbols ``:patch``, ``:minor``, ``:major`` the next patch, minor or major version is used.
   If ``ver`` is not provided, it defaults to ``:patch``.

.. function:: Pkg.publish()

   For each new package version tagged in ``METADATA`` not already published, make sure that the tagged package commits have been pushed to the repo at the registered URL for the package and if they all have, push ``METADATA``.

