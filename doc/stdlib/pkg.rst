.. module:: Base.Pkg

***************************
 Package Manager Functions
***************************

All package manager functions are defined in the ``Pkg`` module. None of the ``Pkg`` module's functions are exported;
to use them, you'll need to prefix each function call with an explicit ``Pkg.``, e.g. ``Pkg.status()`` or ``Pkg.dir()``.

Functions for package development (e.g. ``tag``, ``publish``, etc.) have been moved to the `PkgDev <https://github.com/JuliaLang/PkgDev.jl>`_ package. See `PkgDev README <https://github.com/JuliaLang/PkgDev.jl/blob/master/README.md#usage>`_ for the documentation of those functions.

.. function:: dir() -> AbstractString

   .. Docstring generated from Julia source

   Returns the absolute path of the package directory. This defaults to ``joinpath(homedir(),".julia","v$(VERSION.major).$(VERSION.minor)")`` on all platforms (i.e. ``~/.julia/v0.5`` in UNIX shell syntax). If the ``JULIA_PKGDIR`` environment variable is set, then that path is used in the returned value as ``joinpath(ENV["JULIA_PKGDIR"],"v$(VERSION.major).$(VERSION.minor)")``\ . If ``JULIA_PKGDIR`` is a relative path, it is interpreted relative to whatever the current working directory is.

.. function:: dir(names...) -> AbstractString

   .. Docstring generated from Julia source

   Equivalent to ``normpath(Pkg.dir(),names...)`` – i.e. it appends path components to the package directory and normalizes the resulting path. In particular, ``Pkg.dir(pkg)`` returns the path to the package ``pkg``\ .

.. function:: init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

   .. Docstring generated from Julia source

   Initialize ``Pkg.dir()`` as a package directory. This will be done automatically when the ``JULIA_PKGDIR`` is not set and ``Pkg.dir()`` uses its default value. As part of this process, clones a local METADATA git repository from the site and branch specified by its arguments, which are typically not provided. Explicit (non-default) arguments can be used to support a custom METADATA setup.

.. function:: resolve()

   .. Docstring generated from Julia source

   Determines an optimal, consistent set of package versions to install or upgrade to. The optimal set of package versions is based on the contents of ``Pkg.dir("REQUIRE")`` and the state of installed packages in ``Pkg.dir()``\ , Packages that are no longer required are moved into ``Pkg.dir(".trash")``\ .

.. function:: edit()

   .. Docstring generated from Julia source

   Opens ``Pkg.dir("REQUIRE")`` in the editor specified by the ``VISUAL`` or ``EDITOR`` environment variables; when the editor command returns, it runs ``Pkg.resolve()`` to determine and install a new optimal set of installed package versions.

.. function:: add(pkg, vers...)

   .. Docstring generated from Julia source

   Add a requirement entry for ``pkg`` to ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``\ . If ``vers`` are given, they must be ``VersionNumber`` objects and they specify acceptable version intervals for ``pkg``\ .

.. function:: rm(pkg)

   .. Docstring generated from Julia source

   Remove all requirement entries for ``pkg`` from ``Pkg.dir("REQUIRE")`` and call ``Pkg.resolve()``\ .

.. function:: clone(url, [pkg])

   .. Docstring generated from Julia source

   Clone a package directly from the git URL ``url``\ . The package does not need to be registered in ``Pkg.dir("METADATA")``\ . The package repo is cloned by the name ``pkg`` if provided; if not provided, ``pkg`` is determined automatically from ``url``\ .

.. function:: clone(pkg)

   .. Docstring generated from Julia source

   If ``pkg`` has a URL registered in ``Pkg.dir("METADATA")``\ , clone it from that URL on the default branch. The package does not need to have any registered versions.

.. function:: setprotocol!(proto)

   .. Docstring generated from Julia source

   Set the protocol used to access GitHub-hosted packages. Defaults to 'https', with a blank ``proto`` delegating the choice to the package developer.

.. function:: available() -> Vector{String}

   .. Docstring generated from Julia source

   Returns the names of available packages.

.. function:: available(pkg) -> Vector{VersionNumber}

   .. Docstring generated from Julia source

   Returns the version numbers available for package ``pkg``\ .

.. function:: installed() -> Dict{String,VersionNumber}

   .. Docstring generated from Julia source

   Returns a dictionary mapping installed package names to the installed version number of each package.

.. function:: installed(pkg) -> Void | VersionNumber

   .. Docstring generated from Julia source

   If ``pkg`` is installed, return the installed version number, otherwise return ``nothing``\ .

.. function:: status()

   .. Docstring generated from Julia source

   Prints out a summary of what packages are installed and what version and state they're in.

.. function:: update()

   .. Docstring generated from Julia source

   Update the metadata repo – kept in ``Pkg.dir("METADATA")`` – then update any fixed packages that can safely be pulled from their origin; then call ``Pkg.resolve()`` to determine a new optimal set of packages versions.

.. function:: checkout(pkg, [branch="master"]; merge=true, pull=true)

   .. Docstring generated from Julia source

   Checkout the ``Pkg.dir(pkg)`` repo to the branch ``branch``\ . Defaults to checking out the "master" branch. To go back to using the newest compatible released version, use ``Pkg.free(pkg)``\ . Changes are merged (fast-forward only) if the keyword argument ``merge == true``\ , and the latest version is pulled from the upstream repo if ``pull == true``\ .

.. function:: pin(pkg)

   .. Docstring generated from Julia source

   Pin ``pkg`` at the current version. To go back to using the newest compatible released version, use ``Pkg.free(pkg)``

.. function:: pin(pkg, version)

   .. Docstring generated from Julia source

   Pin ``pkg`` at registered version ``version``\ .

.. function:: free(pkg)

   .. Docstring generated from Julia source

   Free the package ``pkg`` to be managed by the package manager again. It calls ``Pkg.resolve()`` to determine optimal package versions after. This is an inverse for both ``Pkg.checkout`` and ``Pkg.pin``\ .

   You can also supply an iterable collection of package names, e.g., ``Pkg.free(("Pkg1", "Pkg2"))`` to free multiple packages at once.

.. function:: build()

   .. Docstring generated from Julia source

   Run the build scripts for all installed packages in depth-first recursive order.

.. function:: build(pkgs...)

   .. Docstring generated from Julia source

   Run the build script in ``deps/build.jl`` for each package in ``pkgs`` and all of their dependencies in depth-first recursive order. This is called automatically by ``Pkg.resolve()`` on all installed or updated packages.

.. function:: test()

   .. Docstring generated from Julia source

   Run the tests for all installed packages ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its ``test/runtests.jl`` file and test dependencies are specified in ``test/REQUIRE``\ .

.. function:: test(pkgs...)

   .. Docstring generated from Julia source

   Run the tests for each package in ``pkgs`` ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its ``test/runtests.jl`` file and test dependencies are specified in ``test/REQUIRE``\ .

