# Code Loading

Julia has two mechanisms for loading code:

1. **Code inclusion:** e.g. `include("source.jl")`. Inclusion allows you to split a single program across multiple source files. The expression `include("source.jl")` causes the contents of the file `source.jl` to be evaluated in the global scope of the module where the `include` call occurs. If `include("source.jl")` is called multiple times, `source.jl` is evaluated multiple times. The included path, `source.jl`, is interpreted relative to the file where the `include` call occurs. This makes it simple to relocate a subtree of source files. In the REPL, included paths are interpreted relative to the current working directory, `pwd()`.
2. **Package loading:** e.g. `import X` or `using X`. The import mechanism allows you to load a package—i.e. an independent, reusable collection of Julia code, wrapped in a module—and makes the resulting module available by the name `X` inside of the importing module. If the same `X` package is imported multiple times in the same Julia session, it is only loaded the first time—on subsequent imports, the importing module gets a reference to the same module. Note though, that `import X` can load different packages in different contexts: `X` can refer to one package named `X` in the main project but potentially different packages named `X` in each dependency. More on this below.

Code inclusion is quite straightforward: it simply parses and evaluates a source file in the context of the caller. Package loading is built on top of code inclusion and is a lot more complex. Therefore, the rest of this chapter focuses on the behavior and mechanics of package loading.

!!! note
    You only need to read this chapter if you want to understand the technical details of package loading. If you just want to install and use packages, simply use Julia's built-in package manager to add packages to your environment and write `import X` or `using X` in your code to load packages that you've added.

A *package* is a source tree with a standard layout providing functionality that can be reused by other Julia projects. A package is loaded by `import X` or  `using X` statements. These statements also make the module named `X`, which results from loading the package code, available within the module where the import statement occurs. The meaning of `X` in `import X` is context-dependent: which `X` package is loaded depends on what code the statement occurs in. The effect of `import X` depends on two questions:

1. **What** package is `X` in this context?
2. **Where** can that `X` package be found?

Understanding how Julia answers these questions is key to understanding package loading.

## Federation of packages

Julia supports federated management of packages. This means that multiple independent parties can maintain both public and private packages and registries of them, and that projects can depend on a mix of public and private packages from different registries. Packages from various registries are installed and managed using a common set of tools and workflows. The `Pkg` package manager ships with Julia 0.7/1.0 and lets you install and manage your projects' dependencies. It does this by creating and manipulating project files that describe what your project depends on, and manifest files that snapshot exact versions of your project's complete dependency graph.

One consequence of federation is that there cannot be a central authority for package naming. Different entities may use the same name to refer to unrelated packages. This possibility is unavoidable since these entities do not coordinate and may not even know about each other. Because of the lack of a central naming authority, a single project can quite possibly end up depending on different packages that have the same name. Julia's package loading mechanism handles this by not requiring package names to be globally unique, even within the dependency graph of a single project. Instead, packages are identified by [universally unique identifiers](https://en.wikipedia.org/wiki/Universally_unique_identifier) (UUIDs) which are assigned to them before they are registered. The question *"what is `X`?"* is answered by determining the UUID of `X`.

Since the decentralized naming problem is somewhat abstract, it may help to walk through a concrete scenario to understand the issue. Suppose you're developing an application called `App`, which uses two packages: `Pub` and  `Priv`. `Priv` is a private package that you created, whereas `Pub` is a public package that you use but don't control. When you created `Priv`, there was no public package by that name. Subsequently, however, an unrelated package also named `Priv` has been published and become popular. In fact, the `Pub` package has started to use it. Therefore, when you next upgrade `Pub` to get the latest bug fixes and features, `App` will end up—through no action of yours other than upgrading—depending on two different packages named `Priv`. `App` has a direct dependency on your private `Priv` package, and an indirect dependency, through `Pub`, on the new public `Priv` package. Since these two `Priv` packages are different but both required for `App` to continue working correctly, the expression `import Priv` must refer to different `Priv` packages depending on whether it occurs in `App`'s code or in `Pub`'s code. Julia's package loading mechanism allows this by distinguishing the two `Priv` packages by context and UUID. How this distinction works is determined by environments, as explained in the following sections.

## Environments

An *environment* determines what `import X` and `using X` mean in various code contexts and what files these statements cause to be loaded. Julia understands three kinds of environments:

1. **A project environment** is a directory with a project file and an optional manifest file. The project file determines what the names and identities of the direct dependencies of a project are. The manifest file, if present, gives a complete dependency graph, including all direct and indirect dependencies, exact versions of each dependency, and sufficient information to locate and load the correct version.
2. **A package directory** is a directory containing the source trees of a set of packages as subdirectories. This kind of environment was the only kind that existed in Julia 0.6 and earlier. If `X` is a subdirectory of a package directory and `X/src/X.jl` exists, then the package `X` is available in the package directory environment and `X/src/X.jl` is the source file by which it is loaded.
3. **A stacked environment** is an ordered set of project environments and package directories, overlaid to make a single composite environment in which all the packages available in its constituent environments are available. Julia's load path is a stacked environment, for example.

These three kinds of environment each serve a different purpose:

* Project environments provide **reproducibility.** By checking a project environment into version control—e.g. a git repository—along with the rest of the project's source code, you can reproduce the exact state of the project _and_ all of its dependencies since the manifest file captures the exact version of every dependency.
* Package directories provide low-overhead **convenience** when a project environment isn't needed. Package directories are handy when you have a set of packages that you just want to put somewhere and use them as they are, without having to create and maintain a project environment for them.
* Stacked environments allow for **augmentation** of the primary environment with additional tools. You can push an environment including development tools onto the stack and they will be available from the REPL and scripts but not from inside packages.

As an abstraction, an environment provides three maps: `roots`, `graph` and `paths`. When resolving the meaning of `import X`, `roots` and `graph` are used to determine the identity of `X` and answer the question *"what is `X`?"*, while the `paths` map is used to locate the source code of `X` and answer the question *"where is `X`?"* The specific roles of the three maps are:

- **roots:** `name::Symbol` ⟶ `uuid::UUID`

   An environment's `roots` map assigns package names to UUIDs for all the top-level dependencies that the environment makes available to the main project (i.e. the ones that can be loaded in `Main`). When Julia encounters `import X` in the main project, it looks up the identity of `X` as `roots[:X]`.

- **graph:** `context::UUID` ⟶ `name::Symbol` ⟶ `uuid::UUID`

   An environment's `graph` is a multilevel map which assigns, for each `context` UUID, a map from names to UUIDs, similar to the `roots` map but specific to that `context`. When Julia sees `import X` in the code of the package whose UUID is `context`, it looks up the identity of `X` as `graph[context][:X]`. In particular, this means that `import X` can refer to different packages depending on `context`.

- **paths:** `uuid::UUID` × `name::Symbol` ⟶ `path::String`

   The `paths` map assigns to each package UUID-name pair, the location of that package's entry-point source file. After the identity of `X` in `import X` has been resolved to a UUID via `roots` or `graph` (depending on whether it is loaded from the main project or a dependency), Julia determines what file to load to acquire `X` by looking up `paths[uuid,:X]` in the environment. Including this file should create a module named `X`. Once this package is loaded, i.e. after its first import, any subsequent import resolving to the same `uuid` will simply create a new binding to the original already-loaded package module.

Each kind of environment defines these three maps differently, as detailed in the following sections.

!!! note
    For ease of understanding, the examples throughout this chapter show full data structures for `roots`, `graph` and `paths`. However, for efficiency, Julia's package loading code does not actually create them. Instead, it queries them through internal APIs and lazily computes only as much of each structure as it needs to load a given package.

### Project environments

A project environment is determined by a directory containing a project file called `Project.toml`, and optionally a manifest file called `Manifest.toml`. These files may also be called `JuliaProject.toml` and `JuliaManifest.toml`, in which case `Project.toml` and `Manifest.toml` are ignored. (This allows for coexistence with other tools that might consider files called `Project.toml` and `Manifest.toml` significant.) For pure Julia projects, however, the names `Project.toml` and `Manifest.toml` are preferred. The `roots`, `graph` and `paths` maps of a project environment are defined as follows.

**The roots map** of the environment is determined by the contents of the project file, specifically, its top-level `name` and `uuid` entries and its `[deps]` section (all optional). Consider the following example project file for the hypothetical application, `App`, as described earlier:

```toml
name = "App"
uuid = "8f986787-14fe-4607-ba5d-fbff2944afa9"

[deps]
Priv = "ba13f791-ae1d-465a-978b-69c3ad90f72b"
Pub  = "c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1"
```

This project file implies the following `roots` map, if it was represented by a Julia dictionary:

```julia
roots = Dict(
    :App  => UUID("8f986787-14fe-4607-ba5d-fbff2944afa9"),
    :Priv => UUID("ba13f791-ae1d-465a-978b-69c3ad90f72b"),
    :Pub  => UUID("c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1"),
)
```

Given this `roots` map, in `App`'s code the statement `import Priv` will cause Julia to look up `roots[:Priv]`, which yields `ba13f791-ae1d-465a-978b-69c3ad90f72b`, the UUID of the `Priv` package that is to be loaded in that context. This UUID identifies which `Priv` package to load and use when the main application evaluates `import Priv`.

**The dependency graph** of a project environment is determined by the contents of the manifest file, if present. If there is no manifest file, `graph` is empty. A manifest file contains a stanza for each of a project's direct or indirect dependencies, including for each one, its UUID and a source tree hash or an explicit path to the source code. Consider the following example manifest file for `App`:

```toml
[[Priv]] # the private one
deps = ["Pub", "Zebra"]
uuid = "ba13f791-ae1d-465a-978b-69c3ad90f72b"
path = "deps/Priv"

[[Priv]] # the public one
uuid = "2d15fe94-a1f7-436c-a4d8-07a9a496e01c"
git-tree-sha1 = "1bf63d3be994fe83456a03b874b409cfd59a6373"
version = "0.1.5"

[[Pub]]
uuid = "c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1"
git-tree-sha1 = "9ebd50e2b0dd1e110e842df3b433cb5869b0dd38"
version = "2.1.4"

  [Pub.deps]
  Priv = "2d15fe94-a1f7-436c-a4d8-07a9a496e01c"
  Zebra = "f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62"

[[Zebra]]
uuid = "f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62"
git-tree-sha1 = "e808e36a5d7173974b90a15a353b564f3494092f"
version = "3.4.2"
```

This manifest file describes a possible complete dependency graph for the `App` project:

- There are two different `Priv` packages that the application needs—a private one which is a direct dependency and a public one which is an indirect dependency through `Pub`:
  * The private `Priv` depends on the `Pub` and `Zebra` packages.
  * The public `Priv` has no dependencies.
- The application also depends on the `Pub` package, which in turn depends on the public `Priv ` and the same `Zebra` package which the private `Priv` package depends on.

This dependency `graph` represented as a dictionary, looks like this:

```julia
graph = Dict{UUID,Dict{Symbol,UUID}}(
    # Priv – the private one:
    UUID("ba13f791-ae1d-465a-978b-69c3ad90f72b") => Dict{Symbol,UUID}(
        :Pub   => UUID("c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1"),
        :Zebra => UUID("f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62"),
    ),
    # Priv – the public one:
    UUID("2d15fe94-a1f7-436c-a4d8-07a9a496e01c") => Dict{Symbol,UUID}(),
    # Pub:
    UUID("c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1") => Dict{Symbol,UUID}(
        :Priv  => UUID("2d15fe94-a1f7-436c-a4d8-07a9a496e01c"),
        :Zebra => UUID("f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62"),
    ),
    # Zebra:
    UUID("f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62") => Dict{Symbol,UUID}(),
)
```

Given this dependency `graph`, when Julia sees `import Priv` in the `Pub` package—which has UUID `c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1`—it looks up:

```julia
graph[UUID("c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1")][:Priv]
```

and gets `2d15fe94-a1f7-436c-a4d8-07a9a496e01c`, which indicates that in the context of the `Pub` package, `import Priv` refers to the public `Priv` package, rather than the private one which the app depends on directly. This is how the name `Priv` can refer to different packages in the main project than it does in one of its package's dependencies, which allows for duplicate names in the package ecosystem.

What happens if `import Zebra` is evaluated in the main `App` code base? Since `Zebra` does not appear in the project file, the import will fail even though `Zebra` *does* appear in the manifest file. Moreover, if `import Zebra` occurs in the public `Priv` package—the one with UUID `2d15fe94-a1f7-436c-a4d8-07a9a496e01c`—then that would also fail since that `Priv` package has no declared dependencies in the manifest file and therefore cannot load any packages. The `Zebra` package can only be loaded by packages for which it appear as an explicit dependency in the manifest file: the  `Pub` package and one of the `Priv` packages.

**The paths map** of a project environment is also determined by the manifest file if present and is empty if there is no manifest. The path of a package `uuid` named `X` is determined by these two rules:

1. If the manifest stanza matching `uuid` has a `path` entry, use that path relative to the manifest file.
2. Otherwise, if the manifest stanza matching `uuid` has a `git-tree-sha1` entry, compute a deterministic hash function of `uuid` and `git-tree-sha1`—call it `slug`—and look for `packages/X/$slug` in each directory in the Julia `DEPOT_PATH` global array. Use the first such directory that exists.

If applying these rules doesn't find a loadable path, the package should be considered not installed and the system should raise an error or prompt the user to install the appropriate package version.

In the example manifest file above, to find the path of the first `Priv` package—the one with UUID `ba13f791-ae1d-465a-978b-69c3ad90f72b`—Julia looks for its stanza in the manifest file, sees that it has a `path` entry, looks at `deps/Priv` relative to the `App` project directory—let's suppose the `App` code lives in `/home/me/projects/App`—sees that `/home/me/projects/App/deps/Priv` exists and therefore loads `Priv` from there.

If, on the other hand, Julia was loading the *other* `Priv` package—the one with UUID `2d15fe94-a1f7-436c-a4d8-07a9a496e01c`—it finds its stanza in the manifest, see that it does *not* have a `path` entry, but that it does have a `git-tree-sha1` entry. It then computes the `slug` for this UUID/SHA-1 pair, which is `HDkr` (the exact details of this computation aren't important, but it is consistent and deterministic). This means that the path to this `Priv` package will be `packages/Priv/HDkr/src/Priv.jl` in one of the package depots. Suppose the contents of `DEPOT_PATH` is `["/home/me/.julia", "/usr/local/julia"]`; then Julia will look at the following paths to see if they exist:

1. `/home/me/.julia/packages/Priv/HDkr/src/Priv.jl`
2. `/usr/local/julia/packages/Priv/HDkr/src/Priv.jl`

Julia uses the first of these that exists to load the public `Priv` package.

Here is a representation of the `paths` map for the `App` project environment:

```julia
paths = Dict{Tuple{UUID,Symbol},String}(
    # Priv – the private one:
    (UUID("ba13f791-ae1d-465a-978b-69c3ad90f72b"), :Priv) =>
        # relative entry-point inside `App` repo:
        "/home/me/projects/App/deps/Priv/src/Priv.jl",
    # Priv – the public one:
    (UUID("2d15fe94-a1f7-436c-a4d8-07a9a496e01c"), :Priv) =>
        # package installed in the system depot:
        "/usr/local/julia/packages/Priv/HDkr/src/Priv.jl",
    # Pub:
    (UUID("c07ecb7d-0dc9-4db7-8803-fadaaeaf08e1"), :Pub) =>
        # package installed in the user depot:
        "/home/me/.julia/packages/Pub/oKpw/src/Pub.jl",
    # Zebra:
    (UUID("f7a24cb4-21fc-4002-ac70-f0e3a0dd3f62"), :Zebra) =>
        # package installed in the system depot:
        "/usr/local/julia/packages/Zebra/me9k/src/Zebra.jl",
)
```

This example map includes three different kinds of package locations:

1. The private `Priv` package is "[vendored](https://stackoverflow.com/a/35109534/659248)" inside the `App` repository.
2. The public `Priv` and `Zebra` packages are in the system depot, where packages installed and managed by the system administrator live. These are available to all users on the system.
3. The `Pub` package is in the user depot, where packages installed by the user live. These are only available to the user who installed them.

### Package directories

Package directories provide a kind of environment that approximates package loading in Julia 0.6 and earlier, and which resembles package loading in many other dynamic languages. The set of packages available in a package directory corresponds to the set of subdirectories it contains that look like packages: if `X/src/X.jl` is a file in a package directory, then `X` is considered to be a package and `X/src/X.jl` is the file Julia loads to get `X`. Which packages can "see" each other as dependencies depends on whether they contain project files, and if they do, on what appears in those project files' `[deps]` sections.

**The roots map** is determined by the subdirectories `X` of a package directory for which `X/src/X.jl` exists and whether `X/Project.toml` exists and has a top-level `uuid` entry. Specifically `:X => uuid` goes in `roots` for each such `X` where `uuid` is defined as:

1. If `X/Project.toml` exists and has a `uuid` entry, then `uuid` is that value.
2. If `X/Project.toml` exists and but does *not* have a top-level UUID entry, `uuid` is a dummy UUID generated by hashing the canonical path of `X/Project.toml`.
3. If `X/Project.toml` does not exist, then `uuid` is the all-zero [nil UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier#Nil_UUID).

**The dependency graph** of a project directory is determined by the presence and contents of project files in the subdirectory of each package. The rules are:

- If a package subdirectory has no project file, then it is omitted from `graph` and import statements in its code are treated as top-level, the same as the main project and REPL.
- If a package subdirectory has a project file, then the `graph` entry for its UUID is the `[deps]` map of the project file, which is considered to be empty if the section is absent.

As an example, suppose a package directory has the following structure and content:

```
Aardvark/
    src/Aardvark.jl:
        import Bobcat
        import Cobra

Bobcat/
    Project.toml:
        [deps]
        Cobra = "4725e24d-f727-424b-bca0-c4307a3456fa"
        Dingo = "7a7925be-828c-4418-bbeb-bac8dfc843bc"

    src/Bobcat.jl:
        import Cobra
        import Dingo

Cobra/
    Project.toml:
        uuid = "4725e24d-f727-424b-bca0-c4307a3456fa"
        [deps]
        Dingo = "7a7925be-828c-4418-bbeb-bac8dfc843bc"

    src/Cobra.jl:
        import Dingo

Dingo/
    Project.toml:
        uuid = "7a7925be-828c-4418-bbeb-bac8dfc843bc"

    src/Dingo.jl:
        # no imports
```

Here is a corresponding `roots` structure, represented as a dictionary:

```julia
roots = Dict{Symbol,UUID}(
    :Aardvark => UUID("00000000-0000-0000-0000-000000000000"), # no project file, nil UUID
    :Bobcat   => UUID("85ad11c7-31f6-5d08-84db-0a4914d4cadf"), # dummy UUID based on path
    :Cobra    => UUID("4725e24d-f727-424b-bca0-c4307a3456fa"), # UUID from project file
    :Dingo    => UUID("7a7925be-828c-4418-bbeb-bac8dfc843bc"), # UUID from project file
)
```

Here is the corresponding `graph` structure, represented as a dictionary:

```julia
graph = Dict{UUID,Dict{Symbol,UUID}}(
    # Bobcat:
    UUID("85ad11c7-31f6-5d08-84db-0a4914d4cadf") => Dict{Symbol,UUID}(
        :Cobra => UUID("4725e24d-f727-424b-bca0-c4307a3456fa"),
        :Dingo => UUID("7a7925be-828c-4418-bbeb-bac8dfc843bc"),
    ),
    # Cobra:
    UUID("4725e24d-f727-424b-bca0-c4307a3456fa") => Dict{Symbol,UUID}(
        :Dingo => UUID("7a7925be-828c-4418-bbeb-bac8dfc843bc"),
    ),
    # Dingo:
    UUID("7a7925be-828c-4418-bbeb-bac8dfc843bc") => Dict{Symbol,UUID}(),
)
```

A few general rules to note:

1. A package without a project file can depend on any top-level dependency, and since every package in a package directory is available at the top-level, it can import all packages in the environment.
2. A package with a project file cannot depend on one without a project file since packages with project files can only load packages in `graph` and packages without project files do not appear in `graph`.
3. A package with a project file but no explicit UUID can only be depended on by packages without project files since dummy UUIDs assigned to these packages are strictly internal.

Observe the following specific instances of these rules in our example:

* `Aardvark` can import on any of `Bobcat`, `Cobra` or `Dingo`; it does import `Bobcat` and `Cobra`.
* `Bobcat` can and does import both `Cobra` and `Dingo`, which both have project files with UUIDs and are declared as dependencies in `Bobcat`'s `[deps]` section.
* `Bobcat` cannot depend on `Aardvark` since `Aardvark` does not have a project file.
* `Cobra` can and does import `Dingo`, which has a project file and UUID, and is declared as a dependency in `Cobra`'s  `[deps]` section.
* `Cobra` cannot depend on `Aardvark` or `Bobcat` since neither have real UUIDs.
* `Dingo` cannot import anything because it has a project file without a `[deps]` section.

**The paths map** in a package directory is simple: it maps subdirectory names to their corresponding entry-point paths. In other words, if the path to our example project directory is `/home/me/animals` then the `paths` map could be represented by this dictionary:

```julia
paths = Dict{Tuple{UUID,Symbol},String}(
    (UUID("00000000-0000-0000-0000-000000000000"), :Aardvark) =>
        "/home/me/AnimalPackages/Aardvark/src/Aardvark.jl",
    (UUID("85ad11c7-31f6-5d08-84db-0a4914d4cadf"), :Bobcat) =>
        "/home/me/AnimalPackages/Bobcat/src/Bobcat.jl",
    (UUID("4725e24d-f727-424b-bca0-c4307a3456fa"), :Cobra) =>
        "/home/me/AnimalPackages/Cobra/src/Cobra.jl",
    (UUID("7a7925be-828c-4418-bbeb-bac8dfc843bc"), :Dingo) =>
        "/home/me/AnimalPackages/Dingo/src/Dingo.jl",
)
```

Since all packages in a package directory environment are, by definition, subdirectories with the expected entry-point files, their `paths` map entries always have this form.

### Environment stacks

The third and final kind of environment is one that combines other environments by overlaying several of them, making the packages in each available in a single composite environment. These composite environments are called *environment stacks*. The Julia `LOAD_PATH` global defines an environment stack—the environment in which the Julia process operates. If you want your Julia process to have access only to the packages in one project or package directory, make it the only entry in `LOAD_PATH`. It is often quite useful, however, to have access to some of your favorite tools—standard libraries, profilers, debuggers, personal utilities, etc.—even if they are not dependencies of the project you're working on. By pushing an environment containing these tools onto the load path, you immediately have access to them in top-level code without needing to add them to your project.

The mechanism for combining the `roots`, `graph` and `paths` data structures of the components of an environment stack is simple: they are simply merged as dictionaries, favoring earlier entries over later ones in the case of key collisions. In other words, if we have `stack = [env₁, env₂, …]` then we have:

```julia
roots = reduce(merge, reverse([roots₁, roots₂, …]))
graph = reduce(merge, reverse([graph₁, graph₂, …]))
paths = reduce(merge, reverse([paths₁, paths₂, …]))
```

The subscripted `rootsᵢ`, `graphᵢ` and `pathsᵢ` variables correspond to the subscripted environments, `envᵢ`, contained `stack`. The `reverse` is present because `merge` favors the last argument rather than first when there are collisions between keys in its argument dictionaries. That's all there is to stacked environments. There are a couple of noteworthy features of this design:

1. The *primary environment*—i.e. the first environment in a stack—is faithfully embedded in a stacked environment. The full dependency graph of the first environment in a stack is guaranteed to be included intact in the stacked environment including the same versions of all dependencies.
2. Packages in non-primary environments can end up using incompatible versions of their dependencies even if their own environments are entirely compatible. This can happen when one of their dependencies is shadowed by a version in an earlier environment in the stack.

Since the primary environment is typically the environment of a project you're working on, while environments later in the stack contain additional tools, this is the right tradeoff: it's better to break your dev tools but keep the project working. When such incompatibilities occur, you'll typically want to upgrade your dev tools to versions that are compatible with the main project.

## Conclusion

Federated package management and precise software reproducibility are difficult but worthy goals in a package system. In combination, these goals lead to a more complex package loading mechanism than most dynamic languages have, but it also yields scalability and reproducibility that is more commonly associated with static languages. Fortunately, most Julia users can remain oblivious to the technical details of code loading and simply use the built-in package manager to add a package `X` to the appropriate project and manifest files and then write `import X` to load `X` without a further thought.
