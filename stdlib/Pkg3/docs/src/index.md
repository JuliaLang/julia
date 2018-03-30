# Pkg3.jl

!!! warning
    This documentation is a work in progress and the information in it might be or become outdated.

Sections:

```@contents
Pages = [
    "index.md"]
```

## Introduction

Pkg3 is the standard package manager for Julia 1.0 and newer. Unlike traditional
package managers, which install and manage a single global set of packages, Pkg3
is designed around “environments”: independent sets of packages that can be
local to an individual project or shared and selected by name. The exact set of
packages and versions in an environment is captured in a _manifest file_ which
can be checked into a project repository and tracked in version control,
significantly improving reproducibility of projects. If you’ve ever tried to run
code you haven’t used in a while only to find that you can’t get anything to
work because you’ve updated or uninstalled some of the packages your project was
using, you’ll understand the motivation for this approach. In Pkg3, since each
project maintains its own independent set of package versions, you’ll never have
this problem again. Moreover, if you check out a project on a new system, you
can simply materialize the environment described by its manifest file and
immediately be up and running with a known-good set of dependencies.

Since environments are managed and updated independently from each other,
“dependency hell” is significantly alleviated in Pkg3. If you want to use the
latest and greatest version of some package in a new project but you’re stuck on
an older version in a different project, that’s no problem – since they have
separate environments they can just use different versions, which are both
installed at the same time in different locations on your system. The location
of each package version is canonical, so when environments use the same versions
of packages, they can share installations, avoiding unnecessary duplication of
the package. Old package versions that are no longer used by any environments
are periodically “garbage collected” by the package manager.

Pkg3’s approach to local environments may be familiar to people who have used
Python’s `virtualenv` or Ruby’s `bundler`. In Julia, instead of hacking the
language’s code loading mechanisms to support environments, we have the benefit
that Julia natively understands them. In addition, Julia environments are
“stackable”: you can overlay one environment with another and thereby have
access to additional packages outside of the primary environment. This makes it
easy to work on a project, which provides the primary environment, while still
having access to all your usual dev tools like profilers, debuggers, and so on,
just by having an environment including these dev tools later in the load path.

Last but not least, Pkg3 is designed to support federated package registries.
This means that it allows multiple registries managed by different parties to
interact seamlessly. In particular, this includes private registries which can
live behind corporate firewalls. You can install and update your own packages
from a private registry with exactly the same tools and workflows that you use
to install and manage official Julia packages. If you urgently need to apply a
hotfix for a public package that’s critical to your company’s product, you can
tag a private version of it in your company’s internal registry and get a fix to
your developers and ops teams quickly and easily without having to wait for an
upstream patch to be accepted and published. Once an official fix is published,
however, you can just upgrade your dependencies and you'll be back on an
official release again.

## Glossary

**Project:** a source tree with a standard layout, including a `src` directory
for the main body of Julia code, a `test` directory for testing the project,
`docs` for documentation files, and optionally a `build` directory for a build
script and its outputs. A project will typically also have a project file and
may optionally have a manifest file:

- **Project file:** a file in the root directory of a project, named
  `Project.toml` (or `JuliaProject.toml`) describing metadata about the project,
  including its name, UUID (for packages), authors, license, and the names and
  UUIDs of packages and libraries that it depends on.

- **Manifest file:** a file in the root directory of a project, named
  `Manifest.toml` (or `JuliaManifest.toml`) describing a complete dependency graph
  and exact versions of each package and library used by a project.

**Package:** a project which provides reusable functionality that can be used by
other Julia projects via `import X` or `using X`. A package should have a
project file with a `uuid` entry giving its package UUID. This UUID is used to
identify the package in projects that depend on it.

!!! note
    For legacy reasons it is possible to load a package without a project file or
    UUID from the REPL or the top-level of a script. It is not possible, however,
    to load a package with a project file or UUID from a project with them. Once
    you've loaded from a project file, everything needs a project file and UUID.

**Application:** a project which provides standalone functionality not intended
to be reused by other Julia projects. For example a web application or a
commmand-line utility. An application may have a UUID but does not need one. An
application may also provide global configuration options for packages it
depends on. Packages, on the other hand, may not provide global configuration
since that could conflict with the configuration of the main application.

!!! note
    **Projects _vs._ Packages _vs._ Applications:**

    1. Project is an umbrella term: packages and applications are kinds of projects.
    2. Packages should have UUIDs, applications can have a UUIDs but don't need them.
    3. Applications can provide global configuration, whereas packages cannot.

**Library (future work):** a compiled binary dependency (not written in Julia)
packaged to be used by a Julia project. These are currently typically built in-
place by a `deps/build.jl` script in a project’s source tree, but in the future
we plan to make libraries first-class entities directly installed and upgraded
by the package manager.

**Environment:** the combination of the top-level name map provided by a project
file combined with the dependency graph and map from packages to their entry points
provided by a manifest file. For more detail see the manual section on code loading.

- **Explicit environment:** an environment in the form of an explicit project
  file and an optional corresponding manifest file together in a directory. If the
  manifest file is absent then the implied dependency graph and location maps are
  empty.

- **Implicit environment:** an environment provided as a directory (without a
  project file or manifest file) containing packages with entry points of the form
  `X.jl`, `X.jl/src/X.jl` or `X/src/X.jl`. The top-level name map is implied by
  these entry points. The dependency graph is implied by the existence of project
  files inside of these package directories, e.g. `X.jl/Project.toml` or
  `X/Project.toml`. The dependencies of the `X` package are the dependencies in
  the corresponding project file if there is one. The location map is implied by
  the entry points themselves.

**Registry:** a source tree with a standard layout recording metadata about a
registered set of packages, the tagged versions of them which are available, and
which versions of packages are compatible or incompatible with each other. A
registry is indexed by package name and UUID, and has a directory for each
registered package providing the following metadata about it:

  - name – e.g. `DataFrames`
  - UUID – e.g. `a93c6f00-e57d-5684-b7b6-d8193f3e46c0`
  - authors – e.g. `Jane Q. Developer <jane@example.com>`
  - license – e.g. MIT, BSD3, or GPLv2
  - repository – e.g. `https://github.com/JuliaData/DataFrames.jl.git`
  - description – a block of text summarizing the functionality of a package
  - keywords – e.g. `data`, `tabular`, `analysis`, `statistics`
  - versions – a list of all registered version tags

For each registered version of a package, the following information is provided:

  - its semantic version number – e.g. `v1.2.3`
  - its git tree SHA-1 hash – e.g. `7ffb18ea3245ef98e368b02b81e8a86543a11103`
  - a map from names to UUIDs of dependencies
  - which versions of other packages it is compatible/incompatible with

Dependencies and compatiblity are stored in a compressed but human-readable
format using ranges of package verions.

**Depot:** a directory on a system where various package-related resources live,
including:

  - `environments`: shared named environments (e.g. `v0.7`, `devtools`)
  - `clones`: bare clones of package repositories
  - `compiled`: cached compiled package images (`.ji` files)
  - `config`: global configuration files (e.g. `startup.jl`)
  - `dev`: default directory for package development
  - `logs`: log files (e.g. `manifest_usage.toml`, `repl_history.jl`)
  - `packages`: installed package versions
  - `registries`: clones of registries (e.g. `Uncurated`)

**Load path:** a stack of environments where package identities, their
dependencies, and entry-points are searched for. The load path is controlled in
Julia by the `LOAD_PATH` global variable which is populated at startup based on
the value of the `JULIA_LOAD_PATH` environment variable. The first entry is your
primary environment, often the current project, while later entries provide
additional packages one may want to use from the REPL or top-level scripts.

**Depot path:** a stack of depot locations where the package manager, as well as
Julia's code loading mechanisms, look for registries, installed packages, named
environments, repo clones, cached compiled package images, and configuration
files. The depot path is controlled by the Julia `DEPOT_PATH` global variable
which is populated at startup based on the value of the `JULIA_DEPOT_PATH`
environment variable. The first entry is the “user depot” and should be writable
by and owned by the current user. The user depot is where: registries are
cloned, new package versions are installed, named environments are created and
updated, package repos are cloned, new compiled package image files are saved,
log files are written, development packages are checked out by default, and
global configuration data is saved. Later entries in the depot path are treated
as read-only and are appropriate for registries, packages, etc. installed and
managed by system administrators.


## Getting Started

The Pkg REPL-mode is entered using from the Julia REPL using the key `]`.
To return to the `julia>` prompt, either press backspace when the input line is empty or press Ctrl+C.
Help is available by calling `pkg> help`.

To generate files for a new project, use `pkg> generate`.

```
pkg> generate HelloWorld
```

This creates a new project `HelloWorld` with the following files (visualized with the external [`tree` command](https://linux.die.net/man/1/tree)):

```jl
julia> cd("HelloWorld")
shell> tree .
.
├── Project.toml
└── src
    └── HelloWorld.jl

1 directory, 2 files
```

The `Project.toml` file contains the name of the package, its unique UUID, its version, the author and eventual dependencies:

```toml
name = "HelloWorld"
uuid = "b4cd1eb8-1e24-11e8-3319-93036a3eb9f3"
version = "0.1.0"
author = ["Some One <someone@email.com>"]

[deps]
```

The content of `src/HelloWorld.jl` is:

```jl
module HelloWorld

greet() = print("Hello World!")

end # module
```

We can now load the project and use it:

```jl
julia> import HelloWorld

julia> HelloWorld.greet()
Hello World!
```

### Adding packages to the project

Let’s say we want to use the standard library package `Random` and the registered package `JSON` in our project.
We simply `add` these packages:

```
pkg> add Random JSON
 Resolving package versions...
  Updating "~/Documents/HelloWorld/Project.toml"
 [682c06a0] + JSON v0.17.1
 [9a3f8284] + Random
  Updating "~/Documents/HelloWorld/Manifest.toml"
 [34da2185] + Compat v0.57.0
 [682c06a0] + JSON v0.17.1
 [4d1e1d77] + Nullables v0.0.4
 ...
```

Both `Random` and `JSON` got added to the project’s `Project.toml` file, and the resulting dependencies got added to the `Manifest.toml` file.
The resolver has installed each package with the highest possible version, while still respecting the compatibility that each package enforce on its dependencies.

We can now use both `Random` and `JSON` in our project. Changing `src/HelloWorld.jl` to

```
module HelloWorld

import Random
import JSON

greet() = print("Hello World!")
greet_alien() = print("Hello ", Random.randstring(8))

end # module
```

and reloading the package, the new `greet_alien` function that uses `Random` can be used:

```
julia> HelloWorld.greet_alien()
Hello aT157rHV
```

Sometimes we might want to use the very latest, unreleased version of a package, or perhaps a specific branch in the package
git repository. We can use e.g. the `master` branch of `JSON` by specifying the branch after a `#` when adding the package:

```
pkg> add JSON#master
   Cloning package from https://github.com/JuliaIO/JSON.jl.git
 Resolving package versions...
  Updating "~/Documents/HelloWorld/Project.toml"
 [682c06a0] ~ JSON v0.17.1 ⇒ v0.17.1+ #master
  Updating "~/Documents/HelloWorld/Manifest.toml"
 [682c06a0] ~ JSON v0.17.1 ⇒ v0.17.1+ #master
```

If we want to use a package that has not been registered in a registry, we can `add` its git repository url:

```
pkg> add https://github.com/fredrikekre/ImportMacros.jl
  Cloning package from https://github.com/fredrikekre/ImportMacros.jl
 Resolving package versions...
Downloaded MacroTools ─ v0.4.0
  Updating "~/Documents/HelloWorld/Project.toml"
 [5adcef86] + ImportMacros v0.1.0 #master
   Updating "~/Documents/HelloWorld/Manifest.toml"
 [5adcef86] + ImportMacros v0.1.0 #master
 [1914dd2f] + MacroTools v0.4.0
```

The dependencies of the unregistered package (here `MacroTools`) got installed.
For unregistered packages we could have given a branch (or commit SHA) to track using `#`, just like for registered packages.

## Developing packages

Let’s say we found a bug in `JSON` that we want to fix. We can get the full git-repo using the `develop` command

```
pkg> develop JSON
    Cloning package from https://github.com/JuliaIO/JSON.jl.git
  Resolving package versions...
   Updating "~/Documents/HelloWorld/Project.toml"
 [682c06a0] + JSON v0.17.1+ [~/.julia/dev/JSON]
...
```

By default, the package get cloned to the `~/.julia/dev` folder but can also be set by the `JULIA_PKG_DEVDIR` environment variable.
When we have fixed the bug and checked that `JSON` now works correctly with out project, we can make a PR to the `JSON` repository.
When a new release of `JSON` is made, we can go back to using the versioned `JSON` using the command `free` and `update` (see next section):

```
pkg> free JSON
 Resolving package versions...
  Updating "~/Documents/HelloWorld/Project.toml"
 [682c06a0] ~ JSON v0.17.1+ #master ⇒ v0.17.1
  Updating "~/Documents/HelloWorld/Manifest.toml"
 [682c06a0] ~ JSON v0.17.1+ #master ⇒ v0.17.1
```

It is also possible to give a local path as the argument to `develop` which will not clone anything but simply use that directory for the package.

Developing a non registered package is done by giving the git-repo url as an argument to `develop`.

### Updating dependencies

When new versions of packages the project is using  are released, it is a good idea to update. Simply calling `up` will try to update *all* the dependencies of the project. Sometimes this is not what you want. You can specify a subset of the dependencies to upgrade by giving them as arguments to `up`, e.g:

```
pkg> up JSON
```

The version of all other dependencies will stay the same. If you only want to update the minor version of packages, to reduce the risk that your project breaks, you can give the `--minor` flag, e.g:

```
pkg> up --minor JSON
```

Packages that track a branch are not updated when a minor upgrade is done.
Developed packages are never touched by the package manager.

If you just want install the packages that are given by the current `Manifest.toml` use

```
pkg> up --manifest --fixed
```

### Preview mode

If you just want to see the effects of running a command, but not change your state you can `preview` a command.
For example:

```
pkg> preview add Plot
```

or

```
pkg> preview up
```

will show you the effects adding `Plots`, or doing a full upgrade, respectively, would have on your project.
However, nothing would be installed and your `Project.toml` and `Manfiest.toml` are untouched.


### Using someone elses project.

Simple clone their project using e.g. `git clone`, `cd` to the project directory and call

```
pkg> up --manifest --fixed
```

This will install the packages at the same state that the project you cloned was using.
