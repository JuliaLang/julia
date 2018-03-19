# Pkg3.jl

!!! warning
    This documentation is a work in progress and the information in it might be or become outdated.

Sections:

```@contents
Pages = [
    "index.md"]
```

## Introduction

Pkg3 is the package manager for Julia.

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

Let's say we want to use the standard library package `Random` and the registered package `JSON` in our project.
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

Both `Random` and `JSON` got added to the project's `Project.toml` file, and the resulting dependencies got added to the `Manifest.toml` file.
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

Let's say we found a bug in `JSON` that we want to fix. We can get the full git-repo using the `develop` command

```
pkg> develop JSON
    Cloning package from https://github.com/JuliaIO/JSON.jl.git
  Resolving package versions...
   Updating "~/.julia/environments/v0.7/Project.toml"
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
