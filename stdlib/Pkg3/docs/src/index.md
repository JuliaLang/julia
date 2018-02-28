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

The Pkg REPL-mode is entered using from the Julia REPL using the key `]`. To generate the files for a new project, use `pkg> generate`.

```
pkg> generate HelloWorld
```

This creates a new project `HelloWorld` with the following files

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