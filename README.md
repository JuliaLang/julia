# Pkg3

<!--
[![Build Status](https://travis-ci.org/StefanKarpinski/Pkg3.jl.svg?branch=master)](https://travis-ci.org/StefanKarpinski/Pkg3.jl)

[![Coverage Status](https://coveralls.io/repos/StefanKarpinski/Pkg3.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/StefanKarpinski/Pkg3.jl?branch=master)

[![codecov.io](http://codecov.io/github/StefanKarpinski/Pkg3.jl/coverage.svg?branch=master)](http://codecov.io/github/StefanKarpinski/Pkg3.jl?branch=master)
-->

Next-generation package manager for Julia. Very alpha.

Before you can use this, install its dependencies:

    Pkg.clone("https://github.com/StefanKarpinski/TOML.jl.git")
    Pkg.add("SHA")

To load package metadata from `~/.julia/vX.Y/METADATA`:

    include("bin/loadmeta.jl")

To generate a Pkg3-style registry from loaded metadata (`~/.julia/registries/Uncurated`):

    include("bin/generate.jl")

Load `Pkg3` in Julia 0.6 (0.6 only with precompilation off) to add packages and load them:

    $ path/to/julia.6/julia --compilecache=no

    julia> using Pkg3

    julia> Pkg3.add("JSON")
    INFO: Resolving package UUIDs
    INFO: Resolving package versions
    INFO: Updating config file /Users/stefan/projects/Pkg3.jl/Config.toml
    INFO: Updating manifest file /Users/stefan/projects/Pkg3.jl/Manifest.toml

    julia> using JSON

    julia> JSON.parse("[1,2,3]")
    3-element Array{Any,1}:
    1
    2
    3
