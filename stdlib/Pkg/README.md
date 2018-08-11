# Pkg

[![Build Status](https://travis-ci.org/JuliaLang/Pkg.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Pkg.jl) [![Build status](https://ci.appveyor.com/api/projects/status/ywiwk98gvye1ov6x/branch/master?svg=true)](https://ci.appveyor.com/project/KristofferC/pkg3-jl-li0m6/branch/master) [![](https://img.shields.io/badge/docs-latest-blue.svg)](https://julialang.github.io/Pkg.jl/latest/) [![codecov](https://codecov.io/gh/JuliaLang/Pkg.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaLang/Pkg.jl)

Next-generation package manager for Julia. Still quite alpha. The last commit that works on 0.6 is [4bd137919](https://github.com/JuliaLang/Pkg.jl/commit/4bd1379195ce10056c26fb689f732907dd4f09fa).

Pkg is available from the Julia nightly build or by building the Julia master branch.


#### Using the development version of Pkg.jl

If you want to develop this package do the following steps:
- Clone the repo anywhere.
- Remove the `uuid = ` line from the `Project.toml` file.
- Change the current directory to the Pkg repo you just cloned.
- `import Pkg` will now load the files in the cloned repo instead of the Pkg stdlib .
- To test your changes, simple `include("test/runtests.jl")`.
