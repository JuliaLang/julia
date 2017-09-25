# Install dependencies needed to build the documentation.
ENV["JULIA_PKGDIR"] = joinpath(@__DIR__, "deps")
Pkg.init()
cp(joinpath(@__DIR__, "REQUIRE"), Pkg.dir("REQUIRE"); remove_destination = true)
Pkg.update()
Pkg.resolve()

using Documenter

# Include the `build_sysimg` file.

baremodule GenStdLib end
isdefined(:build_sysimg) || @eval module BuildSysImg
    include(joinpath(@__DIR__, "..", "contrib", "build_sysimg.jl"))
end

# Documenter Setup.

# make links for stdlib package docs
if Sys.iswindows()
    cp("../stdlib/DelimitedFiles/docs/src/index.md", "src/stdlib/delimitedfiles.md")
else
    symlink("../../../stdlib/DelimitedFiles/docs/src/index.md", "src/stdlib/delimitedfiles.md")
end

const PAGES = [
    "Home" => "index.md",
    "Manual" => [
        "manual/introduction.md",
        "manual/getting-started.md",
        "manual/variables.md",
        "manual/integers-and-floating-point-numbers.md",
        "manual/mathematical-operations.md",
        "manual/complex-and-rational-numbers.md",
        "manual/strings.md",
        "manual/functions.md",
        "manual/control-flow.md",
        "manual/variables-and-scoping.md",
        "manual/types.md",
        "manual/methods.md",
        "manual/constructors.md",
        "manual/conversion-and-promotion.md",
        "manual/interfaces.md",
        "manual/modules.md",
        "manual/documentation.md",
        "manual/metaprogramming.md",
        "manual/arrays.md",
        "manual/linear-algebra.md",
        "manual/networking-and-streams.md",
        "manual/parallel-computing.md",
        "manual/dates.md",
        "manual/interacting-with-julia.md",
        "manual/running-external-programs.md",
        "manual/calling-c-and-fortran-code.md",
        "manual/handling-operating-system-variation.md",
        "manual/environment-variables.md",
        "manual/embedding.md",
        "manual/packages.md",
        "manual/profile.md",
        "manual/stacktraces.md",
        "manual/performance-tips.md",
        "manual/workflow-tips.md",
        "manual/style-guide.md",
        "manual/faq.md",
        "manual/noteworthy-differences.md",
        "manual/unicode-input.md",
    ],
    "Standard Library" => [
        "stdlib/base.md",
        "stdlib/collections.md",
        "stdlib/math.md",
        "stdlib/numbers.md",
        "stdlib/strings.md",
        "stdlib/arrays.md",
        "stdlib/parallel.md",
        "stdlib/linalg.md",
        "stdlib/constants.md",
        "stdlib/file.md",
        "stdlib/delimitedfiles.md",
        "stdlib/io-network.md",
        "stdlib/punctuation.md",
        "stdlib/sort.md",
        "stdlib/pkg.md",
        "stdlib/dates.md",
        "stdlib/iterators.md",
        "stdlib/test.md",
        "stdlib/c.md",
        "stdlib/libc.md",
        "stdlib/libdl.md",
        "stdlib/profile.md",
        "stdlib/stacktraces.md",
        "stdlib/simd-types.md",
    ],
    "Developer Documentation" => [
        "devdocs/reflection.md",
        "Documentation of Julia's Internals" => [
            "devdocs/init.md",
            "devdocs/ast.md",
            "devdocs/types.md",
            "devdocs/object.md",
            "devdocs/eval.md",
            "devdocs/callconv.md",
            "devdocs/compiler.md",
            "devdocs/functions.md",
            "devdocs/cartesian.md",
            "devdocs/meta.md",
            "devdocs/subarrays.md",
            "devdocs/sysimg.md",
            "devdocs/llvm.md",
            "devdocs/stdio.md",
            "devdocs/boundscheck.md",
            "devdocs/locks.md",
            "devdocs/offset-arrays.md",
            "devdocs/libgit2.md",
            "devdocs/require.md",
            "devdocs/inference.md",
        ],
        "Developing/debugging Julia's C code" => [
            "devdocs/backtraces.md",
            "devdocs/debuggingtips.md",
            "devdocs/valgrind.md",
            "devdocs/sanitizers.md",
        ]
    ],
]

using DelimitedFiles

makedocs(
    build     = joinpath(pwd(), "_build/html/en"),
    modules   = [Base, Core, BuildSysImg, DelimitedFiles],
    clean     = false,
    doctest   = "doctest" in ARGS,
    linkcheck = "linkcheck" in ARGS,
    linkcheck_ignore = ["https://bugs.kde.org/show_bug.cgi?id=136779"], # fails to load from nanosoldier?
    strict    = true,
    checkdocs = :none,
    format    = "pdf" in ARGS ? :latex : :html,
    sitename  = "The Julia Language",
    authors   = "The Julia Project",
    analytics = "UA-28835595-6",
    pages     = PAGES,
    html_prettyurls = ("deploy" in ARGS),
)

if "deploy" in ARGS
    # Only deploy docs from 64bit Linux to avoid committing multiple versions of the same
    # docs from different workers.
    (Sys.ARCH === :x86_64 && Sys.KERNEL === :Linux) || return

    # Since the `.travis.yml` config specifies `language: cpp` and not `language: julia` we
    # need to manually set the version of Julia that we are deploying the docs from.
    ENV["TRAVIS_JULIA_VERSION"] = "nightly"

    deploydocs(
        repo = "github.com/JuliaLang/julia.git",
        target = "_build/html/en",
        dirname = "en",
        deps = nothing,
        make = nothing,
    )
end
