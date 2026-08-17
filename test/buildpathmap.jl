# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for BUILD_PATH_PREFIX_MAP support in image serialization
# (https://reproducible-builds.org/specs/build-path-prefix-map/)

using Test

# parses the given map fresh on every call (the env value is cached per-process)
mapbp(map, path) = ccall(:jl_test_map_build_path, Any, (Any, Any), map, path)::String

@testset "basic mapping" begin
    @test mapbp("julia=/build/dir", "/build/dir/base/foo.jl") == "julia/base/foo.jl"
    @test mapbp("julia=/build/dir", "/build/dir") == "julia"
    @test mapbp("julia=/build/dir", "/other/dir/foo.jl") == "/other/dir/foo.jl" # no match
    @test mapbp("", "/build/dir/foo.jl") == "/build/dir/foo.jl" # empty map
end

@testset "component-boundary matching" begin
    @test mapbp("a=/path/to/a", "/path/to/aa/b") == "/path/to/aa/b" # must not match
    @test mapbp("a=/path/to/a", "/path/to/a/b") == "a/b"
    @test mapbp("a=/path/to/a/", "/path/to/a/b") == "a/b" # trailing sep on source
    @test mapbp("root=/", "/b") == "rootb" # source is fs root; spec: target + remainder
end

@testset "rightmost priority" begin
    @test mapbp("first=/build:second=/build", "/build/x") == "second/x"
    @test mapbp("outer=/build:inner=/build/sub", "/build/sub/x") == "inner/x"
    @test mapbp("inner=/build/sub:outer=/build", "/build/sub/x") == "outer/sub/x"
end

@testset "escaping" begin
    @test mapbp("t%.arget=/pa%.th", "/pa:th/x") == "t:arget/x"   # %. => :
    @test mapbp("t%+arget=/pa%+th", "/pa=th/x") == "t=arget/x"   # %+ => =
    @test mapbp("t%#arget=/pa%#th", "/pa%th/x") == "t%arget/x"   # %# => %
end

@testset "empty segments and empty prefixes" begin
    @test mapbp(":julia=/build::", "/build/x") == "julia/x" # empty subsequences ignored
    # an empty source prefix matches at the root component boundary, i.e. absolute
    # paths only: mapping relative paths would corrupt Base's relative method files
    @test mapbp("rel=", "/abs/x") == "rel/abs/x"
    @test mapbp("rel=", "relative/x") == "relative/x"
    @test mapbp("=/build", "/build/x") == "/x" # empty target strips the prefix
end

@static if Sys.iswindows()
@testset "windows separators and drive letters" begin
    # drive-letter colons are escaped as %. per the spec
    @test mapbp("julia=C%.\\build\\julia", "C:\\build\\julia\\base\\foo.jl") == "julia\\base\\foo.jl"
    @test mapbp("julia=C%.\\build\\julia", "C:\\build\\juliax\\foo.jl") == "C:\\build\\juliax\\foo.jl"
    @test mapbp("julia=C%./build/julia", "C:/build/julia/base/foo.jl") == "julia/base/foo.jl"
    # separators in the source prefix match either spelling
    @test mapbp("julia=C%./build/julia", "C:\\build\\julia\\base\\foo.jl") == "julia\\base\\foo.jl"
    @test mapbp("julia=C%.\\build\\julia", "C:/build/julia/base/foo.jl") == "julia/base/foo.jl"
    # matching is byte-exact: no case folding
    @test mapbp("julia=C%.\\Build", "C:\\build\\x") == "C:\\build\\x"
end
end

@testset "malformed maps invalidate the whole variable" begin
    @test_throws ErrorException mapbp("nopair", "/x")               # no '='
    @test_throws ErrorException mapbp("a=/ok:nopair", "/x")         # one bad pair poisons all
    @test_throws ErrorException mapbp("a=b=c", "/x")                # unescaped '=' in source
    @test_throws ErrorException mapbp("a%zb=/x", "/x")              # invalid escape
    @test_throws ErrorException mapbp("a%=/x", "/x")                # trailing '%'
    # '@'-prefixed targets are reserved for the loader's internal aliases (@depot)
    @test_throws ErrorException mapbp("@depot/stable=/build", "/build/x")
    @test_throws ErrorException mapbp("@foo=/build", "/build/x")
end

if !Sys.iswindows() # symlink-based; the parser/matcher tests above cover Windows
@testset "pkgimage integration" begin
    mktempdir() do tmp
        tmp = realpath(tmp)
        real = joinpath(tmp, "real")
        mapped = joinpath(tmp, "mapped")
        pkgsrc = joinpath(real, "TestPkgMap", "src")
        mkpath(pkgsrc)
        symlink(real, mapped)

        write(joinpath(real, "TestPkgMap", "Project.toml"), """
        name = "TestPkgMap"
        uuid = "d31f4c31-b3ea-4e4e-bd7c-90d8bbd0d165"
        version = "0.1.0"

        [deps]
        InteractiveUtils = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
        """)
        write(joinpath(pkgsrc, "TestPkgMap.jl"), """
        module TestPkgMap
        using InteractiveUtils
        const BUILT_FILE = @__FILE__   # parse-time value: must NOT be remapped
        "adds one"
        f(x) = x + 1
        f(1)
        # native code debug info (DIFile) is mapped too; capture IR emitted while
        # generating output, where the map is active
        const LLVM_IR = sprint(io -> code_llvm(io, f, (Int,); dump_module=true, debuginfo=:source))
        # a quote body puts this file's path into the macro method's roots as
        # LineNumberNode file symbols
        macro qm()
            quote
                1 + 1
            end
        end
        # logging macros bake String(__source__.file) literals into the caller's roots
        logs_one(x) = (@warn "boom" maxlog=1; x)
        # function-body @__DIR__ literals are used for runtime file access (e.g.
        # Documenter locating assets); pkgimage native code must keep them unmapped
        srcdir() = @__DIR__
        srcdir()
        end
        """)

        depot = joinpath(tmp, "depot")
        env = ["JULIA_DEPOT_PATH" => depot * Base.Filesystem.pathsep(),
               "JULIA_LOAD_PATH" => real * Base.Filesystem.pathsep()]

        # precompile with the map active
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -e 'using TestPkgMap'`,
                     env..., "BUILD_PATH_PREFIX_MAP" => "$mapped=$real")
        @test success(pipeline(cmd; stdout, stderr))

        # fresh process without the variable: load and inspect what was serialized
        code = """
        using TestPkgMap
        mapped = raw"$mapped"
        real = raw"$real"
        m = first(methods(TestPkgMap.f))
        startswith(String(m.file), mapped) || error("method file not mapped: ", m.file)
        startswith(String(Base.moduleloc(TestPkgMap).file), mapped) || error("module file not mapped")
        function check_di(di, bad)
            di isa Core.DebugInfo || return true
            d = di.def
            d isa Symbol && startswith(String(d), bad) && return false
            check_di(di.linetable, bad) || return false
            all(check_di(e, bad) for e in di.edges)
        end
        mi = m.specializations isa Core.MethodInstance ? m.specializations :
             first(x for x in m.specializations if x !== nothing)
        check_di(mi.cache.debuginfo, real) || error("debuginfo contains unmapped path")
        startswith(TestPkgMap.BUILT_FILE, real) || error("parse-time @__FILE__ was remapped")
        TestPkgMap.srcdir() == joinpath(real, "TestPkgMap", "src") ||
            error("function-body @__DIR__ was remapped: ", TestPkgMap.srcdir())
        isfile(String(m.file)) || error("mapped path does not resolve")
        Base.isprecompiled(Base.identify_package("TestPkgMap")) || error("cache considered stale")
        cachefile = first(Base.find_all_in_cache_path(Base.identify_package("TestPkgMap")))
        includes = Base.parse_cache_header(cachefile)[2][1]
        any(inc -> startswith(inc.filename, mapped), includes) || error("header deps not mapped")
        any(inc -> startswith(inc.filename, real), includes) && error("header deps leak real path")
        docpath = first(values(first(values(Base.Docs.meta(TestPkgMap))).docs)).data[:path]
        startswith(docpath, mapped) || error("docstring path not mapped: ", docpath)
        difile = joinpath(mapped, "TestPkgMap", "src", "TestPkgMap.jl")
        contains(TestPkgMap.LLVM_IR, difile) || error("native debug info (DIFile) not mapped")
        contains(TestPkgMap.LLVM_IR, real) && error("native debug info leaks real path")
        # path-like values in a method's roots, including LineNumberNode files
        # inside quoted Expr roots
        function roots_paths(m)
            out = String[]
            walk(r) = r isa Symbol ? push!(out, String(r)) :
                      r isa String ? push!(out, r) :
                      r isa LineNumberNode ? (r.file isa Symbol ? push!(out, String(r.file)) : out) :
                      r isa Expr ? foreach(walk, r.args) :
                      r isa QuoteNode ? walk(r.value) : nothing
            foreach(walk, m.roots)
            return out
        end
        mac_paths = roots_paths(first(methods(getglobal(TestPkgMap, Symbol("@qm")))))
        any(p -> startswith(p, mapped), mac_paths) ||
            error("quoted LineNumberNode file not mapped in macro roots")
        mw_paths = roots_paths(first(methods(TestPkgMap.logs_one)))
        any(p -> startswith(p, mapped), mw_paths) ||
            error("logging __source__ file literal not mapped in roots")
        for p in [mac_paths; mw_paths]
            startswith(p, real) && error("method roots leak real path: ", p)
        end
        println("ok")
        """
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -e $code`, env...)
        @test read(pipeline(cmd; stderr), String) == "ok\n"
    end
end
end
