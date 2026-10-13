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

# Exercise virtual metadata paths without changing executable constants or cache identities.
@testset "pkgimage integration" begin
    mktempdir() do tmp
        tmp = realpath(tmp)
        real = joinpath(tmp, "real")
        mapped = joinpath(tmp, "missing")
        pkgdir = joinpath(real, "TestPkgMap")
        pkgsrc = joinpath(pkgdir, "src")
        mkpath(pkgsrc)
        write(joinpath(pkgdir, "Project.toml"), """
        name = "TestPkgMap"
        uuid = "d31f4c31-b3ea-4e4e-bd7c-90d8bbd0d165"
        version = "0.1.0"

        [deps]
        InteractiveUtils = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
        """)
        write(joinpath(pkgdir, "asset.txt"), "asset contents")
        write(joinpath(pkgdir, "extra.jl"), "extra() = 42\n")
        write(joinpath(pkgsrc, "TestPkgMap.jl"), """
        module TestPkgMap
        using InteractiveUtils
        const BUILT_FILE = @__FILE__
        const SYM = Symbol(@__FILE__)
        const EXPR = Expr(:call, :identity, SYM)
        \"adds one\"
        f(x) = x + 1
        f(1)
        const LLVM_IR = sprint(io -> code_llvm(io, f, (Int,); dump_module=true, raw=true))
        macro qm()
            quote
                @__DIR__
            end
        end
        logs_one(x) = (@warn \"boom\"; x)
        logs_override() = @warn \"override\" _file=@__FILE__
        srcdir(x) = (x, @__DIR__)
        uncompiled() = @__DIR__
        Base.@noinline same_symbol(x::Symbol) = x === SYM
        Base.@noinline expr_value(x) = (x, EXPR)
        asset() = read(joinpath(@__DIR__, \"..\", \"asset.txt\"), String)
        Base.include_dependency(joinpath(@__DIR__, \"..\", \"asset.txt\"))
        include(\"../extra.jl\")
        precompile(srcdir, (Int,))
        precompile(same_symbol, (Symbol,))
        precompile(expr_value, (Int,))
        precompile(logs_one, (Int,))
        end
        """)

        pathsep = Sys.iswindows() ? ";" : ":"
        env = ["JULIA_DEPOT_PATH" => joinpath(tmp, "depot") * pathsep,
               "JULIA_LOAD_PATH" => real * pathsep]
        encode(p) = replace(replace(replace(p, "%" => "%#"), "=" => "%+"), ":" => "%.")
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -e 'using TestPkgMap'`,
                     env..., "BUILD_PATH_PREFIX_MAP" => "$(encode(mapped))=$(encode(real))")
        @test success(pipeline(cmd; stdout, stderr))

        code = """
        using TestPkgMap, Test, Logging
        mapped = $(repr(mapped))
        real = $(repr(real))
        src = $(repr(pkgsrc))
        pkg = Base.identify_package("TestPkgMap")
        @test !ispath(mapped)
        @test Base.isprecompiled(pkg)
        @test startswith(String(first(methods(TestPkgMap.f)).file), mapped)
        @test startswith(String(Base.moduleloc(TestPkgMap).file), mapped)
        function check_di(di)
            di isa Core.DebugInfo || return true
            di.def isa Symbol && startswith(String(di.def), real) && return false
            check_di(di.linetable) && all(check_di, di.edges)
        end
        m = first(methods(TestPkgMap.f))
        mi = m.specializations isa Core.MethodInstance ? m.specializations :
             first(x for x in m.specializations if x !== nothing)
        @test check_di(mi.cache.debuginfo)
        @test TestPkgMap.BUILT_FILE == joinpath(src, "TestPkgMap.jl")
        @test last(Base.inferencebarrier(TestPkgMap.srcdir)(1)) == src
        @test last(Base.inferencebarrier(TestPkgMap.srcdir)(1.0)) == src
        @test last(TestPkgMap.srcdir(1)) == src
        @test last(TestPkgMap.srcdir(1.0)) == src
        @test TestPkgMap.uncompiled() == src
        @test TestPkgMap.@qm() == src
        @test TestPkgMap.same_symbol(Base.inferencebarrier(TestPkgMap.SYM))
        @test last(Base.inferencebarrier(TestPkgMap.expr_value)(1)) === TestPkgMap.EXPR
        @test last(TestPkgMap.expr_value(1.0)) === TestPkgMap.EXPR
        @test TestPkgMap.asset() == "asset contents"
        @test TestPkgMap.extra() == 42
        logger = Test.TestLogger()
        with_logger(logger) do
            Base.inferencebarrier(TestPkgMap.logs_one)(1)
            TestPkgMap.logs_one(1.0)
            TestPkgMap.logs_override()
        end
        @test all(log -> startswith(log.file, mapped), logger.logs[1:2])
        @test logger.logs[3].file == TestPkgMap.BUILT_FILE
        cachefile = first(Base.find_all_in_cache_path(pkg))
        includes = Base.parse_cache_header(cachefile)[2][1]
        @test all(inc -> isfile(inc.filename), includes)
        @test any(inc -> inc.filename == joinpath(dirname(src), "asset.txt"), includes)
        @test Base.read_dependency_src(cachefile, joinpath(src, "TestPkgMap.jl")) ==
              read(joinpath(src, "TestPkgMap.jl"), String)
        @test Base.read_dependency_src(cachefile, joinpath(dirname(src), "extra.jl")) ==
              read(joinpath(dirname(src), "extra.jl"), String)
        docpath = first(values(first(values(Base.Docs.meta(TestPkgMap))).docs)).data[:path]
        @test startswith(docpath, mapped)
        @test contains(TestPkgMap.LLVM_IR, "!DIFile")
        @test contains(TestPkgMap.LLVM_IR, $(repr(escape_string(mapped))))
        @test !contains(TestPkgMap.LLVM_IR, $(repr(escape_string(real))))
        """
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -e $code`, env..., "BUILD_PATH_PREFIX_MAP" => "")
        @test success(pipeline(cmd; stdout, stderr))

        relocated = joinpath(tmp, "relocated")
        mv(real, relocated)
        code = """
        using Test
        pkg = Base.identify_package("TestPkgMap")
        @test Base.isprecompiled(pkg)
        @test Base.isrelocatable(pkg)
        src = Base.locate_package(pkg)
        asset = joinpath(dirname(src), "..", "asset.txt")
        contents = read(asset, String)
        write(asset, "changed asset")
        @test !Base.isprecompiled(pkg)
        write(asset, contents)
        @test Base.isprecompiled(pkg)
        extra = joinpath(dirname(src), "..", "extra.jl")
        rm(extra)
        @test !Base.isprecompiled(pkg)
        """
        cmd = addenv(`$(Base.julia_cmd()) --startup-file=no -e $code`, env...,
                     "JULIA_LOAD_PATH" => relocated * pathsep, "BUILD_PATH_PREFIX_MAP" => "")
        @test success(pipeline(cmd; stdout, stderr))
    end
end

# The build's source cache must contain every mapped key, even with independent prefix mappings.
@testset "base source cache" begin
    cachefile = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base.cache")
    open(cachefile) do io
        for (_, filename) in Base._included_files
            seekstart(io)
            @test Base._read_dependency_src(io, filename) isa String
        end
    end
end
