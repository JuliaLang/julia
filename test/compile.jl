# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

function redirected_stderr(expected)
    rd, wr = redirect_stderr()
    t = @async begin
        read = readstring(rd) # also makes sure the kernel isn't being forced to buffer the output
        if !contains(read, expected)
            @show expected
            @show read
            @test false
        end
        nothing
    end
    return t
end

olderr = STDERR
dir = mktempdir()
dir2 = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(Base.LOAD_CACHE_PATH, 1, dir)
Foo_module = :Foo4b3a94a1a081a8cb
try
    Foo_file = joinpath(dir, "$Foo_module.jl")

    write(Foo_file,
          """
          __precompile__(true)

          module $Foo_module
              # test that docs get reconnected
              @doc "foo function" foo(x) = x + 1
              include_dependency("foo.jl")
              include_dependency("foo.jl")
              module Bar
                  @doc "bar function" bar(x) = x + 2
                  include_dependency("bar.jl")
              end

              # test for creation of some reasonably complicated type
              immutable MyType{T} end
              const t17809s = Any[
                    Tuple{
                        Type{Ptr{MyType{i}}},
                        Array{Ptr{MyType{MyType{:sym}()}}(0), 0},
                        Val{Complex{Int}(1, 2)},
                        Val{3},
                        Val{nothing}}
                    for i = 0:25]

              # test that types and methods get reconnected correctly
              # issue 16529 (adding a method to a type with no instances)
              (::Task)(::UInt8, ::UInt16, ::UInt32) = 2

              # issue 16471 (capturing references to a kwfunc)
              Base.Test.@test_throws ErrorException Core.kwfunc(Base.nothing)
              Base.nothing(::UInt8, ::UInt16, ::UInt32; x = 52) = x
              const nothingkw = Core.kwfunc(Base.nothing)

              # issue 16908 (some complicated types and external method definitions)
              abstract CategoricalPool{T, R <: Integer, V}
              abstract CategoricalValue{T, R <: Integer}
              immutable NominalPool{T, R <: Integer, V} <: CategoricalPool{T, R, V}
                  index::Vector{T}
                  invindex::Dict{T, R}
                  order::Vector{R}
                  ordered::Vector{T}
                  valindex::Vector{V}
              end
              immutable NominalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              immutable OrdinalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              (::Union{Type{NominalValue}, Type{OrdinalValue}})() = 1
              (::Union{Type{NominalValue{T}}, Type{OrdinalValue{T}}}){T}() = 2
              (::Type{Vector{NominalValue{T, R}}}){T, R}() = 3
              (::Type{Vector{NominalValue{T, T}}}){T}() = 4
              (::Type{Vector{NominalValue{Int, Int}}})() = 5
          end
          """)
    @test_throws ErrorException Core.kwfunc(Base.nothing) # make sure `nothing` didn't have a kwfunc (which would invalidate the attempted test)

    # Issue #12623
    @test __precompile__(true) === nothing

    Base.require(Foo_module)
    cachefile = joinpath(dir, "$Foo_module.ji")

    # use _require_from_serialized to ensure that the test fails if
    # the module doesn't reload from the image:
    t = redirected_stderr("WARNING: replacing module Foo4b3a94a1a081a8cb.\nWARNING: Method definition ")
    try
        @test isa(Base._require_from_serialized(myid(), Foo_module, cachefile, #=broadcast-load=#false), Array{Any,1})
    finally
        close(STDERR)
        redirect_stderr(olderr)
    end
    wait(t)

    let Foo = eval(Main, Foo_module)
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # issue #12284:
        @test stringmime("text/plain", Base.Docs.doc(Foo.foo)) == "foo function\n"
        @test stringmime("text/plain", Base.Docs.doc(Foo.Bar.bar)) == "bar function\n"

        modules, deps = Base.parse_cache_header(cachefile)
        @test modules == Dict(Foo_module => Base.module_uuid(Foo))
        @test map(x -> x[1],  sort(deps)) == [Foo_file, joinpath(dir, "bar.jl"), joinpath(dir, "foo.jl")]

        modules, deps1 = Base.cache_dependencies(cachefile)
        @test sort(modules) == map(s -> (s, Base.module_uuid(eval(s))),
                                   [:Base, :Core, :Main])
        @test deps == deps1

        @test current_task()(0x01, 0x4000, 0x30031234) == 2
        @test nothing(0x01, 0x4000, 0x30031234) == 52
        @test nothing(0x01, 0x4000, 0x30031234; x = 9142) == 9142
        @test Foo.nothingkw === Core.kwfunc(Base.nothing)

        @test Foo.NominalValue() == 1
        @test Foo.OrdinalValue() == 1
        @test Foo.NominalValue{Int}() == 2
        @test Foo.OrdinalValue{Int}() == 2
        let T = Vector{Foo.NominalValue{Int}}
            @test isa(T(), T)
        end
        @test Vector{Foo.NominalValue{Int32, Int64}}() == 3
        @test Vector{Foo.NominalValue{UInt, UInt}}() == 4
        @test Vector{Foo.NominalValue{Int, Int}}() == 5
        @test all(i -> Foo.t17809s[i + 1] ===
            Tuple{
                Type{Ptr{Foo.MyType{i}}},
                Array{Ptr{Foo.MyType{Foo.MyType{:sym}()}}(0), 0},
                Val{Complex{Int}(1, 2)},
                Val{3},
                Val{nothing}},
            0:25)
    end

    Baz_file = joinpath(dir, "Baz.jl")
    write(Baz_file,
          """
          __precompile__(false)
          module Baz
          end
          """)

    t = redirected_stderr("ERROR: LoadError: __precompile__(false) is not allowed in files that are being precompiled\n in __precompile__")
    try
        Base.compilecache("Baz") # from __precompile__(false)
        error("__precompile__ disabled test failed")
    catch exc
        close(STDERR)
        redirect_stderr(olderr)
        isa(exc, ErrorException) || rethrow(exc)
        !isempty(search(exc.msg, "__precompile__(false)")) && rethrow(exc)
    end
    wait(t)

    # Issue #12720
    FooBar1_file = joinpath(dir, "FooBar1.jl")
    write(FooBar1_file,
          """
          __precompile__(true)
          module FooBar1
              using FooBar
          end
          """)
    sleep(2) # give FooBar and FooBar1 different timestamps, in reverse order too
    FooBar_file = joinpath(dir, "FooBar.jl")
    write(FooBar_file,
          """
          __precompile__(true)
          module FooBar
          end
          """)

    Base.compilecache("FooBar")
    @test isfile(joinpath(dir, "FooBar.ji"))
    @test !Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji"))
    @test !isdefined(Main, :FooBar)
    @test !isdefined(Main, :FooBar1)

    @eval using FooBar
    fb_uuid = Base.module_uuid(Main.FooBar)
    sleep(2); touch(FooBar_file)
    insert!(Base.LOAD_CACHE_PATH, 1, dir2)
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji"))
    @eval using FooBar1
    @test !isfile(joinpath(dir2, "FooBar.ji"))
    @test !isfile(joinpath(dir, "FooBar1.ji"))
    @test isfile(joinpath(dir2, "FooBar1.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji"))
    @test !Base.stale_cachefile(FooBar1_file, joinpath(dir2, "FooBar1.ji"))
    @test fb_uuid == Base.module_uuid(Main.FooBar)
    fb_uuid1 = Base.module_uuid(Main.FooBar1)
    @test fb_uuid != fb_uuid1

    t = redirected_stderr("WARNING: replacing module FooBar.")
    try
        reload("FooBar")
    finally
        close(STDERR)
        redirect_stderr(olderr)
    end
    wait(t)
    @test fb_uuid != Base.module_uuid(Main.FooBar)
    @test fb_uuid1 == Base.module_uuid(Main.FooBar1)
    fb_uuid = Base.module_uuid(Main.FooBar)
    @test isfile(joinpath(dir2, "FooBar.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji"))
    @test !Base.stale_cachefile(FooBar1_file, joinpath(dir2, "FooBar1.ji"))
    @test !Base.stale_cachefile(FooBar_file, joinpath(dir2, "FooBar.ji"))

    t = redirected_stderr("""
                          WARNING: Deserialization checks failed while attempting to load cache from $(joinpath(dir2, "FooBar1.ji")).
                          WARNING: Module FooBar uuid did not match cache file.
                          WARNING: replacing module FooBar1.
                          """)
    try
        reload("FooBar1")
    finally
        close(STDERR)
        redirect_stderr(olderr)
    end
    wait(t)
    @test fb_uuid == Base.module_uuid(Main.FooBar)
    @test fb_uuid1 != Base.module_uuid(Main.FooBar1)

    @test isfile(joinpath(dir2, "FooBar.ji"))
    @test isfile(joinpath(dir2, "FooBar1.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(dir, "FooBar.ji"))
    @test !Base.stale_cachefile(FooBar_file, joinpath(dir2, "FooBar.ji"))
    @test !Base.stale_cachefile(FooBar1_file, joinpath(dir2, "FooBar1.ji"))

    # test behavior of precompile modules that throw errors
    write(FooBar_file,
          """
          __precompile__(true)
          module FooBar
          error("break me")
          end
          """)
    t = redirected_stderr("ERROR: LoadError: break me\n in error")
    try
        Base.require(:FooBar)
        error("\"LoadError: break me\" test failed")
    catch exc
        close(STDERR)
        redirect_stderr(olderr)
        isa(exc, ErrorException) || rethrow(exc)
        !isempty(search(exc.msg, "ERROR: LoadError: break me")) && rethrow(exc)
    end
    wait(t)
finally
    if STDERR != olderr
        close(STDERR)
        redirect_stderr(olderr)
    end
    splice!(Base.LOAD_CACHE_PATH, 1:2)
    splice!(LOAD_PATH, 1)
    rm(dir, recursive=true)
    rm(dir2, recursive=true)
end

# test --compilecache=no command line option
let dir = mktempdir(),
    Time_module = :Time4b3a94a1a081a8cb

    try
        write(joinpath(dir, "$Time_module.jl"),
              """
              module $Time_module
                  __precompile__(true)
                  time = Base.time()
              end
              """)

        eval(quote
            insert!(LOAD_PATH, 1, $(dir))
            insert!(Base.LOAD_CACHE_PATH, 1, $(dir))
            Base.compilecache(:Time4b3a94a1a081a8cb)
        end)

        exename = `$(Base.julia_cmd()) --precompiled=yes --startup-file=no`

        testcode = """
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(Base.LOAD_CACHE_PATH, 1, $(repr(dir)))
            using $Time_module
            getfield($Time_module, :time)
        """

        t1_yes = readchomp(`$exename --compilecache=yes -E $(testcode)`)
        t2_yes = readchomp(`$exename --compilecache=yes -E $(testcode)`)
        @test t1_yes == t2_yes

        t1_no = readchomp(`$exename --compilecache=no -E $(testcode)`)
        t2_no = readchomp(`$exename --compilecache=no -E $(testcode)`)
        @test t1_no != t2_no
        @test parse(Float64, t1_no) < parse(Float64, t2_no)

    finally
        splice!(Base.LOAD_CACHE_PATH, 1)
        splice!(LOAD_PATH, 1)
        rm(dir, recursive=true)
    end
end

let module_name = string("a",randstring())
    insert!(LOAD_PATH, 1, pwd())
    file_name = string(module_name, ".jl")
    sleep(2); touch(file_name)
    code = """module $(module_name)\nend\n"""
    write(file_name, code)
    reload(module_name)
    @test typeof(eval(Symbol(module_name))) == Module
    deleteat!(LOAD_PATH,1)
    rm(file_name)
end
