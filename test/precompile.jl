# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Distributed, Random

Foo_module = :Foo4b3a94a1a081a8cb
Foo2_module = :F2oo4b3a94a1a081a8cb
FooBase_module = :FooBase4b3a94a1a081a8cb
@eval module ConflictingBindings
    export $Foo_module, $FooBase_module
    $Foo_module = 232
    $FooBase_module = 9134
end
using .ConflictingBindings

(f -> f())() do # wrap in function scope, so we can test world errors
dir = mktempdir()
dir2 = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(DEPOT_PATH, 1, dir)
try
    Foo_file = joinpath(dir, "$Foo_module.jl")
    Foo2_file = joinpath(dir, "$Foo2_module.jl")
    FooBase_file = joinpath(dir, "$FooBase_module.jl")

    write(FooBase_file,
          """
          false && __precompile__(false)
          module $FooBase_module
              import Base: hash, >
              struct fmpz end
              struct typeA end
              >(x::fmpz, y::Int) = Base.cmp(x, y) > 0
              function hash(a::typeA, h::UInt)
                  d = den(a)
                  return h
              end
          end
          """)
    write(Foo2_file,
          """
          module $Foo2_module
              export override
              override(x::Integer) = 2
              override(x::AbstractFloat) = Float64(override(1))
          end
          """)
    write(Foo_file,
          """
          module $Foo_module
              import $FooBase_module, $FooBase_module.typeA
              import $Foo2_module: $Foo2_module, override
              import $FooBase_module.hash
              import Test
              module Inner
                  import $FooBase_module.hash
                  using ..$Foo_module
                  import ..$Foo2_module
              end

              struct typeB
                  y::typeA
              end
              hash(x::typeB) = hash(x.y)

              # test that docs get reconnected
              @doc "foo function" foo(x) = x + 1
              include_dependency("foo.jl")
              include_dependency("foo.jl")
              module Bar
                  @doc "bar function" bar(x) = x + 2
                  include_dependency("bar.jl")
              end

              # test for creation of some reasonably complicated type
              struct MyType{T} end
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
              Test.@test !isdefined(typeof(sin).name.mt, :kwsorter)
              Base.sin(::UInt8, ::UInt16, ::UInt32; x = 52) = x
              const sinkw = Core.kwfunc(Base.sin)

              # issue 16908 (some complicated types and external method definitions)
              abstract type CategoricalPool{T, R <: Integer, V} end
              abstract type CategoricalValue{T, R <: Integer} end
              struct NominalPool{T, R <: Integer, V} <: CategoricalPool{T, R, V}
                  index::Vector{T}
                  invindex::Dict{T, R}
                  order::Vector{R}
                  ordered::Vector{T}
                  valindex::Vector{V}
              end
              struct NominalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              struct OrdinalValue{T, R <: Integer} <: CategoricalValue{T, R}
                  level::R
                  pool::NominalPool{T, R, NominalValue{T, R}}
              end
              (::Union{Type{NominalValue}, Type{OrdinalValue}})() = 1
              (::Union{Type{NominalValue{T}}, Type{OrdinalValue{T}}})() where {T} = 2
              (::Type{Vector{NominalValue{T, R}}})() where {T, R} = 3
              (::Type{Vector{NominalValue{T, T}}})() where {T} = 4
              (::Type{Vector{NominalValue{Int, Int}}})() = 5

              # more tests for method signature involving a complicated type
              # issue 18343
              struct Pool18343{R, V}
                  valindex::Vector{V}
              end
              struct Value18343{T, R}
                  pool::Pool18343{R, Value18343{T, R}}
              end
              Base.convert(::Type{Some{S}}, ::Value18343{Some}) where {S} = 2
              Base.convert(::Type{Some{Value18343}}, ::Value18343{Some}) = 2
              Base.convert(::Type{Ref}, ::Value18343{T}) where {T} = 3


              # issue #28297
              mutable struct Result
                  result::Union{Int,Missing}
              end

              const x28297 = Result(missing)

              const d29936a = UnionAll(Dict.var, UnionAll(Dict.body.var, Dict.body.body))
              const d29936b = UnionAll(Dict.body.var, UnionAll(Dict.var, Dict.body.body))

              # issue #28998
              const x28998 = [missing, 2, missing, 6, missing,
                              missing, missing, missing,
                              missing, missing, missing,
                              missing, missing, 6]

              let some_method = which(Base.include, (Module, String,))
                    # global const some_method // FIXME: support for serializing a direct reference to an external Method not implemented
                  global const some_linfo =
                      ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt),
                          some_method, Tuple{typeof(Base.include), Module, String}, Core.svec(), typemax(UInt))
              end

              g() = override(1.0)
              Test.@test g() === 2.0 # compile this

              const abigfloat_f() = big"12.34"
              const abigfloat_x = big"43.21"
              const abigint_f() = big"123"
              const abigint_x = big"124"

              # issue #31488
              _v31488 = Base.StringVector(2)
              resize!(_v31488, 0)
              const a31488 = fill(String(_v31488), 100)

              const ptr1 = Ptr{UInt8}(1)
              ptr2 = Ptr{UInt8}(1)
              const ptr3 = Ptr{UInt8}(-1)
              const layout1 = Ptr{Int8}[Ptr{Int8}(0), Ptr{Int8}(1), Ptr{Int8}(-1)]
              const layout2 = Any[Ptr{Int8}(0), Ptr{Int16}(1), Ptr{Int32}(-1)]
              const layout3 = collect(x.match for x in eachmatch(r"..", "abcdefghijk"))::Vector{SubString{String}}
          end
          """)
    # make sure `sin` didn't have a kwfunc (which would invalidate the attempted test)
    @test !isdefined(typeof(sin).name.mt, :kwsorter)

    # Issue #12623
    @test __precompile__(false) === nothing

    # Issue #21307
    Foo2 = Base.require(Main, Foo2_module)
    @eval $Foo2.override(::Int) = 'a'
    @eval $Foo2.override(::Float32) = 'b'

    Foo = Base.require(Main, Foo_module)
    Base.invokelatest() do # use invokelatest to see the results of loading the compile
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # Issue #21307
        @test Foo.g() === 97.0
        @test Foo.override(1.0e0) == Float64('a')
        @test Foo.override(1.0f0) == 'b'
        @test Foo.override(UInt(1)) == 2

        # Issue #15722
        @test Foo.abigfloat_f()::BigFloat == big"12.34"
        @test (Foo.abigfloat_x::BigFloat + 21) == big"64.21"
        @test Foo.abigint_f()::BigInt == big"123"
        @test Foo.abigint_x::BigInt + 1 == big"125"

        @test Foo.x28297.result === missing

        @test Foo.d29936a === Dict
        @test Foo.d29936b === Dict{K,V} where {V,K}

        @test Foo.x28998[end] == 6

        @test Foo.a31488 == fill("", 100)

        @test Foo.ptr1 === Ptr{UInt8}(1)
        @test Foo.ptr2 === Ptr{UInt8}(0)
        @test Foo.ptr3 === Ptr{UInt8}(-1)
        @test Foo.layout1::Vector{Ptr{Int8}} == Ptr{Int8}[Ptr{Int8}(0), Ptr{Int8}(0), Ptr{Int8}(-1)]
        @test Foo.layout2 == Any[Ptr{Int8}(0), Ptr{Int16}(0), Ptr{Int32}(-1)]
        @test typeof.(Foo.layout2) == [Ptr{Int8}, Ptr{Int16}, Ptr{Int32}]
        @test Foo.layout3 == ["ab", "cd", "ef", "gh", "ij"]
    end

    cachedir = joinpath(dir, "compiled", "v$(VERSION.major).$(VERSION.minor)")
    cachedir2 = joinpath(dir2, "compiled", "v$(VERSION.major).$(VERSION.minor)")
    cachefile = joinpath(cachedir, "$Foo_module.ji")
    # use _require_from_serialized to ensure that the test fails if
    # the module doesn't reload from the image:
    @test_logs (:warn, "Replacing module `$Foo_module`") begin
        ms = Base._require_from_serialized(cachefile)
        @test isa(ms, Array{Any,1})
    end

    @test_throws MethodError Foo.foo(17) # world shouldn't be visible yet
    Base.invokelatest() do # use invokelatest to see the results of loading the compile
        @test Foo.foo(17) == 18
        @test Foo.Bar.bar(17) == 19

        # Issue #21307
        @test Foo.g() === 97.0
        @test Foo.override(1.0e0) == Float64('a')
        @test Foo.override(1.0f0) == 'b'
        @test Foo.override(UInt(1)) == 2

        # issue #12284:
        @test string(Base.Docs.doc(Foo.foo)) == "foo function\n"
        @test string(Base.Docs.doc(Foo.Bar.bar)) == "bar function\n"

        modules, (deps, requires), required_modules = Base.parse_cache_header(cachefile)
        discard_module = mod_fl_mt -> (mod_fl_mt[2], mod_fl_mt[3])
        @test modules == [ Base.PkgId(Foo) => Base.module_build_id(Foo) ]
        @test map(x -> x[2], deps) == [ Foo_file, joinpath(dir, "foo.jl"), joinpath(dir, "bar.jl") ]
        @test requires == [ Base.PkgId(Foo) => Base.PkgId(string(FooBase_module)),
                            Base.PkgId(Foo) => Base.PkgId(Foo2),
                            Base.PkgId(Foo) => Base.PkgId(Test),
                            Base.PkgId(Foo) => Base.PkgId(string(FooBase_module)) ]
        srctxt = Base.read_dependency_src(cachefile, Foo_file)
        @test !isempty(srctxt) && srctxt == read(Foo_file, String)
        @test_throws ErrorException Base.read_dependency_src(cachefile, "/tmp/nonexistent.txt")
        # dependencies declared with `include_dependency` should not be stored
        @test_throws ErrorException Base.read_dependency_src(cachefile, joinpath(dir, "foo.jl"))

        modules, deps1 = Base.cache_dependencies(cachefile)
        stdlib_modules = filter(readdir(Sys.STDLIB)) do s
            pkg = Base.identify_package(s)
            return pkg !== nothing &&
                   haskey(Base.loaded_modules, pkg)
        end
        @test Dict(modules) == merge(
            Dict(let m = Base.PkgId(s)
                    m => Base.module_build_id(Base.root_module(m))
                 end for s in
                 [ "Base", "Core", "Main",
                   string(Foo2_module), string(FooBase_module) ]),
            # plus modules included in the system image
            Dict(let m = Base.root_module(Base, s)
                     Base.PkgId(m) => Base.module_build_id(m)
                 end for s in Symbol.(stdlib_modules)),
                # Plus precompilation module generated at build time
                let id = Base.PkgId("__PackagePrecompilationStatementModule")
                    Dict(id => Base.module_build_id(Base.root_module(id)))
                end
           )
        @test discard_module.(deps) == deps1

        @test current_task()(0x01, 0x4000, 0x30031234) == 2
        @test sin(0x01, 0x4000, 0x30031234) == 52
        @test sin(0x01, 0x4000, 0x30031234; x = 9142) == 9142
        @test Foo.sinkw === Core.kwfunc(Base.sin)

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
        some_method = which(Base.include, (Module, String,))
        some_linfo =
                ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt),
                    some_method, Tuple{typeof(Base.include), Module, String}, Core.svec(), typemax(UInt))
        @test Foo.some_linfo::Core.MethodInstance === some_linfo

        ft = Base.datatype_fieldtypes
        PV = ft(Foo.Value18343{Some}.body)[1]
        VR = ft(PV)[1].parameters[1]
        @test ft(PV)[1] === Array{VR,1}
        @test pointer_from_objref(ft(PV)[1]) ===
              pointer_from_objref(ft(ft(ft(PV)[1].parameters[1])[1])[1])
        @test PV === ft(ft(PV)[1].parameters[1])[1]
        @test pointer_from_objref(PV) === pointer_from_objref(ft(ft(PV)[1].parameters[1])[1])
    end

    Baz_file = joinpath(dir, "Baz.jl")
    write(Baz_file,
          """
          true && __precompile__(false)
          module Baz
          baz() = 1
          end
          """)

    @test Base.compilecache(Base.PkgId("Baz")) == Base.PrecompilableError() # due to __precompile__(false)
    @eval using Baz
    @test Base.invokelatest(Baz.baz) == 1

    # Issue #12720
    FooBar1_file = joinpath(dir, "FooBar1.jl")
    write(FooBar1_file,
          """
          module FooBar1
              using FooBar
          end
          """)
    sleep(2) # give FooBar and FooBar1 different timestamps, in reverse order too
    FooBar_file = joinpath(dir, "FooBar.jl")
    write(FooBar_file,
          """
          module FooBar
          end
          """)

    cachefile = Base.compilecache(Base.PkgId("FooBar"))
    @test cachefile == Base.compilecache_path(Base.PkgId("FooBar"))
    @test isfile(joinpath(cachedir, "FooBar.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) isa Vector
    @test !isdefined(Main, :FooBar)
    @test !isdefined(Main, :FooBar1)

    relFooBar_file = joinpath(dir, "subfolder", "..", "FooBar.jl")
    @test Base.stale_cachefile(relFooBar_file, joinpath(cachedir, "FooBar.ji")) isa (Sys.iswindows() ? Vector : Bool) # `..` is not a symlink on Windows
    mkdir(joinpath(dir, "subfolder"))
    @test Base.stale_cachefile(relFooBar_file, joinpath(cachedir, "FooBar.ji")) isa Vector

    @eval using FooBar
    fb_uuid = Base.module_build_id(FooBar)
    sleep(2); touch(FooBar_file)
    insert!(DEPOT_PATH, 1, dir2)
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) === true
    @eval using FooBar1
    @test !isfile(joinpath(cachedir2, "FooBar.ji"))
    @test !isfile(joinpath(cachedir, "FooBar1.ji"))
    @test isfile(joinpath(cachedir2, "FooBar1.ji"))
    @test Base.stale_cachefile(FooBar_file, joinpath(cachedir, "FooBar.ji")) === true
    @test Base.stale_cachefile(FooBar1_file, joinpath(cachedir2, "FooBar1.ji")) isa Vector
    @test fb_uuid == Base.module_build_id(FooBar)
    fb_uuid1 = Base.module_build_id(FooBar1)
    @test fb_uuid != fb_uuid1

    # test checksum
    open(joinpath(cachedir2, "FooBar1.ji"), "a") do f
        write(f, 0x076cac96) # append 4 random bytes
    end
    @test Base.stale_cachefile(FooBar1_file, joinpath(cachedir2, "FooBar1.ji")) === true

    # test behavior of precompile modules that throw errors
    FooBar2_file = joinpath(dir, "FooBar2.jl")
    write(FooBar2_file,
          """
          module FooBar2
          error("break me")
          end
          """)
    @test_warn "ERROR: LoadError: break me\nStacktrace:\n [1] error" try
            Base.require(Main, :FooBar2)
            error("the \"break me\" test failed")
        catch exc
            isa(exc, ErrorException) || rethrow()
            occursin("ERROR: LoadError: break me", exc.msg) && rethrow()
        end

    # Test that trying to eval into closed modules during precompilation is an error
    FooBar3_file = joinpath(dir, "FooBar3.jl")
    FooBar3_inc = joinpath(dir, "FooBar3_inc.jl")
    write(FooBar3_inc, "x=1\n")
    for code in ["Core.eval(Base, :(x=1))", "Base.include(Base, \"FooBar3_inc.jl\")"]
        write(FooBar3_file, code)
        @test_warn "Evaluation into the closed module `Base` breaks incremental compilation" try
                Base.require(Main, :FooBar3)
            catch exc
                isa(exc, ErrorException) || rethrow()
            end
    end

    # Test transitive dependency for #21266
    FooBarT_file = joinpath(dir, "FooBarT.jl")
    write(FooBarT_file,
          """
          module FooBarT
          end
          """)
    FooBarT1_file = joinpath(dir, "FooBarT1.jl")
    write(FooBarT1_file,
          """
          module FooBarT1
              using FooBarT
          end
          """)
    FooBarT2_file = joinpath(dir, "FooBarT2.jl")
    write(FooBarT2_file,
          """
          module FooBarT2
              using FooBarT1
          end
          """)
    Base.compilecache(Base.PkgId("FooBarT2"))
    write(FooBarT1_file,
          """
          module FooBarT1
          end
          """)
    rm(FooBarT_file)
    @test Base.stale_cachefile(FooBarT2_file, joinpath(cachedir2, "FooBarT2.ji")) === true
    @test Base.require(Main, :FooBarT2) isa Module
finally
    splice!(DEPOT_PATH, 1:2)
    splice!(LOAD_PATH, 1)
    rm(dir, recursive=true)
    rm(dir2, recursive=true)
end
end

# test --compiled-modules=no command line option
let dir = mktempdir(),
    Time_module = :Time4b3a94a1a081a8cb

    try
        write(joinpath(dir, "$Time_module.jl"),
              """
              module $Time_module
                  time = Base.time()
              end
              """)

        eval(quote
            insert!(LOAD_PATH, 1, $(dir))
            insert!(DEPOT_PATH, 1, $(dir))
            Base.compilecache(Base.PkgId("Time4b3a94a1a081a8cb"))
        end)

        exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no`

        testcode = """
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(DEPOT_PATH, 1, $(repr(dir)))
            using $Time_module
            getfield($Time_module, :time)
        """

        t1_yes = readchomp(`$exename --compiled-modules=yes -E $(testcode)`)
        t2_yes = readchomp(`$exename --compiled-modules=yes -E $(testcode)`)
        @test t1_yes == t2_yes

        t1_no = readchomp(`$exename --compiled-modules=no -E $(testcode)`)
        t2_no = readchomp(`$exename --compiled-modules=no -E $(testcode)`)
        @test t1_no != t2_no
        @test parse(Float64, t1_no) < parse(Float64, t2_no)

    finally
        splice!(DEPOT_PATH, 1)
        splice!(LOAD_PATH, 1)
        rm(dir, recursive=true)
    end
end

# test loading a package with conflicting namespace
let dir = mktempdir()
    Test_module = :Test6c92f26
    try
        write(joinpath(dir, "Iterators.jl"),
              """
              module Iterators
              end
              """)

        write(joinpath(dir, "$Test_module.jl"),
              """
              module $Test_module
                   import Iterators # FIXME: use `using`
              end
              """)

        testcode = """
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(DEPOT_PATH, 1, $(repr(dir)))
            using $Test_module
            println(stderr, $Test_module.Iterators)
        """

        exename = `$(Base.julia_cmd()) --startup-file=no`
        let fname = tempname()
            try
                for i = 1:2
                    @test readchomp(pipeline(`$exename -E $(testcode)`, stderr=fname)) == "nothing"
                    @test read(fname, String) == "Iterators\n"
                end
            finally
                rm(fname, force=true)
            end
        end
    finally
        rm(dir, recursive=true)
    end
end

let dir = mktempdir()
    try
        insert!(LOAD_PATH, 1, dir)
        insert!(DEPOT_PATH, 1, dir)

        loaded_modules = Channel{Symbol}(32)
        callback = (mod::Base.PkgId) -> put!(loaded_modules, Symbol(mod.name))
        push!(Base.package_callbacks, callback)

        Test1_module = :Teste4095a81
        Test2_module = :Teste4095a82
        Test3_module = :Teste4095a83

        write(joinpath(dir, "$(Test1_module).jl"),
              """
              module $(Test1_module)
              end
              """)

        Base.compilecache(Base.PkgId("$(Test1_module)"))
        write(joinpath(dir, "$(Test2_module).jl"),
              """
              module $(Test2_module)
                  using $(Test1_module)
              end
              """)
        Base.compilecache(Base.PkgId("$(Test2_module)"))
        @test !Base.isbindingresolved(Main, Test2_module)
        Base.require(Main, Test2_module)
        @test take!(loaded_modules) == Test1_module
        @test take!(loaded_modules) == Test2_module
        write(joinpath(dir, "$(Test3_module).jl"),
              """
              module $(Test3_module)
                  using $(Test3_module)
              end
              """)
        Base.require(Main, Test3_module)
        @test take!(loaded_modules) == Test3_module
    finally
        pop!(Base.package_callbacks)
        splice!(DEPOT_PATH, 1)
        splice!(LOAD_PATH, 1)
        rm(dir, recursive=true)
    end
end

# Issue #19960
(f -> f())() do # wrap in function scope, so we can test world errors
    test_workers = addprocs(1)
    push!(test_workers, myid())
    save_cwd = pwd()
    temp_path = mktempdir()
    try
        cd(temp_path)
        load_path = mktempdir(temp_path)
        load_cache_path = mktempdir(temp_path)

        ModuleA = :Issue19960A
        ModuleB = :Issue19960B

        write(joinpath(load_path, "$ModuleA.jl"),
            """
            module $ModuleA
                import Distributed: myid
                export f
                f() = myid()
            end
            """)

        write(joinpath(load_path, "$ModuleB.jl"),
            """
            module $ModuleB
                using $ModuleA
                export g
                g() = f()
            end
            """)

        @everywhere test_workers begin
            pushfirst!(LOAD_PATH, $load_path)
            pushfirst!(DEPOT_PATH, $load_cache_path)
        end
        try
            @eval using $ModuleB
            uuid = Base.module_build_id(Base.root_module(Main, ModuleB))
            for wid in test_workers
                @test Distributed.remotecall_eval(Main, wid, quote
                        Base.module_build_id(Base.root_module(Main, $(QuoteNode(ModuleB))))
                    end) == uuid
                if wid != myid() # avoid world-age errors on the local proc
                    @test remotecall_fetch(g, wid) == wid
                end
            end
        finally
            @everywhere test_workers begin
                popfirst!(LOAD_PATH)
                popfirst!(DEPOT_PATH)
            end
        end
    finally
        cd(save_cwd)
        rm(temp_path, recursive=true)
        pop!(test_workers) # remove myid
        rmprocs(test_workers)
    end
end

# Ensure that module-loading plays nicely with Base.delete_method
(f -> f())() do # wrap in function scope, so we can test world errors
dir = mktempdir()
insert!(LOAD_PATH, 1, dir)
insert!(DEPOT_PATH, 1, dir)
try
    A_module = :Aedb164bd3a126418
    B_module = :Bedb164bd3a126418
    A_file = joinpath(dir, "$A_module.jl")
    B_file = joinpath(dir, "$B_module.jl")

    write(A_file,
          """
          module $A_module

          export apc, anopc

          apc(::Int, ::Int) = 1
          apc(::Any, ::Any) = 2

          anopc(::Int, ::Int) = 1
          anopc(::Any, ::Any) = 2

          end
          """)
    write(B_file,
          """
          module $B_module

          using $A_module

          bpc(x) = apc(x, x)
          bnopc(x) = anopc(x, x)

          precompile(bpc, (Int,))
          precompile(bpc, (Float64,))

          end
          """)
    A = Base.require(Main, A_module)
    for mths in (collect(methods(A.apc)), collect(methods(A.anopc)))
        Base.delete_method(mths[1])
    end
    B = Base.require(Main, B_module)
    @test Base.invokelatest(B.bpc, 1) == Base.invokelatest(B.bpc, 1.0) == 2
    @test Base.invokelatest(B.bnopc, 1) == Base.invokelatest(B.bnopc, 1.0) == 2
finally
    popfirst!(LOAD_PATH)
    popfirst!(DEPOT_PATH)
    rm(dir, recursive=true)
end

# issue #19030 and #25279
let
    load_path = mktempdir()
    load_cache_path = mktempdir()
    try
        ModuleA = :Issue19030

        write(joinpath(load_path, "$ModuleA.jl"),
            """
            module $ModuleA
                __init__() = push!(Base.package_callbacks, sym->nothing)
            end
            """)

        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)

        l0 = length(Base.package_callbacks)
        @eval using $ModuleA
        @test length(Base.package_callbacks) == l0 + 1
    finally
        rm(load_path, recursive=true)
        rm(load_cache_path, recursive=true)
    end
end

let
    load_path = mktempdir()
    load_cache_path = mktempdir()
    try
        write(joinpath(load_path, "A25604.jl"),
            """
            module A25604
            using B25604
            using C25604
            end
            """)
        write(joinpath(load_path, "B25604.jl"),
            """
            module B25604
            end
            """)
        write(joinpath(load_path, "C25604.jl"),
            """
            module C25604
            using B25604
            end
            """)

        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)

        Base.compilecache(Base.PkgId("A25604"))
        @test_nowarn @eval using A25604
    finally
        rm(load_path, recursive=true)
        rm(load_cache_path, recursive=true)
    end
end

let
    load_path = mktempdir()
    load_cache_path = mktempdir()
    try
        write(joinpath(load_path, "Foo26028.jl"),
            """
            module Foo26028

            module Bar26028
                x = 0
            end

            function __init__()
                include(joinpath(@__DIR__, "Baz26028.jl"))
            end

            end
            """)
        write(joinpath(load_path, "Baz26028.jl"),
            """
            module Baz26028
            import Foo26028.Bar26028.x
            end
            """)

        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)

        Base.compilecache(Base.PkgId("Foo26028"))
        @test_nowarn @eval using Foo26028
    finally
        rm(load_path, recursive=true)
        rm(load_cache_path, recursive=true)
    end
end

# issue #29936
let
    load_path = mktempdir()
    load_cache_path = mktempdir()
    try
        write(joinpath(load_path, "Foo29936.jl"),
              """
              module Foo29936
              const global m = Val{nothing}()
              const global h = Val{:hey}()
              wab = [("a", m), ("b", h),]
              end
              """)
        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)
        @eval using Foo29936
        @test [("Plan", Foo29936.m), ("Plan", Foo29936.h),] isa Vector{Tuple{String,Val}}
    finally
        rm(load_path, recursive=true)
        rm(load_cache_path, recursive=true)
    end
end

# Issue #25971
let
    load_path = mktempdir()
    load_cache_path = mktempdir()
    try
        pushfirst!(LOAD_PATH, load_path)
        pushfirst!(DEPOT_PATH, load_cache_path)
        sourcefile = joinpath(load_path, "Foo25971.jl")
        write(sourcefile, "module Foo25971 end")
        chmod(sourcefile, 0o666)
        cachefile = Base.compilecache(Base.PkgId("Foo25971"))
        @test filemode(sourcefile) == filemode(cachefile)
        chmod(sourcefile, 0o600)
        cachefile = Base.compilecache(Base.PkgId("Foo25971"))
        @test filemode(sourcefile) == filemode(cachefile)
    finally
        rm(load_path, recursive=true)
        rm(load_cache_path, recursive=true)
        filter!((≠)(load_path), LOAD_PATH)
        filter!((≠)(load_cache_path), DEPOT_PATH)
    end
end

end # !withenv
