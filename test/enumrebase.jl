# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for rebasing of auto-assigned enum member values at package image load
# (src/enums.c + src/staticdata.c). Every scenario runs in subprocesses so that
# each gets a fresh session with a controlled package load order.

using Test

const PATHSEP = Sys.iswindows() ? ';' : ':'

function write_pkgs(load_path)
    write(joinpath(load_path, "EnumDef.jl"), """
        module EnumDef
        const Foo = Core._enumtype(@__MODULE__, :Foo, Any, Int16, true)
        Core._enum_add_member(Foo, @__MODULE__, :A, nothing)     # auto: 0
        Core._enum_add_member(Foo, @__MODULE__, :B, Int16(100))  # explicit
        end
        """)
    # ExtA and ExtB are unrelated packages; each auto-assigns one member, so
    # both serialize the same value (1) for their member in their own images.
    for X in ("A", "B")
        write(joinpath(load_path, "Ext$X.jl"), """
            module Ext$X
            using EnumDef
            Core._enum_extend(EnumDef.Foo, @__MODULE__, Int16)
            Core._enum_add_member(EnumDef.Foo, @__MODULE__, :M$X, nothing)
            end
            """)
    end
    write(joinpath(load_path, "EnumApp.jl"), """
        module EnumApp
        using EnumDef, ExtA, ExtB
        struct Holder
            x::EnumDef.Foo
            y::Int
        end
        struct UHolder
            u::Union{EnumDef.Foo, Int32}
        end
        const CV = ExtB.MB
        const VEC = EnumDef.Foo[ExtA.MA, ExtB.MB, EnumDef.A]
        const H = Holder(ExtA.MA, 42)
        const U1 = UHolder(ExtB.MB)
        const U2 = UHolder(Int32(7))
        getmb() = CV
        precompile(getmb, ())
        end
        """)
    # ExtC and ExtD pin the same explicit value: loading both must error
    for X in ("C", "D")
        write(joinpath(load_path, "Ext$X.jl"), """
            module Ext$X
            using EnumDef
            Core._enum_extend(EnumDef.Foo, @__MODULE__, Int16)
            Core._enum_add_member(EnumDef.Foo, @__MODULE__, :M$X, Int16(42))
            end
            """)
    end
end

function run_sub(load_path, depot, code; flags = String[])
    cmd = `$(Base.julia_cmd()) --startup-file=no $(flags) -e $code`
    cmd = addenv(cmd,
        "JULIA_LOAD_PATH" => string(load_path, PATHSEP),
        "JULIA_DEPOT_PATH" => string(depot, PATHSEP))
    out = IOBuffer()
    err = IOBuffer()
    p = run(pipeline(cmd; stdout = out, stderr = err); wait = false)
    wait(p)
    return success(p), String(take!(out)), String(take!(err))
end

const ASSERTION_CODE = """
    # adversarial load order: opposite of the (dependency-driven) compile order
    using EnumDef
    using ExtB
    using ExtA
    using EnumApp
    vA  = reinterpret(Int16, EnumDef.A)
    vB  = reinterpret(Int16, EnumDef.B)
    vMA = reinterpret(Int16, ExtA.MA)
    vMB = reinterpret(Int16, ExtB.MB)
    @assert vA == 0
    @assert vB == 100
    @assert allunique([vA, vB, vMA, vMB])
    # module-level consts, array data, struct fields, isbits-union fields and
    # compiled code must all observe the rebased values
    @assert EnumApp.CV === ExtB.MB
    @assert EnumApp.VEC[1] === ExtA.MA
    @assert EnumApp.VEC[2] === ExtB.MB
    @assert EnumApp.VEC[3] === EnumDef.A
    @assert EnumApp.H.x === ExtA.MA
    @assert EnumApp.H.y == 42
    @assert EnumApp.U1.u === ExtB.MB
    @assert EnumApp.U2.u === Int32(7)
    @assert EnumApp.getmb() === ExtB.MB
    # the member registry agrees with the bindings
    tab = Core._enum_members(EnumDef.Foo)
    insts = [tab[3 + 4*(i-1) + 3] for i in 1:(length(tab) - 3) ÷ 4]
    @assert ExtA.MA in insts && ExtB.MB in insts
    println("REBASE_OK")
    """

mktempdir() do dir
    load_path = joinpath(dir, "pkgs")
    depot = joinpath(dir, "depot")
    mkpath(load_path)
    mkpath(depot)
    write_pkgs(load_path)

    @testset "precompile packages" begin
        ok, out, err = run_sub(load_path, depot, "using EnumApp; println(\"COMPILED\")")
        if !ok
            @info "compile failed" out err
        end
        @test ok
        @test occursin("COMPILED", out)
    end

    @testset "rebase on adversarial load order" begin
        ok, out, err = run_sub(load_path, depot, ASSERTION_CODE)
        if !ok || !occursin("REBASE_OK", out)
            @info "rebase run failed" out err
        end
        @test ok
        @test occursin("REBASE_OK", out)
    end

    @testset "rebase without pkgimages" begin
        ok, out, err = run_sub(load_path, depot, ASSERTION_CODE; flags = ["--pkgimages=no"])
        if !ok || !occursin("REBASE_OK", out)
            @info "no-pkgimages run failed" out err
        end
        @test ok
        @test occursin("REBASE_OK", out)
    end

    @testset "explicit value conflict errors catchably" begin
        code = """
            using ExtC
            try
                Base.require(Main, :ExtD)
                println("NO_CONFLICT")
            catch err
                msg = sprint(showerror, err)
                if occursin("already taken", msg)
                    println("CONFLICT_OK")
                else
                    println("WRONG_ERROR: ", msg)
                end
            end
            """
        ok, out, err = run_sub(load_path, depot, code)
        if !ok || !occursin("CONFLICT_OK", out)
            @info "conflict run failed" out err
        end
        @test ok
        @test occursin("CONFLICT_OK", out)
    end
end
