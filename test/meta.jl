# This file is a part of Julia. License is MIT: https://julialang.org/license

# test meta-expressions that annotate blocks of code

const inlining_on = Base.JLOptions().can_inline != 0

function f(x)
    y = x+5
    z = y*y
    q = z/y
    m = q-3
end

@inline function f_inlined(x)
    y = x+5
    z = y*y
    q = z/y
    m = q-3
end

g(x) = f(2x)
g_inlined(x) = f_inlined(2x)

@test g(3) == g_inlined(3)
@test f(3) == f_inlined(3)

f() = backtrace()
@inline g_inlined() = f()
@noinline g_noinlined() = f()
h_inlined() = g_inlined()
h_noinlined() = g_noinlined()

function foundfunc(bt, funcname)
    for b in bt
        for lkup in StackTraces.lookup(b)
            if lkup.func == funcname
                return true
            end
        end
    end
    false
end
@test foundfunc(h_inlined(), :g_inlined)
@test foundfunc(h_noinlined(), :g_noinlined)

using Base: popmeta!

macro attach_meta(val, ex)
    esc(_attach_meta(val, ex))
end
_attach_meta(val, ex) = Base.pushmeta!(ex, Expr(:test, val))

@attach_meta 42 function dummy()
    false
end
let ast = only(code_lowered(dummy, Tuple{}))
    body = Expr(:block)
    body.args = ast.code
    @test popmeta!(body, :test) == (true, [42])
    @test popmeta!(body, :nonexistent) == (false, [])
end

# Simple popmeta!() tests
let ex1 = quote
        $(Expr(:meta, :foo))
        x*x+1
    end
    @test popmeta!(ex1, :foo)[1]
    @test !popmeta!(ex1, :foo)[1]
    @test !popmeta!(ex1, :bar)[1]
    @test !(popmeta!(:(x*x+1), :foo)[1])
end

# Find and pop meta information from general ast locations
let multi_meta = quote
        $(Expr(:meta, :foo1))
        y = x
        $(Expr(:meta, :foo2, :foo3))
        begin
            $(Expr(:meta, :foo4, Expr(:foo5, 1, 2)))
        end
        x*x+1
    end
    @test popmeta!(deepcopy(multi_meta), :foo1) == (true, [])
    @test popmeta!(deepcopy(multi_meta), :foo2) == (true, [])
    @test popmeta!(deepcopy(multi_meta), :foo3) == (true, [])
    @test popmeta!(deepcopy(multi_meta), :foo4) == (true, [])
    @test popmeta!(deepcopy(multi_meta), :foo5) == (true, [1,2])
    @test popmeta!(deepcopy(multi_meta), :bar)  == (false, [])

    # Test that popmeta!() removes meta blocks entirely when they become empty.
    ast = :(dummy() = $multi_meta)
    for m in [:foo1, :foo2, :foo3, :foo4, :foo5]
        @test popmeta!(multi_meta, m)[1]
    end
    @test Base.findmeta(ast)[1] == 0
end

# Test that pushmeta! can push across other macros,
# in the case multiple pushmeta!-based macros are combined
@attach_meta 40 @attach_meta 41 @attach_meta 42 dummy_multi() = return nothing
let ast = only(code_lowered(dummy_multi, Tuple{}))
    body = Expr(:block)
    body.args = ast.code
    @test popmeta!(body, :test) == (true, [40])
    @test popmeta!(body, :test) == (true, [41])
    @test popmeta!(body, :test) == (true, [42])
    @test popmeta!(body, :nonexistent) == (false, [])
end

# tests to fully cover functions in base/meta.jl
using Base.Meta

@test isexpr(:(1+1),Set([:call]))
@test isexpr(:(1+1),Vector([:call]))
@test isexpr(:(1+1),(:call,))
@test isexpr(1,:call)==false
@test isexpr(:(1+1),:call,3)

let
    fakeline = LineNumberNode(100000,"A")
    # Interop with __LINE__
    @test macroexpand(@__MODULE__, replace_sourceloc!(fakeline, :(@__LINE__))) == fakeline.line
    # replace_sourceloc! should recurse:
    @test replace_sourceloc!(fakeline, :((@a) + 1)).args[2].args[2] == fakeline
    @test replace_sourceloc!(fakeline, :(@a @b)).args[3].args[2] == fakeline
end

ioB = IOBuffer()
show_sexpr(ioB,:(1+1))

show_sexpr(ioB,QuoteNode(1),1)

# test base/expr.jl
baremodule B
    eval = 0
    x = 1
    module M; x = 2; end
    import Base
    Base.@eval x = 3
    Base.@eval M x = 4
end
@test B.x == 3
@test B.M.x == 4

# specialization annotations

function _nospec_some_args(@nospecialize(x), y, @nospecialize z::Int)
end
@test first(methods(_nospec_some_args)).nospecialize == 5
@test first(methods(_nospec_some_args)).sig == Tuple{typeof(_nospec_some_args),Any,Any,Int}
function _nospec_some_args2(x, y, z)
    @nospecialize x y
    return 0
end
@test first(methods(_nospec_some_args2)).nospecialize == 3
function _nospec_with_default(@nospecialize x = 1)
    2x
end
@test collect(methods(_nospec_with_default))[2].nospecialize == 1
@test _nospec_with_default() == 2
@test _nospec_with_default(10) == 20


let oldout = stdout
    ex = Meta.@lower @dump x + y
    local rdout, wrout, out
    try
        rdout, wrout = redirect_stdout()
        out = @async read(rdout, String)

        @test eval(ex) === nothing

        redirect_stdout(oldout)
        close(wrout)

        @test fetch(out) == """
            Expr
              head: Symbol call
              args: Array{Any}((3,))
                1: Symbol +
                2: Symbol x
                3: Symbol y
            """
    finally
        redirect_stdout(oldout)
    end
end

macro is_dollar_expr(ex)
    return Meta.isexpr(ex, :$)
end

module TestExpandModule
macro is_in_def_module()
    return __module__ === @__MODULE__
end
end

let a = 1
    @test @is_dollar_expr $a
    @test !TestExpandModule.@is_in_def_module
    @test @eval TestExpandModule @is_in_def_module

    @test Meta.lower(@__MODULE__, :($a)) === 1
    @test !Meta.lower(@__MODULE__, :(@is_dollar_expr $a))
    @test Meta.@lower @is_dollar_expr $a
    @test Meta.@lower @__MODULE__() @is_dollar_expr $a
    @test !Meta.@lower TestExpandModule.@is_in_def_module
    @test Meta.@lower TestExpandModule @is_in_def_module

    @test macroexpand(@__MODULE__, :($a)) === 1
    @test !macroexpand(@__MODULE__, :(@is_dollar_expr $a))
    @test @macroexpand @is_dollar_expr $a
end

let ex = Meta.parse("@foo"; filename=:bar)
    @test Meta.isexpr(ex, :macrocall)
    arg2 = ex.args[2]
    @test isa(arg2, LineNumberNode) && arg2.file === :bar
end
let ex = Meta.parseatom("@foo", 1, filename=:bar)[1]
    @test Meta.isexpr(ex, :macrocall)
    arg2 = ex.args[2]
    @test isa(arg2, LineNumberNode) && arg2.file === :bar
end
let ex = Meta.parseall("@foo", filename=:bar)
    @test Meta.isexpr(ex, :toplevel)
    arg1 = ex.args[1]
    @test isa(arg1, LineNumberNode) && arg1.file === :bar
    arg2 = ex.args[2]
    @test Meta.isexpr(arg2, :macrocall)
    arg2arg2 = arg2.args[2]
    @test isa(arg2arg2, LineNumberNode) && arg2arg2.file === :bar
end

_lower(m::Module, ex, world::UInt) = Base.fl_lower(m, ex, world, false, LineNumberNode(0, :none))[1]

module TestExpandInWorldModule
macro m() 1 end
wa = Base.get_world_counter()
macro m() 2 end
end

@test _lower(TestExpandInWorldModule, :(@m), TestExpandInWorldModule.wa) == 1

f(::T) where {T} = T
ci = code_lowered(f, Tuple{Int})[1]
@test Meta.partially_inline!(ci.code, [], Tuple{typeof(f),Int}, Any[Int], 0, 0, :propagate) ==
    Any[QuoteNode(Int), Core.ReturnNode(Core.SSAValue(1))]

g(::Val{x}) where {x} = x ? 1 : 0
ci = code_lowered(g, Tuple{Val{true}})[1]
@test Meta.partially_inline!(ci.code, [], Tuple{typeof(g),Val{true}}, Any[true], 0, 0, :propagate)[2] ==
   Core.GotoIfNot(Core.SSAValue(1), 4)
@test Meta.partially_inline!(ci.code, [], Tuple{typeof(g),Val{true}}, Any[true], 0, 2, :propagate)[2] ==
   Core.GotoIfNot(Core.SSAValue(3), 6)

@testset "inlining with isdefined" begin
    isdefined_slot(x) = @isdefined(x)
    ci = code_lowered(isdefined_slot, Tuple{Int})[1]
    @test Meta.partially_inline!(copy(ci.code), [], Tuple{typeof(isdefined_slot), Int},
                                 [], 0, 0, :propagate)[1] == Expr(:isdefined, Core.SlotNumber(2))
    @test Meta.partially_inline!(copy(ci.code), [isdefined_slot, 1], Tuple{typeof(isdefined_slot), Int},
                                 [], 0, 0, :propagate)[1] == true

    isdefined_sparam(::T) where {T} = @isdefined(T)
    ci = code_lowered(isdefined_sparam, Tuple{Int})[1]
    @test Meta.partially_inline!(copy(ci.code), [], Tuple{typeof(isdefined_sparam), Int},
                                 Any[Int], 0, 0, :propagate)[1] == true
    @test Meta.partially_inline!(copy(ci.code), [], Tuple{typeof(isdefined_sparam), Int},
                                 [], 0, 0, :propagate)[1] == Expr(:isdefined, Expr(:static_parameter, 1))

    @eval isdefined_globalref(x) = $(Expr(:isdefined, GlobalRef(Base, :foo)))
    ci = code_lowered(isdefined_globalref, Tuple{Int})[1]
    @test Meta.partially_inline!(copy(ci.code), Any[isdefined_globalref, 1], Tuple{typeof(isdefined_globalref), Int},
                                 [], 0, 0, :propagate)[1] == Expr(:call, GlobalRef(Core, :isdefinedglobal), Base, QuoteNode(:foo))

    withunreachable(s::String) = sin(s)
    ci = code_lowered(withunreachable, Tuple{String})[1]
    ci.code[end] = Core.ReturnNode()
    @test Meta.partially_inline!(copy(ci.code), Any[withunreachable, "foo"], Tuple{typeof(withunreachable), String},
                                 [], 0, 0, :propagate)[end] == Core.ReturnNode()
end

# Generated function that runs a method's IR after moving it past two prepended statements
# and past the extra slot that holds the kernel.
function shifted_kernel_gen(world::UInt, source, _, K, X)
    match = only(Base._methods_by_ftype(Tuple{K, X}, -1, world))
    ci = Base.uncompressed_ir(match.method)
    kernel_ir!(K.instance, ci)
    Meta.partially_inline!(ci.code, Any[QuoteNode(K.instance)], match.method.sig,
                           Any[match.sparams...], 1, 2, :propagate)
    prepend!(ci.code, Any[nothing, nothing])
    ci.ssavaluetypes = length(ci.code)
    ci.ssaflags = zeros(UInt32, length(ci.code))
    ci.debuginfo = Core.DebugInfo(:shifted_kernel)
    ci.slotnames = Symbol[Symbol("#self#"), :kernel, ci.slotnames[2:end]...]
    ci.slotflags = UInt8[0x00, 0x00, ci.slotflags[2:end]...]
    ci.nargs = 3
    return ci
end
kernel_ir!(_, ci) = ci
phi_kernel(c) = c
function kernel_ir!(::typeof(phi_kernel), ci)
    ci.code = Any[Core.GotoIfNot(Core.SlotNumber(2), 3), Core.GotoNode(4), Core.GotoNode(4),
                  Core.PhiNode(Int32[2, 3], Any[1, 2]), Core.ReturnNode(Core.SSAValue(4))]
    return ci
end
@eval function run_shifted(kernel, x)
    $(Expr(:meta, :generated, shifted_kernel_gen))
    $(Expr(:meta, :generated_only))
end

const scoped_kernel_value = Base.ScopedValues.ScopedValue(1)
function scoped_kernel(c)
    Base.ScopedValues.@with scoped_kernel_value => 2 begin
        try
            c ? scoped_kernel_value[] : error()
        catch
            -scoped_kernel_value[]
        end
    end
end
function ccall_kernel(x::T) where {T}
    cf = @cfunction(identity, Ref{T}, (Ref{T},))
    return ccall(cf, Ref{T}, (Ref{T},), x)
end
strlen_kernel(s::Vector{T}) where {T} = ccall(:strlen, Csize_t, (Ref{T},), s)
gc_safe_kernel(s::Vector{UInt8}) = @ccall gc_safe=true strlen(s::Ptr{UInt8})::Csize_t

@testset "partially_inline! keeps a moved method working" begin
    @test run_shifted(phi_kernel, true) === 1
    @test run_shifted(phi_kernel, false) === 2
    @test run_shifted(scoped_kernel, true) === 2
    @test run_shifted(scoped_kernel, false) === -2
    @test run_shifted(ccall_kernel, 1) === 1
    @test run_shifted(ccall_kernel, "a") === "a"
    @test run_shifted(strlen_kernel, UInt8[97, 98, 0]) == 2
    @test run_shifted(gc_safe_kernel, UInt8[97, 98, 0]) == 2
end

@testset "show_sexpr" begin
    @test sprint(Meta.show_sexpr, Expr(:call)) == "(:call,)"
    @test sprint(Meta.show_sexpr, Expr(:block)) == "(:block,)"
    @test sprint(Meta.show_sexpr, Expr(:tuple, 1)) == "(:tuple, 1)"
    mktemp() do path, io
        redirect_stdout(() -> Meta.show_sexpr(:(f(x))), io)
        close(io)
        @test read(path, String) == "(:call, :f, :x)"
    end
end

@testset "Meta.parse error forms" begin
    extra_token = "extra token after end of expression"
    @test Meta.ParseError("msg") == Meta.ParseError("msg", nothing)
    @test_throws Meta.ParseError(extra_token) Meta.parse("1\n2")
    @test Meta.parse("1\n2"; raise=false) == Expr(:error, extra_token)
end

# The tests below pin the details of internal functions. If they get in the way of changing
# those internals they can be deleted, but weigh the coverage that would be lost.

using Core: SSAValue, SlotNumber, PhiNode, PhiCNode, PiNode, UpsilonNode, EnterNode, GotoNode, GotoIfNot, NewvarNode

@testset "partially_inline! on IR nodes" begin
    inl(x; slots=Any[], sparams=Any[], bc=:propagate) =
        Meta.partially_inline!(Any[x], slots, Tuple{}, sparams, 100, 10, bc)[1]

    @test inl(GotoNode(3)) === GotoNode(13)
    @test inl(NewvarNode(SlotNumber(2))) === NewvarNode(SlotNumber(102))
    @test inl(NewvarNode(SlotNumber(1)); slots=Any[SlotNumber(7)]) === NewvarNode(SlotNumber(7))
    @test !isdefined(inl(UpsilonNode()), :val)
    @test inl(UpsilonNode(SSAValue(1))) === UpsilonNode(SSAValue(11))
    @test inl(UpsilonNode(SlotNumber(1)); slots=Any[42]) === UpsilonNode(42)
    @test inl(Expr(:gotoifnot, SSAValue(1), 3)) == Expr(:gotoifnot, SSAValue(11), 13)
    @test inl(GotoIfNot(SlotNumber(1), 3); slots=Any[true]) === GotoIfNot(true, 13)

    @test inl(EnterNode(0)) === EnterNode(0)
    @test !isdefined(inl(EnterNode(0)), :scope)
    @test inl(EnterNode(5)) === EnterNode(15)
    @test !isdefined(inl(EnterNode(5)), :scope)
    @test inl(EnterNode(5, SSAValue(1))) === EnterNode(15, SSAValue(11))
    @test inl(EnterNode(0, SSAValue(1))) === EnterNode(0, SSAValue(11))
    @test inl(EnterNode(5, SlotNumber(1)); slots=Any[42]) === EnterNode(15, 42)

    @test inl(PiNode(SSAValue(1), Int)) === PiNode(SSAValue(11), Int)

    let phi = inl(PhiNode(Int32[1, 2], Any[SSAValue(1), SlotNumber(1)]); slots=Any[42])
        @test phi.edges == Int32[11, 12]
        @test isequal(phi.values, Any[SSAValue(11), 42])
    end
    let values = Vector{Any}(undef, 2)
        values[1] = SSAValue(1)
        phi = inl(PhiNode(Int32[1, 2], values))
        @test phi.values[1] === SSAValue(11)
        @test !isassigned(phi.values, 2)
    end
    @test inl(PhiCNode(Any[SSAValue(1), SSAValue(2)])) == PhiCNode(Any[SSAValue(11), SSAValue(12)])

    @test inl(Expr(:boundscheck); bc=:propagate) == Expr(:boundscheck)
    @test inl(Expr(:boundscheck); bc=:off) === false
    @test inl(Expr(:boundscheck); bc=:on) === true
    @test inl(Expr(:call, :f, Expr(:boundscheck)); bc=:off) == Expr(:call, :f, false)

    for replacement in (SlotNumber(9), GlobalRef(Base, :sin), :y)
        @test inl(Expr(:isdefined, SlotNumber(1)); slots=Any[replacement]) == Expr(:isdefined, replacement)
    end
    @test inl(Expr(:isdefined, SlotNumber(1)); slots=Any[SSAValue(3)]) === true
    @test inl(Expr(:isdefined, SlotNumber(2)); slots=Any[1]) == Expr(:isdefined, SlotNumber(102))
    @test inl(Expr(:isdefined, GlobalRef(Base, :sin))) == Expr(:isdefined, GlobalRef(Base, :sin))
    @test inl(Expr(:isdefined, :y)) == Expr(:isdefined, :y)

    closure_cfunction(g, ::T) where {T} = @cfunction($g, Ref{T}, (Ref{T},))
    ci = code_lowered(closure_cfunction, Tuple{Any, Int})[1]
    @test Meta.partially_inline!(ci.code, Any[], only(methods(closure_cfunction)).sig, Any[Int], 100, 10, :propagate)[1] ==
        Expr(:cfunction, Base.CFunction, SlotNumber(102), Ref{Int}, Core.svec(Ref{Int}), QuoteNode(:ccall))
end

@testset "Meta.parse with internal parser arguments" begin
    extra_token = "extra token after end of expression"
    let err = Meta.parse("1\n2"; raise=false, type=Base.Syntax, filename="a.jl")
        @test err isa Base.Syntax
        @test Base.syntax_to_expr(err) == Expr(:error, extra_token)
    end
    # the flisp parser reports errors as plain strings
    fl_parse(args...) = Base.fl_parse(args..., (1, 14))
    @test_throws Meta.ParseError("\"*\" is not a unary operator") Meta.parse("1 +* 2", 1; _parse=fl_parse)
end

const escape_test_var = :from_caller
module EscapeHelpers
const escape_test_var = :from_macro_module
end

@testset "unblock, unescape, reescape and uncurly" begin
    lnn = LineNumberNode(1, :file)
    esc1(x) = Expr(:escape, x)
    hyg(x) = Expr(:var"hygienic-scope", x, EscapeHelpers, lnn)

    @test Meta.unblock(Expr(:block, lnn, :x)) === :x
    @test Meta.unblock(Expr(:block, lnn, Expr(:block, Expr(:line, 1), :x))) === :x
    @test Meta.unblock(Expr(:block, :x, :y)) == Expr(:block, :x, :y)
    @test Meta.unblock(hyg(esc1(:x))) === :x
    @test Meta.unblock(hyg(:x)) == hyg(:x)
    @test Meta.unblock(hyg(esc1(hyg(esc1(Expr(:block, lnn, :x)))))) === :x

    for wrapped in (esc1(:x), hyg(:x), hyg(esc1(:x)), esc1(hyg(:x)), esc1(esc1(:x)), hyg(hyg(esc1(esc1(:x)))))
        @test Meta.unescape(wrapped) === :x
        @test Meta.reescape(:x, wrapped) == wrapped
    end
    @test Meta.unescape(Expr(:block, lnn, esc1(:x))) === :x
    @test Meta.reescape(:x, :y) === :x

    @test eval(Meta.reescape(:escape_test_var, hyg(esc1(:x)))) === :from_caller

    @test Meta.uncurly(:(A{B})) === :A
    @test Meta.uncurly(:(A{B}{C})) == :(A{B})
    @test Meta.uncurly(:A) === :A
end

@testset "Base.Meta docstrings" begin
    @test isempty(Docs.undocumented_names(Meta))
end

@testset "Meta.parse uses the oldest edition by default (no module specified)" begin
    tg = "typegroup struct MetaTG end end"

    @test_throws Meta.ParseError Meta.parse(tg)
    @test Meta.isexpr(Meta.parseall(tg).args[end], :error)
    @test_throws Meta.ParseError Meta.parse(tg; mod=Module(:MetaTGDefault))

    # 1.13 parser
    p13 = Base.VersionedParse(Base.OLDEST_EDITION)
    @test_throws Meta.ParseError Meta.parse(tg; _parse=p13)
    @test Meta.isexpr(Meta.parse(tg; raise=false, _parse=p13), :error)

    # 1.13 module
    m13 = Module(:MetaTG13)
    Base.set_syntax_version(m13, v"1.13")
    @test_throws Meta.ParseError invokelatest(Meta.parse, tg; mod=m13)

    # 1.14 module
    m14 = Module(:MetaTG14)
    Base.set_syntax_version(m14, v"1.14")
    @test Meta.isexpr(Meta.parse(tg; mod=m14), :typegroup)
end
