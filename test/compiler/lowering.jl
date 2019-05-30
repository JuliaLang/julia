using Core: SSAValue

# Call into lowering stage 1; syntax desugaring
function fl_expand_forms(ex)
    ccall(:jl_call_scm_on_ast_formonly, Any, (Cstring, Any, Any), "expand-forms", ex, Main)
end

function lift_lowered_expr!(ex, nextid, valmap, lift_full)
    if ex isa SSAValue
        # Rename SSAValues into renumbered symbols
        return get!(valmap, ex) do
            newid = nextid[]
            nextid[] = newid+1
            Symbol("ssa$newid")
        end
    end
    if ex isa Expr
        filter!(e->!(e isa LineNumberNode), ex.args)
        if ex.head == :block && length(ex.args) == 1
            # Remove trivial blocks
            return lift_lowered_expr!(ex.args[1], nextid, valmap, lift_full)
        end
        map!(ex.args, ex.args) do e
            lift_lowered_expr!(e, nextid, valmap, lift_full)
        end
        if lift_full && ex.head == :top
            return Expr(:(.), :Top, QuoteNode(ex.args[1]))
        end
        if lift_full && ex.head == :core
            return Expr(:(.), :Core, QuoteNode(ex.args[1]))
        end
        if lift_full && ex.head == :unnecessary
            # Rename to something we can actually type
            return Expr(:call, :maybe_unused, ex.args...)
        end
    end
    return ex
end

"""
Clean up an `Expr` into an equivalent form which can be easily entered by
hand

* Replacing `SSAValue(id)` with consecutively numbered symbols :ssa\$i
* Remove trivial blocks
"""
function lift_lowered_expr(ex; lift_full=false)
    valmap = Dict{SSAValue,Symbol}()
    nextid = Ref(0)
    lift_lowered_expr!(deepcopy(ex), nextid, valmap, lift_full)
end

"""
Very slight lowering of reference expressions to allow comparison with
desugared forms.

* Remove trivial blocks
* Translate psuedo-module expressions Top.x and Core.x to Expr(:top) and
  Expr(:core)
"""
function lower_ref_expr!(ex)
    if ex isa Expr
        filter!(e->!(e isa LineNumberNode), ex.args)
        if ex.head == :block && length(ex.args) == 1
            # Remove trivial blocks
            return lower_ref_expr!(ex.args[1])
        end
        if ex.head == :(.) && length(ex.args) >= 1 && (ex.args[1] == :Top ||
                                                       ex.args[1] == :Core)
            (length(ex.args) == 2 && ex.args[2] isa QuoteNode) || throw("Unexpected top/core expression $(sprint(dump, ex))")
            return Expr(ex.args[1] == :Top ? :top : :core, ex.args[2].value)
        end
        if ex.head == :call && length(ex.args) >= 1 && ex.args[1] == :maybe_unused
            return Expr(:unnecessary, ex.args[2:end]...)
        end
        map!(lower_ref_expr!, ex.args, ex.args)
    end
    return ex
end
lower_ref_expr(ex) = lower_ref_expr!(deepcopy(ex))


function diffdump(io::IOContext, ex1, ex2, n, prefix, indent)
    if ex1 == ex2
        isempty(prefix) || print(io, prefix)
        dump(io, ex1, n, indent)
    else
        if ex1 isa Expr && ex2 isa Expr && ex1.head == ex2.head && length(ex1.args) == length(ex2.args)
            isempty(prefix) || print(io, prefix)
            println(io, "Expr")
            println(io, indent, "  head: ", ex1.head)
            println(io, indent, "  args: Array{Any}(", size(ex1.args), ")")
            for i in 1:length(ex1.args)
                prefix = string(indent, "    ", i, ": ")
                diffdump(io, ex1.args[i], ex2.args[i], n - 1, prefix, string("    ", indent))
                i < length(ex1.args) && println(io)
            end
        else
            printstyled(io, string(prefix, sprint(dump, ex1, n, indent; context=io)), color=:red)
            println()
            printstyled(io, string(prefix, sprint(dump, ex2, n, indent; context=io)), color=:green)
        end
    end
end

"""
Display colored differences between two expressions `ex1` and `ex2` using the
`dump` format.
"""
function diffdump(ex1, ex2; maxdepth=20)
    mod = get(stdout, :module, Main)
    diffdump(IOContext(stdout, :limit => true, :module => mod), ex1, ex2, maxdepth, "", "")
    println(stdout)
end

# For interactive convenience in constructing test cases with flisp based lowering
desugar(ex) = lift_lowered_expr(fl_expand_forms(ex); lift_full=true)

macro desugar(ex)
    quote
        desugar($(QuoteNode(ex)))
    end
end

"""
Test that syntax desugaring of `input` produces an expression equivalent to the
reference expression `ref`.
"""
macro test_desugar(input, ref)
    ex = quote
        input = lift_lowered_expr(fl_expand_forms($(QuoteNode(input))))
        ref   = lower_ref_expr($(QuoteNode(ref)))
        @test input == ref
        if input != ref
            println("Diff dump:")
            diffdump(input, ref)
        end
    end
    # Attribute the test to the correct line number
    @assert ex.args[6].args[1] == Symbol("@test")
    ex.args[6].args[2] = __source__
    ex
end

@testset "Property notation" begin
    @test_desugar(a.b,   Top.getproperty(a, :b))
    @test_desugar(a.b.c, Top.getproperty(Top.getproperty(a, :b), :c))

    @test_desugar(
        a.b = c,
        begin
            Top.setproperty!(a, :b, c)
            maybe_unused(c)
        end
    )
    @test_desugar(
        a.b.c = d,
        begin
            ssa0 = Top.getproperty(a, :b)
            Top.setproperty!(ssa0, :c, d)
            maybe_unused(d)
        end
    )
end

@testset "Index notation" begin
    @testset "getindex" begin
        # Indexing
        @test_desugar a[i]      Top.getindex(a, i) 
        @test_desugar a[i,j]    Top.getindex(a, i, j) 
        # Indexing with `end`
        @test_desugar a[end]    Top.getindex(a, Top.lastindex(a)) 
        @test_desugar a[i,end]  Top.getindex(a, i, Top.lastindex(a,2)) 
        # Nesting of `end`
        @test_desugar a[b[end] + end]  Top.getindex(a, Top.getindex(b, Top.lastindex(b)) + Top.lastindex(a)) 
        @test_desugar a[f(end) + 1]    Top.getindex(a, f(Top.lastindex(a)) + 1) 
        # Interaction of end with splatting
        @test_desugar(a[I..., end],
            Core._apply(Top.getindex, Core.tuple(a), I,
                        Core.tuple(Top.lastindex(a, Top.:+(1, Top.length(I)))))
        )
    end

    @testset "setindex!" begin
        # setindex!
        @test_desugar(
            a[i] = b,
            begin
                Top.setindex!(a, b, i)
                maybe_unused(b)
            end
        )
        @test_desugar(
            a[i,end] = b+c,
            begin
                ssa0 = b+c
                Top.setindex!(a, ssa0, i, Top.lastindex(a,2))
                maybe_unused(ssa0)
            end
        )
    end
end

@testset "Array notation" begin
    @testset "Literals" begin
        @test_desugar [a,b]     Top.vect(a,b)
        @test_desugar T[a,b]    Top.getindex(T, a,b)
    end

    @testset "Concatenation" begin
        @test_desugar [a b]     Top.hcat(a,b)
        @test_desugar [a; b]    Top.vcat(a,b)
        @test_desugar T[a b]    Top.typed_hcat(T, a,b)
        @test_desugar T[a; b]   Top.typed_vcat(T, a,b)
        @test_desugar [a b; c]  Top.hvcat(Core.tuple(2,1), a, b, c)
        @test_desugar T[a b; c] Top.typed_hvcat(T, Core.tuple(2,1), a, b, c)
    end
end

@testset "Splatting" begin
    @test_desugar(
        f(i, j, v..., k),
        Core._apply(f, Core.tuple(i,j), v, Core.tuple(k))
    )
end

@testset "Comparison chains" begin
    @test_desugar(
        a < b < c,
        if a < b
            b < c
        else
            false
        end
    )
    # Nested
    @test_desugar(
        a < b > d <= e,
        if a < b
            if b > d
                d <= e
            else
                false
            end
        else
            false
        end
    )
    # Subexpressions
    @test_desugar(
        a < b+c < d,
        if (ssa0 = b+c; a < ssa0)
            ssa0 < d
        else
            false
        end
    )

    # Interaction with broadcast syntax
    @test_desugar(
        a < b .< c,
        Top.materialize(Top.broadcasted(&, a < b, Top.broadcasted(<, b, c)))
    )
    @test_desugar(
        a .< b+c < d,
        Top.materialize(Top.broadcasted(&,
                                        begin
                                            ssa0 = b+c
                                            # Is this a bug?
                                            Top.materialize(Top.broadcasted(<, a, ssa0))
                                        end,
                                        ssa0 < d))
    )
    @test_desugar(
        a < b+c .< d,
        Top.materialize(Top.broadcasted(&,
                                        begin
                                            ssa0 = b+c
                                            a < ssa0
                                        end,
                                        Top.broadcasted(<, ssa0, d)))
    )
end

