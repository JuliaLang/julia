# This file is a part of Julia. License is MIT: https://julialang.org/license

# Compare the JuliaSyntax-based operator classification in
# JuliaFrontend/src/flisp_ops.jl against the flisp reference implementation
# (reached via the libjulia-frontend trampolines). Must be run with a julia
# whose frontend library is the flisp one (i.e. the in-tree build).
#
# Usage: julia test_ops_parity.jl [--verbose] [--full]
#
# By default sweeps the basic multilingual plane plus multi-character
# combinations (a few minutes); --full extends the single-codepoint sweep
# through the supplementary planes used for operators.

module OpsParityHarness

const _JS = Base.JuliaSyntax
include(joinpath(@__DIR__, "JuliaFrontend", "src", "flisp_ops.jl"))

fl_ops(s) = (
    ccall(:jl_is_operator, Cint, (Cstring,), s) != 0,
    ccall(:jl_is_unary_operator, Cint, (Cstring,), s) != 0,
    ccall(:jl_is_unary_and_binary_operator, Cint, (Cstring,), s) != 0,
    ccall(:jl_is_syntactic_operator, Cint, (Cstring,), s) != 0,
    Int(ccall(:jl_operator_precedence, Cint, (Cstring,), s)))

js_ops(s) = begin
    info = _opinfo(s)
    (info.isop, info.unary, info.unary_binary, info.syntactic, Int(info.prec))
end

function candidates(full::Bool)
    cands = String[]
    # All single codepoints
    for c in 0x21:(full ? 0x0002ffff : 0x0000ffff)
        ((0xd800 <= c <= 0xdfff) || !Base.isvalid(Char, c)) && continue
        push!(cands, string(Char(c)))
    end
    # Multi-character operators: every operator token kind with a string form
    multi = String[]
    for i in 0:Int(typemax(UInt16))
        k = try _JS.Kind(i) catch; continue end
        _JS.is_operator(k) || continue
        str = _JS.untokenize(k)
        str === nothing && continue
        push!(multi, str)
    end
    append!(multi, ["==", "===", "!==", "!=", "<=", ">=", "->", "-->", "<--", "<-->",
                    "<<", ">>", ">>>", "//", "..", "...", "....", "::", ":::", "'", "''",
                    "&&", "||", "|>", "<|", "++", "%", "÷", "⊻", "==>", "<==", "<==>",
                    "isa", "in", "where", "im", "foo", "if", "=", ":=", "~", "?", "\$"])
    unique!(multi)
    append!(cands, multi)
    # Compound assignments
    for op in ["+", "-", "*", "/", "//", "\\", "^", "÷", "%", "<<", ">>", ">>>",
               "|", "&", "⊻", "\$", "==", "<", "..."]
        push!(cands, op * "=")
    end
    # Dotted and suffixed variants of everything so far (covers .+, .&&, +₁, ⊕₁ etc.)
    base = copy(cands)
    for s in base
        push!(cands, "." * s)
        push!(cands, s * "₁")
        push!(cands, s * "ᵀ")
    end
    # Pairs of common ascii op chars, mostly invalid combinations
    opchars = collect("+-*/\\^<>=!~&|%:?.\$")
    for a in opchars, b in opchars
        push!(cands, string(a, b))
        push!(cands, string(a, b, b))
    end
    unique!(cands)
    return cands
end

function run(verbose::Bool, full::Bool)
    cands = candidates(full)
    nmismatch = 0
    shown = 0
    for s in cands
        f = fl_ops(s)
        j = try
            js_ops(s)
        catch err
            println("ERROR classifying ", repr(s), ": ", sprint(showerror, err))
            nmismatch += 1
            shown += 1
            continue
        end
        if f != j
            nmismatch += 1
            if shown < 60 || verbose
                println("MISMATCH ", rpad(repr(s), 12),
                        " flisp=(op=", f[1], ",un=", f[2], ",ub=", f[3], ",syn=", f[4], ",prec=", f[5], ")",
                        " js=(op=", j[1], ",un=", j[2], ",ub=", j[3], ",syn=", j[4], ",prec=", j[5], ")")
                shown += 1
            end
        end
    end
    println("checked ", length(cands), " candidates, ", nmismatch, " mismatches")
    return nmismatch
end

end # module

exit(OpsParityHarness.run(any(==("--verbose"), ARGS), any(==("--full"), ARGS)) == 0 ? 0 : 1)
