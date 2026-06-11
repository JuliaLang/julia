# This file is a part of Julia. License is MIT: https://julialang.org/license

# End-to-end checks of the juliac-compiled frontend library through its C
# ABI (via frontend_driver), compared where possible against the flisp
# reference frontend available in the running julia.
#
# Usage: julia test_frontend_parity.jl <frontend_driver> <libjulia-frontend-jl.so>

const driver = ARGS[1]
const frontend_lib = ARGS[2]

drive(mode, arg) = readchomp(`$driver $frontend_lib $mode $arg`)

nfail = 0
function check(name, cond)
    global nfail
    if cond
        println("PASS ", name)
    else
        nfail += 1
        println("FAIL ", name)
    end
end

# --- Parse parity vs flisp on snippets ------------------------------------
# JuliaSyntax is Expr-compatible with flisp on these; compare exact reprs.
snippets = [
    "f(x) = 2x + 1",
    "for i in 1:10\n    s += i^2\nend",
    "function g(a::Int, b=2; kw...)\n    return a * b\nend",
    "module M\nstruct Point{T}\n    x::T\n    y::T\nend\nend",
    "[a for a in xs if a > 0]",
    "try\n    f()\ncatch err\n    rethrow()\nfinally\n    done()\nend",
    "a .+= b' * \"str\\n\" ^ 2",
    "@inbounds x[i] += 1",
    "quote\n    \$x + \$(esc(y))\nend",
    "let (a, b) = t, c = 1\n    a < b ? c : -c\nend",
    "x = [1 2; 3 4]'",
    "T where {S <: Integer, T <: AbstractArray{S}}",
]
for (i, code) in enumerate(snippets)
    fl = repr(Base.fl_parse(code, "snippet", 1, 0, :all)[1])
    tmp = tempname()
    write(tmp, code)
    js = drive("parse", tmp)
    rm(tmp)
    # the driver uses the file path as the parse filename; normalize it away
    js = replace(js, r"#= [^ ]*:(\d+) =#" => s"#= FILE:\1 =#")
    fl = replace(fl, r"#= [^ ]*:(\d+) =#" => s"#= FILE:\1 =#")
    check("parse snippet $i", fl == js)
    if fl != js
        println("  flisp: ", fl, "\n  jsfe:  ", js)
    end
end

# --- Parse parity on real Base source files --------------------------------
# The compiled library must agree exactly with the in-process JuliaSyntax
# parser (same engine, exercised through the C ABI). Agreement with flisp is
# reported for information; flisp has known line-attribution divergences
# (e.g. multi-line short-form definitions) that JuliaSyntax intentionally
# does not replicate.
for file in ["abstractset.jl", "sort.jl"]
    path = joinpath(Sys.BINDIR, "..", "..", "base", file)
    isfile(path) || (path = joinpath(dirname(dirname(Sys.BINDIR)), "base", file))
    if isfile(path)
        code = read(path, String)
        js_host = repr(Base.JuliaSyntax.core_parser_hook(code, path, 1, 0, :all)[1])
        js_abi = drive("parse", path)
        check("parse base/$file (ABI == in-process JuliaSyntax)", js_host == js_abi)
        fl = repr(Base.fl_parse(code, path, 1, 0, :all)[1])
        println("  info: flisp agreement on base/$file: ", fl == js_abi)
    else
        println("SKIP base/$file (source not found)")
    end
end

# --- Lowering: behavioral checks through the ABI ---------------------------
# JuliaLowering's IR is not textually identical to flisp's, so check that
# lowering through the C ABI produces code that evaluates correctly.
evalchecks = [
    ("1 + 2", "3"),
    ("let s = 0; for i = 1:10; s += i; end; s; end", "55"),
    ("(function (x); y -> y + x; end)(2)(3)", "5"),
    ("begin; local q = [i^2 for i in 1:4]; sum(q); end", "30"),
    ("try; error(\"boom\"); catch e; sprint(showerror, e); end", "\"boom\""),
    ("struct PtX; a::Int; end; PtX(7).a", "7"),
]
for (code, expected) in evalchecks
    got = try
        drive("eval", code)
    catch err
        sprint(showerror, err)
    end
    check("lower+eval $(repr(code))", got == expected)
    got == expected || println("  expected: ", expected, "\n  got:      ", got)
end

# --- Operator queries through the ABI vs flisp ------------------------------
let cands = ["+", "-", "*", "//", "^", "::", "<:", "->", "=", "+=", ".+", ".=",
             "&&", ".&&", ":", "<<", "...", "..", "\$", "&", "!", "√", "⊕", "~",
             "==", "<|", "|>", "where", ".", "isa", "in", "≔", "⊻=", "+₁", "⊕₁",
             "->₁", ".\$", "im", "foo"]
    tmp = tempname()
    write(tmp, join(cands, "\n") * "\n")
    out = readchomp(`$driver $frontend_lib ops $tmp`)
    rm(tmp)
    nbad = 0
    for line in split(out, '\n')
        parts = split(line, '\t')
        s = String(parts[1])
        abi = parse.(Int, parts[2:6])
        fl = Int[ccall(:jl_is_operator, Cint, (Cstring,), s) != 0,
                 ccall(:jl_is_unary_operator, Cint, (Cstring,), s) != 0,
                 ccall(:jl_is_unary_and_binary_operator, Cint, (Cstring,), s) != 0,
                 ccall(:jl_is_syntactic_operator, Cint, (Cstring,), s) != 0,
                 ccall(:jl_operator_precedence, Cint, (Cstring,), s)]
        if abi != fl
            nbad += 1
            println("  op mismatch ", repr(s), ": abi=", abi, " flisp=", fl)
        end
    end
    check("operator queries via ABI ($(length(cands)) candidates)", nbad == 0)
end

# --- Macro expansion through the ABI ---------------------------------------
got = try
    drive("macroexpand", "@assert true")
catch err
    sprint(showerror, err)
end
check("macroexpand @assert", occursin("AssertionError", got))
got2 = try
    drive("eval", "@show 1 + 1")
catch err
    sprint(showerror, err)
end
check("eval with macro", occursin("2", got2))

println(nfail == 0 ? "ALL PASSED" : "$nfail FAILURES")
exit(nfail == 0 ? 0 : 1)
