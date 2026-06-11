# This file is a part of Julia. License is MIT: https://julialang.org/license

# juliac input script for building libjulia-frontend-jl: a drop-in
# implementation of the libjulia-frontend interface compiled from
# JuliaSyntax/JuliaLowering. See the Makefile in this directory.

using JuliaFrontend
using JuliaFrontend.JuliaSyntax
using JuliaFrontend.JuliaLowering

# Make the flisp frontend unreachable from Julia code in this image: the
# default Core._parse/Core._lower hooks installed during bootstrap are
# Base.fl_parse/Base.fl_lower, which reach flisp through the runtime's
# frontend trampolines. Base.__init__ replaces the parser with JuliaSyntax
# (when JULIA_USE_FLISP_PARSER is unset), but lowering would still go to
# flisp; these method overrides reroute both to this package
# unconditionally. Note this also means the remainder of this image build
# is lowered by JuliaLowering.
@eval Base function fl_parse(text::Union{Core.SimpleVector,String},
                             filename::String, lineno, offset, options)
    if text isa Core.SimpleVector
        text, text_len = text
        return $(JuliaFrontend).frontend_parse(text, Int(text_len), filename,
                                             Int(lineno), Int(offset), options)
    end
    GC.@preserve text begin
        return $(JuliaFrontend).frontend_parse(pointer(text), sizeof(text), filename,
                                              Int(lineno), Int(offset), options)
    end
end

@eval Base function fl_lower(ex, mod::Module, filename::Union{String,Ptr{UInt8}}="none",
                             lineno::Integer=0, world::UInt=typemax(Csize_t),
                             warn::Bool=false)
    filename = filename isa Ptr{UInt8} ? unsafe_string(filename) : filename
    return $(JuliaFrontend).frontend_lower(ex, mod, filename, Int(lineno), world, warn)
end

# Exercise the frontend entry points so that their full call graphs are
# compiled into the image: the standalone library runs without a JIT
# (codegen stubs only), so anything not compiled here would run in the
# interpreter.
let
    code = "begin\n    f(x::Int; kw=1) = 2x + kw\n    \"doc\" module M\n    struct P{T}; x::T; end\n    g() = [a^2 for a in 1:10 if a > 0x02]\n    h() = \"s\" * 'c' * string(1.5, 1f0, 0xff, true)\n    end\nend\n"
    for rule in (:all, :statement, :atom)
        GC.@preserve code begin
            JuliaFrontend._c_frontend_parse(pointer(code), Csize_t(sizeof(code)),
                                            "warmup.jl", Csize_t(1), Csize_t(0), rule)
        end
    end
    bad = "f(x) = "
    GC.@preserve bad begin
        JuliaFrontend._c_frontend_parse(pointer(bad), Csize_t(sizeof(bad)),
                                        "warmup.jl", Csize_t(1), Csize_t(0), :statement)
    end
    fname = "warmup.jl"
    # exercise lowering into a bare module, as the cross-runtime path does
    # with its shadow modules
    shadow = Module(:WarmupShadow, false, false)
    GC.@preserve fname begin
        JuliaFrontend._c_frontend_lower(:(function lw(x); y -> y + x; end), Main,
                                        pointer(fname), Cint(1), Csize_t(typemax(Csize_t)), Cint(0))
        JuliaFrontend._c_frontend_lower(
            :(begin
                  s = 0
                  for i = 1:3
                      s += i * i
                  end
                  t = try
                      error("x")
                  catch
                      2
                  end
                  q = [a^2 for a in 1:3]
                  (function (x); y -> y + x; end)(2)(3) + s + t + length(q)
              end), shadow,
            pointer(fname), Cint(1), Csize_t(typemax(Csize_t)), Cint(0))
        JuliaFrontend._c_macroexpand(:(@assert true), Main, Cint(1), Cint(0), Cint(1))
        # exercise the lowering-error path (escape outside of a macro errors)
        JuliaFrontend._c_frontend_lower(Expr(:escape, :x), shadow,
                                        pointer(fname), Cint(1), Csize_t(typemax(Csize_t)), Cint(0))
    end

    for s in ("+", ".+", "+=", "where", "in", "⊕₁", "&&", "::", "...", "im", "√")
        for f in (JuliaFrontend._c_is_operator, JuliaFrontend._c_is_unary_operator,
                  JuliaFrontend._c_is_unary_and_binary_operator,
                  JuliaFrontend._c_is_syntactic_operator, JuliaFrontend._c_operator_precedence)
            GC.@preserve s f(Base.unsafe_convert(Cstring, s))
        end
    end

    # Parse and lower the whole `base/` source corpus through the C-ABI
    # entry points. This is what makes a full julia bootstrap through the
    # standalone library possible: it compiles essentially every
    # JuliaSyntax/JuliaLowering code path that lowering base exercises into
    # the image (anything missed runs in the interpreter, where `ccall`
    # fails).
    juliahome = abspath(joinpath(@__DIR__, "..", ".."))
    corpus_dirs = [joinpath(juliahome, "base"), joinpath(juliahome, "Compiler", "src")]
    function warm_lower(ex, fname)
        if ex isa Expr && (ex.head === :toplevel || ex.head === :block)
            for a in ex.args
                warm_lower(a, fname)
            end
            return
        elseif ex isa Expr && ex.head === :module && length(ex.args) >= 3
            body = ex.args[3]
            body isa Expr && warm_lower(body, fname)
            return
        end
        try
            GC.@preserve fname begin
                JuliaFrontend._c_frontend_lower(ex, Main, pointer(fname), Cint(1),
                                                Csize_t(typemax(Csize_t)), Cint(0))
            end
        catch
        end
    end
    nfiles = 0
    for basedir in corpus_dirs
        isdir(basedir) || continue
        for (root, _, files) in walkdir(basedir)
            for fn in files
                endswith(fn, ".jl") || continue
                path = joinpath(root, fn)
                code = try
                    read(path, String)
                catch
                    continue
                end
                parsed = try
                    GC.@preserve code begin
                        JuliaFrontend._c_frontend_parse(
                            pointer(code), Csize_t(sizeof(code)), path,
                            Csize_t(1), Csize_t(0), :all)
                    end
                catch
                    continue
                end
                warm_lower(parsed[1], path)
                nfiles += 1
                if nfiles % 50 == 0
                    Core.println("frontend warmup: ", nfiles, " files")
                end
            end
        end
        Core.println("frontend warmup: lowered ", nfiles, " base files")
    end
end
