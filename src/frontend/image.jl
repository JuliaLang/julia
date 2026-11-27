# This file is a part of Julia. License is MIT: https://julialang.org/license

# Input script for building the image of libjulia-frontend: an
# implementation of the frontend interface compiled from JuliaSyntax and
# JuliaLowering (see build_image.jl and the Makefile in this directory).

using JuliaFrontend
using JuliaFrontend.JuliaSyntax
using JuliaFrontend.JuliaLowering

# Exercise the frontend entry points so that their full call graphs are
# compiled into the image: the standalone library runs without a JIT
# (codegen stubs only), so anything not compiled here would run in the
# interpreter.
_ptr(@nospecialize(x)) = pointer_from_objref(x)

let
    code = "begin\n    f(x::Int; kw=1) = 2x + kw\n    \"doc\" module M\n    struct P{T}; x::T; end\n    g() = [a^2 for a in 1:10 if a > 0x02]\n    h() = \"s\" * 'c' * string(1.5, 1f0, 0xff, true)\n    end\nend\n"
    for rule in (:all, :statement, :atom)
        GC.@preserve code begin
            JuliaFrontend._c_frontend_parse(pointer(code), Csize_t(sizeof(code)),
                                            _ptr("warmup.jl"), Csize_t(1), Csize_t(0), _ptr(rule))
        end
    end
    bad = "f(x) = "
    GC.@preserve bad begin
        JuliaFrontend._c_frontend_parse(pointer(bad), Csize_t(sizeof(bad)),
                                        _ptr("warmup.jl"), Csize_t(1), Csize_t(0), _ptr(:statement))
    end
    fname = "warmup.jl"
    # exercise lowering into a bare module, as the cross-runtime path does
    # with its shadow modules
    shadow = Module(:WarmupShadow, false, false)
    lower(ex, mod) = GC.@preserve ex fname JuliaFrontend._c_frontend_lower(
        mod, _ptr(ex), Csize_t(typemax(Csize_t)), Cint(0), pointer(fname), Cint(1))
    begin
        lower(:(function lw(x); y -> y + x; end), Main)
        lower(:(@generated gw(x) = :(x + 1)), shadow)
        lower(
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
              end), shadow)
        let ex = :(@assert true)
            GC.@preserve ex JuliaFrontend._c_macroexpand(_ptr(ex), Main, Cint(1), Cint(0), Cint(1))
        end
        # exercise the lowering-error path (escape outside of a macro errors)
        lower(Expr(:escape, :x), shadow)
    end

    let salt = "w0#"
        GC.@preserve salt JuliaFrontend._c_set_binding_salt(Base.unsafe_convert(Cstring, salt))
        JuliaFrontend.JuliaLowering._module_binding_salt[] = ""
    end

    # the operator queries
    for name in ("+", ".+", "+₁", "+=", "&&", "->", "in", "x")
        GC.@preserve name JuliaFrontend._c_operator_info(Base.unsafe_convert(Cstring, name))
    end

    # Parse and lower the whole `base/` source corpus through the C-ABI
    # entry points. This is what makes a full julia bootstrap through the
    # standalone library possible: it compiles essentially every
    # JuliaSyntax/JuliaLowering code path that lowering base exercises into
    # the image (anything missed runs in the interpreter, where `ccall`
    # fails).
    # the source tree of the julia this frontend is built for
    juliahome = get(ENV, "JULIA_FRONTEND_SOURCE_ROOT", abspath(joinpath(@__DIR__, "..", "..")))
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
            GC.@preserve ex fname begin
                JuliaFrontend._c_frontend_lower(Main, _ptr(ex), Csize_t(typemax(Csize_t)),
                                                Cint(0), pointer(fname), Cint(1))
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
                    GC.@preserve code path begin
                        JuliaFrontend._c_frontend_parse(
                            pointer(code), Csize_t(sizeof(code)), _ptr(path),
                            Csize_t(1), Csize_t(0), _ptr(:all))
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
