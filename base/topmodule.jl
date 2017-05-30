"""
    @topmodule

Place a call to `@Base.topmodule` as the _first_ statement in a module
to permit overriding a "top" function later in that module.
See `names(Base.TopModule)` for the set of names this will define.
"""
macro topmodule()
    @noinline topmodule() = ccall(:jl_set_istopmod, Void, (Bool,), false)
    return quote
        $topmodule()
        using Main.Base.TopModule
    end
end

# this is a record of all functions that may get used by lowering in Expr(:top)
# it must be kept in sync with julia-syntax.scm
baremodule TopModule
    import ..Base # keep name pollution down to an absolute minimum
    macro from(mod, exports)
        # reexport everything in `exports` from `mod`
        getindex = Base.getindex
        colon = Base.colon
        Base.Meta.isexpr(exports, :export) || error("expected export expression")
        # flatten a A.B.C expression into a list
        modpath = Base.Vector{Symbol}()
        while isa(mod, Expr)
            p = mod.args[2]
            (Base.Meta.isexpr(p, :quote) && isa(p.args[1], Symbol)) || error("expected quoted symbol")
            Base.unshift!(modpath, p.args[1])
            mod = mod.args[1]
        end
        isa(mod, Symbol) || error("expected symbol")
        Base.unshift!(modpath, mod)
        # make an import for each export
        imports = Any[ Expr(:import, modpath..., exports.args[i]) for i = 1:Base.length(exports.args) ]
        Base.push!(imports, exports)
        # return the result
        expr = Expr(:toplevel)
        expr.args = imports
        return expr
    end

    @from Base export
        endof, trailingsize, size, length, isempty,
        start, next, done, indexed_next,
        error, kwerr, as_kwargs,
        vector_any, append_any,
        vect, hcat, typed_hcat, typed_vcat, string,
        collect, product, Iterators#=.Filter=#, Generator, Flatten,
        setindex!, # note: setindex! / getindex are usually not special
        convert, ptr_arg_cconvert, cconvert, ptr_arg_unsafe_convert, unsafe_convert,
        dotview, broadcast, broadcast!, identity,
        literal_pow, Val,
        !, +, -, *, >>, !=
end
