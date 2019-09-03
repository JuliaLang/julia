# This file is a part of Julia. License is MIT: https://julialang.org/license

# script to generate tables of common symbols
# steps to rerun this:
#   1. empty the contents of common_symbols*.inc
#   2. build
#   3. cd src && ../julia --depwarn=no gen_sysimg_symtab.jl

import Base.Iterators: take, drop

function _eachmethod(f, m::Module, visited, vmt)
    push!(visited, m)
    for nm in names(m, all=true)
        if isdefined(m, nm)
            x = getfield(m, nm)
            if isa(x, Module) && !in(x, visited)
                _eachmethod(f, x, visited, vmt)
            elseif isa(x, Function)
                mt = typeof(x).name.mt
                if !in(mt, vmt)
                    push!(vmt, mt)
                    Base.visit(f, mt)
                end
            elseif isa(x, Type)
                x = Base.unwrap_unionall(x)
                if isa(x, DataType) && isdefined(x.name, :mt)
                    mt = x.name.mt
                    if !in(mt, vmt)
                        push!(vmt, mt)
                        Base.visit(f, mt)
                    end
                end
            end
        end
    end
end

function eachmethod(f, mods = Base.loaded_modules_array())
    visited = Set{Module}()
    vmt = Set{Any}()
    for mod in mods
        _eachmethod(f, mod, visited, vmt)
    end
end

function symcounts()
    rts = IdDict{Any,Int}()

    eachmethod() do m
        if isdefined(m, :roots)
            foreach(m.roots) do r
                isa(r, Symbol) && (rts[r] = get(rts, r, 0) + 1)
            end
        end
    end

    sort!(Any[x.first for x in rts], by = x->rts[x], rev=true)
end

syms = symcounts()
filter!(s -> let str = string(s)
        !isabspath(str) &&
        !startswith(str, "#") &&
        !in('\\', str)
        end,
        syms)

function outputline(io, name)
    println(io, "jl_symbol(\"", name, "\"),")
end

open(f->foreach(l->outputline(f,l), take(syms, 106)), "common_symbols1.inc", "w")
open(f->foreach(l->outputline(f,l), take(drop(syms, 106), 254)), "common_symbols2.inc", "w")
