
_args_and_call(args...; kwargs...) = (args[1:end-1], kwargs, args[end](args[1:end-1]...; kwargs...))
"""
    @inferred f(x)

Tests that the call expression `f(x)` returns a value of the same type
inferred by the compiler. It is useful to check for type stability.

`f(x)` can be any call expression.
Returns the result of `f(x)` if the types match,
and an `Error` `Result` if it finds different types.

```jldoctest
julia> using Test

julia> f(a,b,c) = b > 1 ? 1 : 1.0
f (generic function with 1 method)

julia> typeof(f(1,2,3))
Int64

julia> @code_warntype f(1,2,3)
Variables:
  a<optimized out>
  b::Int64
  c<optimized out>

Body:
  begin
      unless (Base.slt_int)(1, b::Int64)::Bool goto 3
      return 1
      3:
      return 1.0
  end::UNION{FLOAT64, INT64}

julia> @inferred f(1,2,3)
ERROR: return type Int64 does not match inferred return type Union{Float64, Int64}
[...]

julia> @inferred max(1,2)
2
```
"""
macro inferred(ex)
    if Meta.isexpr(ex, :ref)
        ex = Expr(:call, :getindex, ex.args...)
    end
    Meta.isexpr(ex, :call)|| error("@inferred requires a call expression")

    Base.remove_linenums!(quote
        let
            $(if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex.args)
                # Has keywords
                args = gensym()
                kwargs = gensym()
                quote
                    $(esc(args)), $(esc(kwargs)), result = $(esc(Expr(:call, _args_and_call, ex.args[2:end]..., ex.args[1])))
                    inftypes = $(Base.gen_call_with_extracted_types(__module__, Base.return_types, :($(ex.args[1])($(args)...; $(kwargs)...))))
                end
            else
                # No keywords
                quote
                    args = ($([esc(ex.args[i]) for i = 2:length(ex.args)]...),)
                    result = $(esc(ex.args[1]))(args...)
                    inftypes = Base.return_types($(esc(ex.args[1])), Base.typesof(args...))
                end
            end)
            @assert length(inftypes) == 1
            rettype = isa(result, Type) ? Type{result} : typeof(result)
            rettype == inftypes[1] || error("return type $rettype does not match inferred return type $(inftypes[1])")
            result
        end
    end)
end

"""
    detect_ambiguities(mod1, mod2...; imported=false, recursive=false, ambiguous_bottom=false)

Returns a vector of `(Method,Method)` pairs of ambiguous methods
defined in the specified modules.
Use `imported=true` if you wish to also test functions that were
imported into these modules from elsewhere.
Use `recursive=true` to test in all submodules.

`ambiguous_bottom` controls whether ambiguities triggered only by
`Union{}` type parameters are included; in most cases you probably
want to set this to `false`. See [`Base.isambiguous`](@ref).
"""
function detect_ambiguities(mods...;
                            imported::Bool = false,
                            recursive::Bool = false,
                            ambiguous_bottom::Bool = false)
    function sortdefs(m1, m2)
        ord12 = m1.file < m2.file
        if !ord12 && (m1.file == m2.file)
            ord12 = m1.line < m2.line
        end
        ord12 ? (m1, m2) : (m2, m1)
    end
    ambs = Set{Tuple{Method,Method}}()
    for mod in mods
        for n in names(mod, true, imported)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                println("Skipping ", mod, '.', n)  # typically stale exports
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if recursive && isa(f, Module) && f !== mod && module_parent(f) === mod && module_name(f) === n
                subambs = detect_ambiguities(f,
                    imported=imported, recursive=recursive, ambiguous_bottom=ambiguous_bottom)
                union!(ambs, subambs)
            elseif isa(f, DataType) && isdefined(f.name, :mt)
                mt = Base.MethodList(f.name.mt)
                for m in mt
                    if m.ambig !== nothing
                        for m2 in m.ambig
                            if Base.isambiguous(m, m2, ambiguous_bottom=ambiguous_bottom)
                                push!(ambs, sortdefs(m, m2))
                            end
                        end
                    end
                end
            end
        end
    end
    return collect(ambs)
end

"""
    detect_unbound_args(mod1, mod2...; imported=false, recursive=false)

Returns a vector of `Method`s which may have unbound type parameters.
Use `imported=true` if you wish to also test functions that were
imported into these modules from elsewhere.
Use `recursive=true` to test in all submodules.
"""
function detect_unbound_args(mods...;
                             imported::Bool = false,
                             recursive::Bool = false)
    ambs = Set{Method}()
    for mod in mods
        for n in names(mod, true, imported)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                println("Skipping ", mod, '.', n)  # typically stale exports
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if recursive && isa(f, Module) && module_parent(f) === mod && module_name(f) === n
                subambs = detect_unbound_args(f, imported=imported, recursive=recursive)
                union!(ambs, subambs)
            elseif isa(f, DataType) && isdefined(f.name, :mt)
                mt = Base.MethodList(f.name.mt)
                for m in mt
                    if has_unbound_vars(m.sig)
                        tuple_sig = Base.unwrap_unionall(m.sig)::DataType
                        if Base.isvatuple(tuple_sig)
                            params = tuple_sig.parameters[1:(end - 1)]
                            tuple_sig = Base.rewrap_unionall(Tuple{params...}, m.sig)
                            mf = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tuple_sig, typemax(UInt))
                            if mf != nothing && mf.func !== m && mf.func.sig <: tuple_sig
                                continue
                            end
                        end
                        push!(ambs, m)
                    end
                end
            end
        end
    end
    return collect(ambs)
end

# find if var will be constrained to have a definite value
# in any concrete leaftype subtype of typ
function constrains_param(var::TypeVar, @nospecialize(typ), covariant::Bool)
    typ === var && return true
    while typ isa UnionAll
        covariant && constrains_param(var, typ.var.ub, covariant) && return true
        # typ.var.lb doesn't constrain var
        typ = typ.body
    end
    if typ isa Union
        # for unions, verify that both options would constrain var
        ba = constrains_param(var, typ.a, covariant)
        bb = constrains_param(var, typ.b, covariant)
        (ba && bb) && return true
    elseif typ isa DataType
        # return true if any param constrains var
        fc = length(typ.parameters)
        if fc > 0
            if typ.name === Tuple.name
                # vararg tuple needs special handling
                for i in 1:(fc - 1)
                    p = typ.parameters[i]
                    constrains_param(var, p, covariant) && return true
                end
                lastp = typ.parameters[fc]
                vararg = Base.unwrap_unionall(lastp)
                if vararg isa DataType && vararg.name === Base._va_typename
                    N = vararg.parameters[2]
                    constrains_param(var, N, covariant) && return true
                    # T = vararg.parameters[1] doesn't constrain var
                else
                    constrains_param(var, lastp, covariant) && return true
                end
            else
                for i in 1:fc
                    p = typ.parameters[i]
                    constrains_param(var, p, false) && return true
                end
            end
        end
    end
    return false
end

function has_unbound_vars(@nospecialize sig)
    while sig isa UnionAll
        var = sig.var
        sig = sig.body
        if !constrains_param(var, sig, true)
            return true
        end
    end
    return false
end

