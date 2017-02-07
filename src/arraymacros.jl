# Julia 0.6 macros to aid in vectorization: @view, @views, @__dot__ (@.),
# backported from Julia 0.6.

# prior to julia#20247, the replace_ref_end! macro had hygiene bugs
if VERSION < v"0.6.0-dev.2406"
    function trailingsize(A, n)
        s = 1
        for i=n:ndims(A)
            s *= size(A,i)
        end
        return s
    end
    replace_ref_end!(ex) = replace_ref_end_!(ex, nothing)[1]
    # replace_ref_end_!(ex,withex) returns (new ex, whether withex was used)
    function replace_ref_end_!(ex, withex)
        used_withex = false
        if isa(ex,Symbol) && ex == :end
            withex === nothing && error("Invalid use of end")
            return withex, true
        elseif isa(ex,Expr)
            if ex.head == :ref
                ex.args[1], used_withex = replace_ref_end_!(ex.args[1],withex)
                S = isa(ex.args[1],Symbol) ? ex.args[1]::Symbol : gensym(:S) # temp var to cache ex.args[1] if needed
                used_S = false # whether we actually need S
                # new :ref, so redefine withex
                nargs = length(ex.args)-1
                if nargs == 0
                    return ex, used_withex
                elseif nargs == 1
                    # replace with endof(S)
                    ex.args[2], used_S = replace_ref_end_!(ex.args[2],:($endof($S)))
                else
                    n = 1
                    J = endof(ex.args)
                    for j = 2:J-1
                        exj, used = replace_ref_end_!(ex.args[j],:($size($S,$n)))
                        used_S |= used
                        ex.args[j] = exj
                        if isa(exj,Expr) && exj.head == :...
                            # splatted object
                            exjs = exj.args[1]
                            n = :($n + length($exjs))
                        elseif isa(n, Expr)
                            # previous expression splatted
                            n = :($n + 1)
                        else
                            # an integer
                            n += 1
                        end
                    end
                    ex.args[J], used = replace_ref_end_!(ex.args[J],:($trailingsize($S,$n)))
                    used_S |= used
                end
                if used_S && S !== ex.args[1]
                    S0 = ex.args[1]
                    ex.args[1] = S
                    ex = Expr(:let, ex, :($S = $S0))
                end
            else
                # recursive search
                for i = eachindex(ex.args)
                    ex.args[i], used = replace_ref_end_!(ex.args[i],withex)
                    used_withex |= used
                end
            end
        end
        ex, used_withex
    end
end

# convert x[...] on lhs of .= to a view in broadcast! call
if VERSION < v"0.5.0-dev+5575" #17510
    dotview(args...) = getindex(args...)
    dotview(A::AbstractArray, args...) = view(A, args...)
    dotview{T<:AbstractArray}(A::AbstractArray{T}, args::Integer...) = getindex(A, args...)
    todotview(x) = x
    function todotview(ex::Expr)
        if ex.head == :ref
            ex = replace_ref_end!(ex)
            if Meta.isexpr(ex, :ref)
                ex = Expr(:call, dotview, ex.args...)
            else # ex replaced by let ...; foo[...]; end
                assert(Meta.isexpr(ex, :let) && Meta.isexpr(ex.args[1], :ref))
                ex.args[1] = Expr(:call, dotview, ex.args[1].args...)
            end
        end
    end
end

if !isdefined(Base, Symbol("@view"))
    macro view(ex)
        if Meta.isexpr(ex, :ref)
            ex = replace_ref_end!(ex)
            if Meta.isexpr(ex, :ref)
                ex = Expr(:call, view, ex.args...)
            else # ex replaced by let ...; foo[...]; end
                assert(Meta.isexpr(ex, :let) && Meta.isexpr(ex.args[1], :ref))
                ex.args[1] = Expr(:call, view, ex.args[1].args...)
            end
            Expr(:&&, true, esc(ex))
        else
            throw(ArgumentError("Invalid use of @view macro: argument must be a reference expression A[...]."))
        end
    end
    export @view
end

if !isdefined(Base, Symbol("@views"))
    maybeview(A, args...) = getindex(A, args...)
    maybeview(A::AbstractArray, args...) = view(A, args...)
    maybeview(A::AbstractArray, args::Number...) = getindex(A, args...)
    maybeview(A) = getindex(A)
    maybeview(A::AbstractArray) = getindex(A)

    _views(x) = x
    function _views(ex::Expr)
        if ex.head in (:(=), :(.=))
            # don't use view for ref on the lhs of an assignment,
            # but still use views for the args of the ref:
            lhs = ex.args[1]
            Expr(ex.head, Meta.isexpr(lhs, :ref) ?
                          Expr(:ref, map(_views, lhs.args)...) : _views(lhs),
                 _views(ex.args[2]))
        elseif VERSION < v"0.5.0-dev+5575" && isexpr(ex, :comparison) && ex.args[2] == :.= #17510
            # as above, but in Julia 0.4 a .= produces a comparison expression
            lhs_ = ex.args[1]
            lhs = Meta.isexpr(lhs_, :ref) ? Expr(:ref, map(_views, lhs_.args)...) : _views(lhs_)
            if length(ex.args) == 3
                Expr(:.=, lhs, _views(ex.args[3]))
            else
                Expr(:.=, lhs, _views(Expr(:comparison, ex.args[3:end]...)))
            end
        elseif ex.head == :ref
            Expr(:call, maybeview, map(_views, ex.args)...)
        else
            h = string(ex.head)
            # don't use view on the lhs of an op-assignment a[i...] += ...
            if last(h) == '=' && Meta.isexpr(ex.args[1], :ref)
                lhs = ex.args[1]

                # temp vars to avoid recomputing a and i,
                # which will be assigned in a let block:
                a = gensym(:a)
                i = [gensym(:i) for k = 1:length(lhs.args)-1]

                # for splatted indices like a[i, j...], we need to
                # splat the corresponding temp var.
                I = similar(i, Any)
                for k = 1:length(i)
                    if Meta.isexpr(lhs.args[k+1], :...)
                        I[k] = Expr(:..., i[k])
                        lhs.args[k+1] = lhs.args[k+1].args[1] # unsplat
                    else
                        I[k] = i[k]
                    end
                end

                Expr(:let,
                     Expr(first(h) == '.' ? :(.=) : :(=), :($a[$(I...)]),
                          Expr(:call, Symbol(h[1:end-1]),
                               :($maybeview($a, $(I...))),
                               map(_views, ex.args[2:end])...)),
                     :($a = $(_views(lhs.args[1]))),
                     [:($(i[k]) = $(_views(lhs.args[k+1]))) for k=1:length(i)]...)
            else
                Expr(ex.head, map(_views, ex.args)...)
            end
        end
    end

    macro views(x)
        esc(_views(replace_ref_end!(x)))
    end
    export @views
end

# we can't define @. because that doesn't parse in Julia < 0.6, but
# we can define @__dot__, which is what @. is sugar for:
if !isdefined(Base, Symbol("@__dot__"))
    dottable(x) = false # avoid dotting spliced objects (e.g. view calls inserted by @view)
    dottable(x::Symbol) = !Base.isoperator(x) || first(string(x)) != '.' || x == :.. # don't add dots to dot operators
    dottable(x::Expr) = x.head != :$
    undot(x) = x
    function undot(x::Expr)
        if x.head == :.=
            Expr(:(=), x.args...)
        elseif x.head == :block # occurs in for x=..., y=...
            Expr(:block, map(undot, x.args)...)
        else
            x
        end
    end
    __dot__(x) = x
    function __dot__(x::Expr)
        dotargs = map(__dot__, x.args)
        if x.head == :call && dottable(x.args[1])
            Expr(:., dotargs[1], Expr(:tuple, dotargs[2:end]...))
        elseif x.head == :$
            x.args[1]
        elseif x.head == :let # don't add dots to "let x=... assignments
            Expr(:let, dotargs[1], map(undot, dotargs[2:end])...)
        elseif x.head == :for # don't add dots to for x=... assignments
            Expr(:for, undot(dotargs[1]), dotargs[2])
        elseif (x.head == :(=) || x.head == :function || x.head == :macro) &&
               Meta.isexpr(x.args[1], :call) # function or macro definition
            Expr(x.head, x.args[1], dotargs[2])
        else
            head = string(x.head)
            if last(head) == '=' && first(head) != '.'
                Expr(Symbol('.',head), dotargs...)
            else
                Expr(x.head, dotargs...)
            end
        end
    end
    macro __dot__(x)
        esc(__dot__(x))
    end
    macro dotcompat(x)
        esc(_compat(__dot__(x)))
    end
    export @__dot__, @dotcompat
else
    # in 0.6, use the __dot__ function from Base.Broadcast
    macro dotcompat(x)
        esc(_compat(Base.Broadcast.__dot__(x)))
    end
    export @dotcompat
end
