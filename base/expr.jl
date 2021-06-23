# This file is a part of Julia. License is MIT: https://julialang.org/license

## symbols ##

"""
    gensym([tag])

Generates a symbol which will not conflict with other variable names.
"""
gensym() = ccall(:jl_gensym, Ref{Symbol}, ())

gensym(s::String) = ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Csize_t), s, sizeof(s))

gensym(ss::String...) = map(gensym, ss)
gensym(s::Symbol) = ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Csize_t), s, -1 % Csize_t)

"""
    @gensym

Generates a gensym symbol for a variable. For example, `@gensym x y` is transformed into
`x = gensym("x"); y = gensym("y")`.
"""
macro gensym(names...)
    blk = Expr(:block)
    for name in names
        push!(blk.args, :($(esc(name)) = gensym($(string(name)))))
    end
    push!(blk.args, :nothing)
    return blk
end

## expressions ##

isexpr(@nospecialize(ex), head::Symbol) = isa(ex, Expr) && ex.head === head
isexpr(@nospecialize(ex), head::Symbol, n::Int) = isa(ex, Expr) && ex.head === head && length(ex.args) == n

copy(e::Expr) = exprarray(e.head, copy_exprargs(e.args))

# copy parts of an AST that the compiler mutates
function copy_exprs(@nospecialize(x))
    if isa(x, Expr)
        return copy(x)
    elseif isa(x, PhiNode)
        values = x.values
        nvalues = length(values)
        new_values = Vector{Any}(undef, nvalues)
        @inbounds for i = 1:nvalues
            isassigned(values, i) || continue
            new_values[i] = copy_exprs(values[i])
        end
        return PhiNode(copy(x.edges), new_values)
    elseif isa(x, PhiCNode)
        values = x.values
        nvalues = length(values)
        new_values = Vector{Any}(undef, nvalues)
        @inbounds for i = 1:nvalues
            isassigned(values, i) || continue
            new_values[i] = copy_exprs(values[i])
        end
        return PhiCNode(new_values)
    end
    return x
end
copy_exprargs(x::Array{Any,1}) = Any[copy_exprs(@inbounds x[i]) for i in 1:length(x)]

@eval exprarray(head::Symbol, arg::Array{Any,1}) = $(Expr(:new, :Expr, :head, :arg))

# create copies of the CodeInfo definition, and any mutable fields
function copy(c::CodeInfo)
    cnew = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), c)
    cnew.code = copy_exprargs(cnew.code)
    cnew.slotnames = copy(cnew.slotnames)
    cnew.slotflags = copy(cnew.slotflags)
    cnew.codelocs  = copy(cnew.codelocs)
    cnew.linetable = copy(cnew.linetable::Union{Vector{Any},Vector{Core.LineInfoNode}})
    cnew.ssaflags  = copy(cnew.ssaflags)
    cnew.edges     = cnew.edges === nothing ? nothing : copy(cnew.edges::Vector)
    ssavaluetypes  = cnew.ssavaluetypes
    ssavaluetypes isa Vector{Any} && (cnew.ssavaluetypes = copy(ssavaluetypes))
    return cnew
end


==(x::Expr, y::Expr) = x.head === y.head && isequal(x.args, y.args)
==(x::QuoteNode, y::QuoteNode) = isequal(x.value, y.value)
==(stmt1::Core.PhiNode, stmt2::Core.PhiNode) = stmt1.edges == stmt2.edges && stmt1.values == stmt2.values

"""
    macroexpand(m::Module, x; recursive=true)

Take the expression `x` and return an equivalent expression with all macros removed (expanded)
for executing in module `m`.
The `recursive` keyword controls whether deeper levels of nested macros are also expanded.
This is demonstrated in the example below:
```julia-repl
julia> module M
           macro m1()
               42
           end
           macro m2()
               :(@m1())
           end
       end
M

julia> macroexpand(M, :(@m2()), recursive=true)
42

julia> macroexpand(M, :(@m2()), recursive=false)
:(#= REPL[16]:6 =# M.@m1)
```
"""
function macroexpand(m::Module, @nospecialize(x); recursive=true)
    if recursive
        ccall(:jl_macroexpand, Any, (Any, Any), x, m)
    else
        ccall(:jl_macroexpand1, Any, (Any, Any), x, m)
    end
end

"""
    @macroexpand

Return equivalent expression with all macros removed (expanded).

There are differences between `@macroexpand` and [`macroexpand`](@ref).

* While [`macroexpand`](@ref) takes a keyword argument `recursive`, `@macroexpand`
  is always recursive. For a non recursive macro version, see [`@macroexpand1`](@ref).

* While [`macroexpand`](@ref) has an explicit `module` argument, `@macroexpand` always
  expands with respect to the module in which it is called.

This is best seen in the following example:
```julia-repl
julia> module M
           macro m()
               1
           end
           function f()
               (@macroexpand(@m),
                macroexpand(M, :(@m)),
                macroexpand(Main, :(@m))
               )
           end
       end
M

julia> macro m()
           2
       end
@m (macro with 1 method)

julia> M.f()
(1, 1, 2)
```
With `@macroexpand` the expression expands where `@macroexpand` appears in the code (module `M` in the example).
With `macroexpand` the expression expands in the module given as the first argument.
"""
macro macroexpand(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=true))
end


"""
    @macroexpand1

Non recursive version of [`@macroexpand`](@ref).
"""
macro macroexpand1(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=false))
end

## misc syntax ##

"""
    Core.eval(m::Module, expr)

Evaluate an expression in the given module and return the result.
"""
Core.eval

"""
    @inline

Give a hint to the compiler that this function is worth inlining.

Small functions typically do not need the `@inline` annotation,
as the compiler does it automatically. By using `@inline` on bigger functions,
an extra nudge can be given to the compiler to inline it.

`@inline` can be applied immediately before the definition or in its function body.

```julia
# annotate long-form definition
@inline function longdef(x)
    ...
end

# annotate short-form definition
@inline shortdef(x) = ...

# annotate anonymous function that a `do` block creates
f() do
    @inline
    ...
end
```

!!! compat "Julia 1.8"
    The usage within a function body requires at least Julia 1.8.
"""
macro inline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :inline) : ex)
end
macro inline() Expr(:meta, :inline) end

"""
    @noinline

Give a hint to the compiler that it should not inline a function.

Small functions are typically inlined automatically.
By using `@noinline` on small functions, auto-inlining can be
prevented.

`@noinline` can be applied immediately before the definition or in its function body.

```julia
# annotate long-form definition
@noinline function longdef(x)
    ...
end

# annotate short-form definition
@noinline shortdef(x) = ...

# annotate anonymous function that a `do` block creates
f() do
    @noinline
    ...
end
```

!!! compat "Julia 1.8"
    The usage within a function body requires at least Julia 1.8.

!!! note
    If the function is trivial (for example returning a constant) it might get inlined anyway.
"""
macro noinline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :noinline) : ex)
end
macro noinline() Expr(:meta, :noinline) end

"""
    @pure ex
    @pure(ex)

`@pure` gives the compiler a hint for the definition of a pure function,
helping for type inference.

This macro is intended for internal compiler use and may be subject to changes.
"""
macro pure(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :pure) : ex)
end

"""
    @aggressive_constprop ex
    @aggressive_constprop(ex)

`@aggressive_constprop` requests more aggressive interprocedural constant
propagation for the annotated function. For a method where the return type
depends on the value of the arguments, this can yield improved inference results
at the cost of additional compile time.
"""
macro aggressive_constprop(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :aggressive_constprop) : ex)
end

"""
    @propagate_inbounds

Tells the compiler to inline a function while retaining the caller's inbounds context.
"""
macro propagate_inbounds(ex)
    if isa(ex, Expr)
        pushmeta!(ex, :inline)
        pushmeta!(ex, :propagate_inbounds)
    end
    esc(ex)
end

"""
    @polly

Tells the compiler to apply the polyhedral optimizer Polly to a function.
"""
macro polly(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :polly) : ex)
end

## some macro utilities ##

function pushmeta!(ex::Expr, sym::Symbol, args::Any...)
    if isempty(args)
        tag = sym
    else
        tag = Expr(sym, args...)::Expr
    end

    inner = ex
    while inner.head === :macrocall
        inner = inner.args[end]::Expr
    end

    idx, exargs = findmeta(inner)
    if idx != 0
        push!(exargs[idx].args, tag)
    else
        body = inner.args[2]::Expr
        pushfirst!(body.args, Expr(:meta, tag))
    end
    ex
end

popmeta!(body, sym) = _getmeta(body, sym, true)
peekmeta(body, sym) = _getmeta(body, sym, false)

function _getmeta(body::Expr, sym::Symbol, delete::Bool)
    body.head === :block || return false, []
    _getmeta(body.args, sym, delete)
end
_getmeta(arg, sym, delete::Bool) = (false, [])
function _getmeta(body::Array{Any,1}, sym::Symbol, delete::Bool)
    idx, blockargs = findmeta_block(body, args -> findmetaarg(args,sym)!=0)
    if idx == 0
        return false, []
    end
    metaargs = blockargs[idx].args
    i = findmetaarg(blockargs[idx].args, sym)
    if i == 0
        return false, []
    end
    ret = isa(metaargs[i], Expr) ? (metaargs[i]::Expr).args : []
    if delete
        deleteat!(metaargs, i)
        isempty(metaargs) && deleteat!(blockargs, idx)
    end
    true, ret
end

# Find index of `sym` in a meta expression argument list, or 0.
function findmetaarg(metaargs, sym)
    for i = 1:length(metaargs)
        arg = metaargs[i]
        if (isa(arg, Symbol) && (arg::Symbol)    == sym) ||
           (isa(arg, Expr)   && (arg::Expr).head == sym)
            return i
        end
    end
    return 0
end

function is_short_function_def(ex)
    ex.head === :(=) || return false
    while length(ex.args) >= 1 && isa(ex.args[1], Expr)
        (ex.args[1].head === :call) && return true
        (ex.args[1].head === :where || ex.args[1].head === :(::)) || return false
        ex = ex.args[1]
    end
    return false
end

function findmeta(ex::Expr)
    if ex.head === :function || is_short_function_def(ex) || ex.head === :->
        body = ex.args[2]::Expr
        body.head === :block || error(body, " is not a block expression")
        return findmeta_block(ex.args)
    end
    error(ex, " is not a function expression")
end

findmeta(ex::Array{Any,1}) = findmeta_block(ex)

function findmeta_block(exargs, argsmatch=args->true)
    for i = 1:length(exargs)
        a = exargs[i]
        if isa(a, Expr)
            if a.head === :meta && argsmatch(a.args)
                return i, exargs
            elseif a.head === :block
                idx, exa = findmeta_block(a.args, argsmatch)
                if idx != 0
                    return idx, exa
                end
            end
        end
    end
    return 0, []
end

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    if ex.head === :block || ex.head === :quote
        # remove line number expressions from metadata (not argument literal or inert) position
        filter!(ex.args) do x
            isa(x, Expr) && x.head === :line && return false
            isa(x, LineNumberNode) && return false
            return true
        end
    end
    for subex in ex.args
        subex isa Expr && remove_linenums!(subex)
    end
    return ex
end
function remove_linenums!(src::CodeInfo)
    src.codelocs .= 0
    length(src.linetable) > 1 && resize!(src.linetable, 1)
    return src
end

macro generated()
    return Expr(:generated)
end

"""
    @generated f
    @generated(f)
`@generated` is used to annotate a function which will be generated.
In the body of the generated function, only types of arguments can be read
(not the values). The function returns a quoted expression evaluated when the
function is called. The `@generated` macro should not be used on functions mutating
the global scope or depending on mutable elements.

See [Metaprogramming](@ref) for further details.

## Example:
```jldoctest
julia> @generated function bar(x)
           if x <: Integer
               return :(x ^ 2)
           else
               return :(x)
           end
       end
bar (generic function with 1 method)

julia> bar(4)
16

julia> bar("baz")
"baz"
```
"""
macro generated(f)
    if isa(f, Expr) && (f.head === :function || is_short_function_def(f))
        body = f.args[2]
        lno = body.args[1]
        return Expr(:escape,
                    Expr(f.head, f.args[1],
                         Expr(:block,
                              lno,
                              Expr(:if, Expr(:generated),
                                   body,
                                   Expr(:block,
                                        Expr(:meta, :generated_only),
                                        Expr(:return, nothing))))))
    else
        error("invalid syntax; @generated must be used with a function definition")
    end
end


"""
    @atomic var
    @atomic order ex

Mark `var` or `ex` as being performed atomically, if `ex` is a supported expression.

    @atomic a.b.x = new
    @atomic a.b.x += addend
    @atomic :acquire_release a.b.x = new
    @atomic :acquire_release a.b.x += addend

Perform the store operation expressed on the right atomically and return the
new value.

With `=`, this operation translates to a `setproperty!(a.b, :x, new)` call.
With any operator also, this operation translates to a `modifyproperty!(a.b,
:x, +, addend)[2]` call.

    @atomic a.b.x max arg2
    @atomic a.b.x + arg2
    @atomic max(a.b.x, arg2)
    @atomic :acquire_release max(a.b.x, arg2)
    @atomic :acquire_release a.b.x + arg2
    @atomic :acquire_release a.b.x max arg2

Perform the binary operation expressed on the right atomically. Store the
result into the field in the first argument and return the values `(old, new)`.

This operation translates to a `modifyproperty!(a.b, :x, func, arg2)` call.


See [atomics](#man-atomics) in the manual for more details.

```jldoctest
julia> mutable struct Atomic{T}; @atomic x::T; end

julia> a = Atomic(1)
Atomic{Int64}(1)

julia> @atomic a.x # fetch field x of a, with sequential consistency
1

julia> @atomic :sequentially_consistent a.x = 2 # set field x of a, with sequential consistency
2

julia> @atomic a.x += 1 # increment field x of a, with sequential consistency
3

julia> @atomic a.x + 1 # increment field x of a, with sequential consistency
(3, 4)

julia> @atomic a.x # fetch field x of a, with sequential consistency
4

julia> @atomic max(a.x, 10) # change field x of a to the max value, with sequential consistency
(4, 10)

julia> @atomic a.x max 5 # again change field x of a to the max value, with sequential consistency
(10, 10)
```
"""
macro atomic(ex)
    if !isa(ex, Symbol) && !is_expr(ex, :(::))
        return make_atomic(QuoteNode(:sequentially_consistent), ex)
    end
    return esc(Expr(:atomic, ex))
end
macro atomic(order, ex)
    order isa QuoteNode || (order = esc(order))
    return make_atomic(order, ex)
end
macro atomic(a1, op, a2)
    return make_atomic(QuoteNode(:sequentially_consistent), a1, op, a2)
end
macro atomic(order, a1, op, a2)
    order isa QuoteNode || (order = esc(order))
    return make_atomic(order, a1, op, a2)
end
function make_atomic(order, ex)
    @nospecialize
    if ex isa Expr
        if isexpr(ex, :., 2)
            l, r = esc(ex.args[1]), esc(ex.args[2])
            return :(getproperty($l, $r, $order))
        elseif isexpr(ex, :call, 3)
            return make_atomic(order, ex.args[2], ex.args[1], ex.args[3])
        elseif ex.head === :(=)
            l, r = ex.args[1], esc(ex.args[2])
            if is_expr(l, :., 2)
                ll, lr = esc(l.args[1]), esc(l.args[2])
                return :(setproperty!($ll, $lr, $r, $order))
            end
        end
        if length(ex.args) == 2
            if ex.head === :(+=)
                op = :+
            elseif ex.head === :(-=)
                op = :-
            elseif @isdefined string
                shead = string(ex.head)
                if endswith(shead, '=')
                    op = Symbol(shead[1:prevind(shead, end)])
                end
            end
            if @isdefined(op)
                return Expr(:ref, make_atomic(order, ex.args[1], op, ex.args[2]), 2)
            end
        end
    end
    error("could not parse @atomic expression $ex")
end
function make_atomic(order, a1, op, a2)
    @nospecialize
    is_expr(a1, :., 2) || error("@atomic modify expression missing field access")
    a1l, a1r, op, a2 = esc(a1.args[1]), esc(a1.args[2]), esc(op), esc(a2)
    return :(modifyproperty!($a1l, $a1r, $op, $a2, $order))
end


"""
    @atomicswap a.b.x = new
    @atomicswap :sequentially_consistent a.b.x = new

Stores `new` into `a.b.x` and returns the old value of `a.b.x`.

This operation translates to a `swapproperty!(a.b, :x, new)` call.

See [atomics](#man-atomics) in the manual for more details.

```jldoctest
julia> mutable struct Atomic{T}; @atomic x::T; end

julia> a = Atomic(1)
Atomic{Int64}(1)

julia> @atomicswap a.x = 2+2 # replace field x of a with 4, with sequential consistency
1

julia> @atomic a.x # fetch field x of a, with sequential consistency
4
```
"""
macro atomicswap(order, ex)
    order isa QuoteNode || (order = esc(order))
    return make_atomicswap(order, ex)
end
macro atomicswap(ex)
    return make_atomicswap(QuoteNode(:sequentially_consistent), ex)
end
function make_atomicswap(order, ex)
    @nospecialize
    is_expr(ex, :(=), 2) || error("@atomicswap expression missing assignment")
    l, val = ex.args[1], esc(ex.args[2])
    is_expr(l, :., 2) || error("@atomicswap expression missing field access")
    ll, lr = esc(l.args[1]), esc(l.args[2])
    return :(swapproperty!($ll, $lr, $val, $order))
end


"""
    @atomicreplace a.b.x expected => desired
    @atomicreplace :sequentially_consistent a.b.x expected => desired
    @atomicreplace :sequentially_consistent :monotonic a.b.x expected => desired

Perform the conditional replacement expressed by the pair atomically, returning
the values `(old, success::Bool)`. Where `success` indicates whether the
replacement was completed.

This operation translates to a `replaceproperty!(a.b, :x, expected, desired)` call.

See [atomics](#man-atomics) in the manual for more details.

```jldoctest
julia> mutable struct Atomic{T}; @atomic x::T; end

julia> a = Atomic(1)
Atomic{Int64}(1)

julia> @atomicreplace a.x 1 => 2 # replace field x of a with 2 if it was 1, with sequential consistency
(1, true)

julia> @atomic a.x # fetch field x of a, with sequential consistency
2

julia> @atomicreplace a.x 1 => 2 # replace field x of a with 2 if it was 1, with sequential consistency
(2, false)

julia> xchg = 2 => 0; # replace field x of a with 0 if it was 1, with sequential consistency

julia> @atomicreplace a.x xchg
(2, true)

julia> @atomic a.x # fetch field x of a, with sequential consistency
0
```
"""
macro atomicreplace(success_order, fail_order, ex, old_new)
    fail_order isa QuoteNode || (fail_order = esc(fail_order))
    success_order isa QuoteNode || (success_order = esc(success_order))
    return make_atomicreplace(success_order, fail_order, ex, old_new)
end
macro atomicreplace(order, ex, old_new)
    order isa QuoteNode || (order = esc(order))
    return make_atomicreplace(order, order, ex, old_new)
end
macro atomicreplace(ex, old_new)
    return make_atomicreplace(QuoteNode(:sequentially_consistent), QuoteNode(:sequentially_consistent), ex, old_new)
end
function make_atomicreplace(success_order, fail_order, ex, old_new)
    @nospecialize
    is_expr(ex, :., 2) || error("@atomicreplace expression missing field access")
    ll, lr = esc(ex.args[1]), esc(ex.args[2])
    if is_expr(old_new, :call, 3) && old_new.args[1] === :(=>)
        exp, rep = esc(old_new.args[2]), esc(old_new.args[3])
        return :(replaceproperty!($ll, $lr, $exp, $rep, $success_order, $fail_order))
    else
        old_new = esc(old_new)
        return :(replaceproperty!($ll, $lr, $old_new::Pair..., $success_order, $fail_order))
    end
end
