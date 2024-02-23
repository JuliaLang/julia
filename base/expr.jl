# This file is a part of Julia. License is MIT: https://julialang.org/license

isexpr(@nospecialize(ex), heads) = isa(ex, Expr) && in(ex.head, heads)
isexpr(@nospecialize(ex), heads, n::Int) = isa(ex, Expr) && in(ex.head, heads) && length(ex.args) == n
const is_expr = isexpr

## symbols ##

"""
    gensym([tag])

Generates a symbol which will not conflict with other variable names (in the same module).
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

## line numbers ##
convert(::Type{LineNumberNode}, lin::Core.LineInfoNode) = LineNumberNode(Int(lin.line), lin.file)

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
    if cnew.slottypes !== nothing
        cnew.slottypes = copy(cnew.slottypes::Vector{Any})
    end
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
    @macroexpand [mod,] ex

Return equivalent expression with all macros removed (expanded).
If two arguments are provided, the first is the module to evaluate in.

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

!!! compat "Julia 1.11"
    The two-argument form requires at least Julia 1.11.
"""
macro macroexpand(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=true))
end
macro macroexpand(mod, code)
    return :(macroexpand($(esc(mod)), $(QuoteNode(code)), recursive=true))
end

"""
    @macroexpand1 [mod,] ex

Non recursive version of [`@macroexpand`](@ref).
"""
macro macroexpand1(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=false))
end
macro macroexpand1(mod, code)
    return :(macroexpand($(esc(mod)), $(QuoteNode(code)), recursive=false))
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

`@inline` can be applied immediately before a function definition or within a function body.

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

---
    @inline block

Give a hint to the compiler that calls within `block` are worth inlining.

```julia
# The compiler will try to inline `f`
@inline f(...)

# The compiler will try to inline `f`, `g` and `+`
@inline f(...) + g(...)
```

!!! note
    A callsite annotation always has the precedence over the annotation applied to the
    definition of the called function:
    ```julia
    @noinline function explicit_noinline(args...)
        # body
    end

    let
        @inline explicit_noinline(args...) # will be inlined
    end
    ```

!!! note
    When there are nested callsite annotations, the innermost annotation has the precedence:
    ```julia
    @noinline let a0, b0 = ...
        a = @inline f(a0)  # the compiler will try to inline this call
        b = f(b0)          # the compiler will NOT try to inline this call
        return a, b
    end
    ```

!!! warning
    Although a callsite annotation will try to force inlining in regardless of the cost model,
    there are still chances it can't succeed in it. Especially, recursive calls can not be
    inlined even if they are annotated as `@inline`d.

!!! compat "Julia 1.8"
    The callsite annotation requires at least Julia 1.8.
"""
macro inline(x)
    return annotate_meta_def_or_block(x, :inline)
end

"""
    @noinline

Give a hint to the compiler that it should not inline a function.

Small functions are typically inlined automatically.
By using `@noinline` on small functions, auto-inlining can be
prevented.

`@noinline` can be applied immediately before a function definition or within a function body.

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

---
    @noinline block

Give a hint to the compiler that it should not inline the calls within `block`.

```julia
# The compiler will try to not inline `f`
@noinline f(...)

# The compiler will try to not inline `f`, `g` and `+`
@noinline f(...) + g(...)
```

!!! note
    A callsite annotation always has the precedence over the annotation applied to the
    definition of the called function:
    ```julia
    @inline function explicit_inline(args...)
        # body
    end

    let
        @noinline explicit_inline(args...) # will not be inlined
    end
    ```

!!! note
    When there are nested callsite annotations, the innermost annotation has the precedence:
    ```julia
    @inline let a0, b0 = ...
        a = @noinline f(a0)  # the compiler will NOT try to inline this call
        b = f(b0)            # the compiler will try to inline this call
        return a, b
    end
    ```

!!! compat "Julia 1.8"
    The callsite annotation requires at least Julia 1.8.

---
!!! note
    If the function is trivial (for example returning a constant) it might get inlined anyway.
"""
macro noinline(x)
    return annotate_meta_def_or_block(x, :noinline)
end

"""
    Base.@constprop setting [ex]

Control the mode of interprocedural constant propagation for the annotated function.

Two `setting`s are supported:

- `Base.@constprop :aggressive [ex]`: apply constant propagation aggressively.
  For a method where the return type depends on the value of the arguments,
  this can yield improved inference results at the cost of additional compile time.
- `Base.@constprop :none [ex]`: disable constant propagation. This can reduce compile
  times for functions that Julia might otherwise deem worthy of constant-propagation.
  Common cases are for functions with `Bool`- or `Symbol`-valued arguments or keyword arguments.

`Base.@constprop` can be applied immediately before a function definition or within a function body.

```julia
# annotate long-form definition
Base.@constprop :aggressive function longdef(x)
    ...
end

# annotate short-form definition
Base.@constprop :aggressive shortdef(x) = ...

# annotate anonymous function that a `do` block creates
f() do
    Base.@constprop :aggressive
    ...
end
```

!!! compat "Julia 1.10"
    The usage within a function body requires at least Julia 1.10.
"""
macro constprop(setting, ex)
    sym = constprop_setting(setting)
    isa(ex, Expr) && return esc(pushmeta!(ex, sym))
    throw(ArgumentError(LazyString("Bad expression `", ex, "` in `@constprop settings ex`")))
end
macro constprop(setting)
    sym = constprop_setting(setting)
    return Expr(:meta, sym)
end

function constprop_setting(@nospecialize setting)
    isa(setting, QuoteNode) && (setting = setting.value)
    if setting === :aggressive
        return :aggressive_constprop
    elseif setting === :none
        return :no_constprop
    end
    throw(ArgumentError(LazyString("@constprop "), setting, "not supported"))
end

"""
    Base.@assume_effects setting... [ex]

Override the compiler's effect modeling.
This macro can be used in several contexts:
1. Immediately before a method definition, to override the entire effect modeling of the applied method.
2. Within a function body without any arguments, to override the entire effect modeling of the enclosing method.
3. Applied to a code block, to override the local effect modeling of the applied code block.

# Examples
```jldoctest
julia> Base.@assume_effects :terminates_locally function fact(x)
           # usage 1:
           # this :terminates_locally allows `fact` to be constant-folded
           res = 1
           0 ≤ x < 20 || error("bad fact")
           while x > 1
               res *= x
               x -= 1
           end
           return res
       end
fact (generic function with 1 method)

julia> code_typed() do
           fact(12)
       end |> only
CodeInfo(
1 ─     return 479001600
) => Int64

julia> code_typed() do
           map((2,3,4)) do x
               # usage 2:
               # this :terminates_locally allows this anonymous function to be constant-folded
               Base.@assume_effects :terminates_locally
               res = 1
               0 ≤ x < 20 || error("bad fact")
               while x > 1
                   res *= x
                   x -= 1
               end
               return res
           end
       end |> only
CodeInfo(
1 ─     return (2, 6, 24)
) => Tuple{Int64, Int64, Int64}

julia> code_typed() do
           map((2,3,4)) do x
               res = 1
               0 ≤ x < 20 || error("bad fact")
               # usage 3:
               # with this :terminates_locally annotation the compiler skips tainting
               # `:terminates` effect within this `while` block, allowing the parent
               # anonymous function to be constant-folded
               Base.@assume_effects :terminates_locally while x > 1
                   res *= x
                   x -= 1
               end
               return res
           end
       end |> only
CodeInfo(
1 ─     return (2, 6, 24)
) => Tuple{Int64, Int64, Int64}
```

!!! compat "Julia 1.8"
    Using `Base.@assume_effects` requires Julia version 1.8.

!!! compat "Julia 1.10"
    The usage within a function body requires at least Julia 1.10.

!!! compact "Julia 1.11"
    The code block annotation requires at least Julia 1.11.

!!! warning
    Improper use of this macro causes undefined behavior (including crashes,
    incorrect answers, or other hard to track bugs). Use with care and only as a
    last resort if absolutely required. Even in such a case, you SHOULD take all
    possible steps to minimize the strength of the effect assertion (e.g.,
    do not use `:total` if `:nothrow` would have been sufficient).

In general, each `setting` value makes an assertion about the behavior of the
function, without requiring the compiler to prove that this behavior is indeed
true. These assertions are made for all world ages. It is thus advisable to limit
the use of generic functions that may later be extended to invalidate the
assumption (which would cause undefined behavior).

The following `setting`s are supported.
- `:consistent`
- `:effect_free`
- `:nothrow`
- `:terminates_globally`
- `:terminates_locally`
- `:notaskstate`
- `:inaccessiblememonly`
- `:noub`
- `:noub_if_noinbounds`
- `:foldable`
- `:removable`
- `:total`

# Extended help

---
## `:consistent`

The `:consistent` setting asserts that for egal (`===`) inputs:
- The manner of termination (return value, exception, non-termination) will always be the same.
- If the method returns, the results will always be egal.

!!! note
    This in particular implies that the method must not return a freshly allocated
    mutable object. Multiple allocations of mutable objects (even with identical
    contents) are not egal.

!!! note
    The `:consistent`-cy assertion is made world-age wise. More formally, write
    ``fᵢ`` for the evaluation of ``f`` in world-age ``i``, then we require:
    ```math
    ∀ i, x, y: x ≡ y → fᵢ(x) ≡ fᵢ(y)
    ```
    However, for two world ages ``i``, ``j`` s.t. ``i ≠ j``, we may have ``fᵢ(x) ≢ fⱼ(y)``.

    A further implication is that `:consistent` functions may not make their
    return value dependent on the state of the heap or any other global state
    that is not constant for a given world age.

!!! note
    The `:consistent`-cy includes all legal rewrites performed by the optimizer.
    For example, floating-point fastmath operations are not considered `:consistent`,
    because the optimizer may rewrite them causing the output to not be `:consistent`,
    even for the same world age (e.g. because one ran in the interpreter, while
    the other was optimized).

!!! note
    If `:consistent` functions terminate by throwing an exception, that exception
    itself is not required to meet the egality requirement specified above.

---
## `:effect_free`

The `:effect_free` setting asserts that the method is free of externally semantically
visible side effects. The following is an incomplete list of externally semantically
visible side effects:
- Changing the value of a global variable.
- Mutating the heap (e.g. an array or mutable value), except as noted below
- Changing the method table (e.g. through calls to eval)
- File/Network/etc. I/O
- Task switching

However, the following are explicitly not semantically visible, even if they
may be observable:
- Memory allocations (both mutable and immutable)
- Elapsed time
- Garbage collection
- Heap mutations of objects whose lifetime does not exceed the method (i.e.
  were allocated in the method and do not escape).
- The returned value (which is externally visible, but not a side effect)

The rule of thumb here is that an externally visible side effect is anything
that would affect the execution of the remainder of the program if the function
were not executed.

!!! note
    The `:effect_free` assertion is made both for the method itself and any code
    that is executed by the method. Keep in mind that the assertion must be
    valid for all world ages and limit use of this assertion accordingly.

---
## `:nothrow`

The `:nothrow` settings asserts that this method does not throw an exception
(i.e. will either always return a value or never return).

!!! note
    It is permissible for `:nothrow` annotated methods to make use of exception
    handling internally as long as the exception is not rethrown out of the
    method itself.

!!! note
    If the execution of a method may raise `MethodError`s and similar exceptions, then
    the method is not considered as `:nothrow`.
    However, note that environment-dependent errors like `StackOverflowError` or `InterruptException`
    are not modeled by this effect and thus a method that may result in `StackOverflowError`
    does not necessarily need to be `!:nothrow` (although it should usually be `!:terminates` too).

---
## `:terminates_globally`

The `:terminates_globally` settings asserts that this method will eventually terminate
(either normally or abnormally), i.e. does not loop indefinitely.

!!! note
    This `:terminates_globally` assertion covers any other methods called by the annotated method.

!!! note
    The compiler will consider this a strong indication that the method will
    terminate relatively *quickly* and may (if otherwise legal) call this
    method at compile time. I.e. it is a bad idea to annotate this setting
    on a method that *technically*, but not *practically*, terminates.

---
## `:terminates_locally`

The `:terminates_locally` setting is like `:terminates_globally`, except that it only
applies to syntactic control flow *within* the annotated method. It is thus
a much weaker (and thus safer) assertion that allows for the possibility of
non-termination if the method calls some other method that does not terminate.

!!! note
    `:terminates_globally` implies `:terminates_locally`.

---
## `:notaskstate`

The `:notaskstate` setting asserts that the method does not use or modify the
local task state (task local storage, RNG state, etc.) and may thus be safely
moved between tasks without observable results.

!!! note
    The implementation of exception handling makes use of state stored in the
    task object. However, this state is currently not considered to be within
    the scope of `:notaskstate` and is tracked separately using the `:nothrow`
    effect.

!!! note
    The `:notaskstate` assertion concerns the state of the *currently running task*.
    If a reference to a `Task` object is obtained by some other means that
    does not consider which task is *currently* running, the `:notaskstate`
    effect need not be tainted. This is true, even if said task object happens
    to be `===` to the currently running task.

!!! note
    Access to task state usually also results in the tainting of other effects,
    such as `:effect_free` (if task state is modified) or `:consistent` (if
    task state is used in the computation of the result). In particular,
    code that is not `:notaskstate`, but is `:effect_free` and `:consistent`
    may still be dead-code-eliminated and thus promoted to `:total`.

---
## `:inaccessiblememonly`

The `:inaccessiblememonly` setting asserts that the method does not access or modify
externally accessible mutable memory. This means the method can access or modify mutable
memory for newly allocated objects that is not accessible by other methods or top-level
execution before return from the method, but it can not access or modify any mutable
global state or mutable memory pointed to by its arguments.

!!! note
    Below is an incomplete list of examples that invalidate this assumption:
    - a global reference or `getglobal` call to access a mutable global variable
    - a global assignment or `setglobal!` call to perform assignment to a non-constant global variable
    - `setfield!` call that changes a field of a global mutable variable

!!! note
    This `:inaccessiblememonly` assertion covers any other methods called by the annotated method.

---
## `:noub`

The `:noub` setting asserts that the method will not execute any undefined behavior
(for any input). Note that undefined behavior may technically cause the method to violate
any other effect assertions (such as `:consistent` or `:effect_free`) as well, but we do
not model this, and they assume the absence of undefined behavior.

---
## `:foldable`

This setting is a convenient shortcut for the set of effects that the compiler
requires to be guaranteed to constant fold a call at compile time. It is
currently equivalent to the following `setting`s:
- `:consistent`
- `:effect_free`
- `:terminates_globally`
- `:noub`

!!! note
    This list in particular does not include `:nothrow`. The compiler will still
    attempt constant propagation and note any thrown error at compile time. Note
    however, that by the `:consistent`-cy requirements, any such annotated call
    must consistently throw given the same argument values.

!!! note
    An explicit `@inbounds` annotation inside the function will also disable
    constant folding and not be overridden by `:foldable`.

---
## `:removable`

This setting is a convenient shortcut for the set of effects that the compiler
requires to be guaranteed to delete a call whose result is unused at compile time.
It is currently equivalent to the following `setting`s:
- `:effect_free`
- `:nothrow`
- `:terminates_globally`

---
## `:total`

This `setting` is the maximum possible set of effects. It currently implies
the following other `setting`s:
- `:consistent`
- `:effect_free`
- `:nothrow`
- `:terminates_globally`
- `:notaskstate`
- `:inaccessiblememonly`
- `:noub`

!!! warning
    `:total` is a very strong assertion and will likely gain additional semantics
    in future versions of Julia (e.g. if additional effects are added and included
    in the definition of `:total`). As a result, it should be used with care.
    Whenever possible, prefer to use the minimum possible set of specific effect
    assertions required for a particular application. In cases where a large
    number of effect overrides apply to a set of functions, a custom macro is
    recommended over the use of `:total`.

---
## Negated effects

Effect names may be prefixed by `!` to indicate that the effect should be removed
from an earlier meta effect. For example, `:total !:nothrow` indicates that while
the call is generally total, it may however throw.
"""
macro assume_effects(args...)
    lastex = args[end]
    override = compute_assumed_settings(args[begin:end-1])
    if is_function_def(unwrap_macrocalls(lastex))
        return esc(pushmeta!(lastex, form_purity_expr(override)))
    elseif isexpr(lastex, :macrocall) && lastex.args[1] === Symbol("@ccall")
        lastex.args[1] = GlobalRef(Base, Symbol("@ccall_effects"))
        insert!(lastex.args, 3, Core.Compiler.encode_effects_override(override))
        return esc(lastex)
    end
    override′ = compute_assumed_setting(override, lastex)
    if override′ !== nothing
        # anonymous function case
        return Expr(:meta, form_purity_expr(override′))
    else
        # call site annotation case
        return Expr(:block,
                    form_purity_expr(override),
                    Expr(:local, Expr(:(=), :val, esc(lastex))),
                    Expr(:purity), # region end token
                    :val)
    end
end

function compute_assumed_settings(settings)
    override = EffectsOverride()
    for setting in settings
        override = compute_assumed_setting(override, setting)
        override === nothing &&
            throw(ArgumentError("@assume_effects $setting not supported"))
    end
    return override
end

using Core.Compiler: EffectsOverride

function compute_assumed_setting(override::EffectsOverride, @nospecialize(setting), val::Bool=true)
    if isexpr(setting, :call) && setting.args[1] === :(!)
        return compute_assumed_setting(override, setting.args[2], !val)
    elseif isa(setting, QuoteNode)
        return compute_assumed_setting(override, setting.value, val)
    end
    if setting === :consistent
        return EffectsOverride(override; consistent = val)
    elseif setting === :effect_free
        return EffectsOverride(override; effect_free = val)
    elseif setting === :nothrow
        return EffectsOverride(override; nothrow = val)
    elseif setting === :terminates_globally
        return EffectsOverride(override; terminates_globally = val)
    elseif setting === :terminates_locally
        return EffectsOverride(override; terminates_locally = val)
    elseif setting === :notaskstate
        return EffectsOverride(override; notaskstate = val)
    elseif setting === :inaccessiblememonly
        return EffectsOverride(override; inaccessiblememonly = val)
    elseif setting === :noub
        return EffectsOverride(override; noub = val)
    elseif setting === :noub_if_noinbounds
        return EffectsOverride(override; noub_if_noinbounds = val)
    elseif setting === :foldable
        consistent = effect_free = terminates_globally = noub = val
        return EffectsOverride(override; consistent, effect_free, terminates_globally, noub)
    elseif setting === :removable
        effect_free = nothrow = terminates_globally = val
        return EffectsOverride(override; effect_free, nothrow, terminates_globally)
    elseif setting === :total
        consistent = effect_free = nothrow = terminates_globally = notaskstate =
            inaccessiblememonly = noub = val
        return EffectsOverride(override;
            consistent, effect_free, nothrow, terminates_globally, notaskstate,
            inaccessiblememonly, noub)
    end
    return nothing
end

function form_purity_expr(override::EffectsOverride)
    return Expr(:purity,
        override.consistent, override.effect_free, override.nothrow,
        override.terminates_globally, override.terminates_locally, override.notaskstate,
        override.inaccessiblememonly, override.noub, override.noub_if_noinbounds)
end

"""
    Base.@nospecializeinfer function f(args...)
        @nospecialize ...
        ...
    end
    Base.@nospecializeinfer f(@nospecialize args...) = ...

Tells the compiler to infer `f` using the declared types of `@nospecialize`d arguments.
This can be used to limit the number of compiler-generated specializations during inference.

# Examples

```julia
julia> f(A::AbstractArray) = g(A)
f (generic function with 1 method)

julia> @noinline Base.@nospecializeinfer g(@nospecialize(A::AbstractArray)) = A[1]
g (generic function with 1 method)

julia> @code_typed f([1.0])
CodeInfo(
1 ─ %1 = invoke Main.g(_2::AbstractArray)::Any
└──      return %1
) => Any
```

In this example, `f` will be inferred for each specific type of `A`,
but `g` will only be inferred once with the declared argument type `A::AbstractArray`,
meaning that the compiler will not likely see the excessive inference time on it
while it can not infer the concrete return type of it.
Without the `@nospecializeinfer`, `f([1.0])` would infer the return type of `g` as `Float64`,
indicating that inference ran for `g(::Vector{Float64})` despite the prohibition on
specialized code generation.
"""
macro nospecializeinfer(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :nospecializeinfer) : ex)
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

unwrap_macrocalls(@nospecialize(x)) = x
function unwrap_macrocalls(ex::Expr)
    inner = ex
    while inner.head === :macrocall
        inner = inner.args[end]::Expr
    end
    return inner
end

function pushmeta!(ex::Expr, tag::Union{Symbol,Expr})
    inner = unwrap_macrocalls(ex)
    idx, exargs = findmeta(inner)
    if idx != 0
        metastmt = exargs[idx]::Expr
        push!(metastmt.args, tag)
    else
        body = inner.args[2]::Expr
        pushfirst!(body.args, Expr(:meta, tag))
    end
    return ex
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

function annotate_meta_def_or_block(@nospecialize(ex), meta::Symbol)
    inner = unwrap_macrocalls(ex)
    if is_function_def(inner)
        # annotation on a definition
        return esc(pushmeta!(ex, meta))
    else
        # annotation on a block
        return Expr(:block,
                    Expr(meta, true),
                    Expr(:local, Expr(:(=), :val, esc(ex))),
                    Expr(meta, false),
                    :val)
    end
end

function is_short_function_def(@nospecialize(ex))
    isexpr(ex, :(=)) || return false
    while length(ex.args) >= 1 && isa(ex.args[1], Expr)
        (ex.args[1].head === :call) && return true
        (ex.args[1].head === :where || ex.args[1].head === :(::)) || return false
        ex = ex.args[1]
    end
    return false
end
is_function_def(@nospecialize(ex)) =
    return isexpr(ex, :function) || is_short_function_def(ex) || isexpr(ex, :->)

function findmeta(ex::Expr)
    if is_function_def(ex)
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

"""
    Base.remove_linenums!(ex)

Remove all line-number metadata from expression-like object `ex`.
"""
function remove_linenums!(@nospecialize ex)
    if ex isa Expr
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
    elseif ex isa CodeInfo
        ex.codelocs .= 0
        length(ex.linetable::Vector) > 1 && resize!(ex.linetable::Vector, 1)
        return ex
    else
        return ex
    end
end

replace_linenums!(ex, ln::LineNumberNode) = ex
function replace_linenums!(ex::Expr, ln::LineNumberNode)
    if ex.head === :block || ex.head === :quote
        # replace line number expressions from metadata (not argument literal or inert) position
        map!(ex.args, ex.args) do @nospecialize(x)
            isa(x, Expr) && x.head === :line && length(x.args) == 1 && return Expr(:line, ln.line)
            isa(x, Expr) && x.head === :line && length(x.args) == 2 && return Expr(:line, ln.line, ln.file)
            isa(x, LineNumberNode) && return ln
            return x
        end
    end
    # preserve any linenums inside `esc(...)` guards
    if ex.head !== :escape
        for subex in ex.args
            subex isa Expr && replace_linenums!(subex, ln)
        end
    end
    return ex
end

macro generated()
    return Expr(:generated)
end

"""
    @generated f

`@generated` is used to annotate a function which will be generated.
In the body of the generated function, only types of arguments can be read
(not the values). The function returns a quoted expression evaluated when the
function is called. The `@generated` macro should not be used on functions mutating
the global scope or depending on mutable elements.

See [Metaprogramming](@ref) for further details.

# Examples
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
If no `order` is specified it defaults to :sequentially_consistent.

    @atomic a.b.x = new
    @atomic a.b.x += addend
    @atomic :release a.b.x = new
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


See [Per-field atomics](@ref man-atomics) section in the manual for more details.

# Examples
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
3 => 4

julia> @atomic a.x # fetch field x of a, with sequential consistency
4

julia> @atomic max(a.x, 10) # change field x of a to the max value, with sequential consistency
4 => 10

julia> @atomic a.x max 5 # again change field x of a to the max value, with sequential consistency
10 => 10
```

!!! compat "Julia 1.7"
    This functionality requires at least Julia 1.7.
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

See [Per-field atomics](@ref man-atomics) section in the manual for more details.

# Examples
```jldoctest
julia> mutable struct Atomic{T}; @atomic x::T; end

julia> a = Atomic(1)
Atomic{Int64}(1)

julia> @atomicswap a.x = 2+2 # replace field x of a with 4, with sequential consistency
1

julia> @atomic a.x # fetch field x of a, with sequential consistency
4
```

!!! compat "Julia 1.7"
    This functionality requires at least Julia 1.7.
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

See [Per-field atomics](@ref man-atomics) section in the manual for more details.

# Examples
```jldoctest
julia> mutable struct Atomic{T}; @atomic x::T; end

julia> a = Atomic(1)
Atomic{Int64}(1)

julia> @atomicreplace a.x 1 => 2 # replace field x of a with 2 if it was 1, with sequential consistency
(old = 1, success = true)

julia> @atomic a.x # fetch field x of a, with sequential consistency
2

julia> @atomicreplace a.x 1 => 2 # replace field x of a with 2 if it was 1, with sequential consistency
(old = 2, success = false)

julia> xchg = 2 => 0; # replace field x of a with 0 if it was 2, with sequential consistency

julia> @atomicreplace a.x xchg
(old = 2, success = true)

julia> @atomic a.x # fetch field x of a, with sequential consistency
0
```

!!! compat "Julia 1.7"
    This functionality requires at least Julia 1.7.
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

"""
    @atomiconce a.b.x = value
    @atomiconce :sequentially_consistent a.b.x = value
    @atomiconce :sequentially_consistent :monotonic a.b.x = value

Perform the conditional assignment of value atomically if it was previously
unset, returning the value `success::Bool`. Where `success` indicates whether
the assignment was completed.

This operation translates to a `setpropertyonce!(a.b, :x, value)` call.

See [Per-field atomics](@ref man-atomics) section in the manual for more details.

# Examples
```jldoctest
julia> mutable struct AtomicOnce
           @atomic x
           AtomicOnce() = new()
       end

julia> a = AtomicOnce()
AtomicOnce(#undef)

julia> @atomiconce a.x = 1 # set field x of a to 1, if unset, with sequential consistency
true

julia> @atomic a.x # fetch field x of a, with sequential consistency
1

julia> @atomiconce a.x = 1 # set field x of a to 1, if unset, with sequential consistency
false
```

!!! compat "Julia 1.11"
    This functionality requires at least Julia 1.11.
"""
macro atomiconce(success_order, fail_order, ex)
    fail_order isa QuoteNode || (fail_order = esc(fail_order))
    success_order isa QuoteNode || (success_order = esc(success_order))
    return make_atomiconce(success_order, fail_order, ex)
end
macro atomiconce(order, ex)
    order isa QuoteNode || (order = esc(order))
    return make_atomiconce(order, order, ex)
end
macro atomiconce(ex)
    return make_atomiconce(QuoteNode(:sequentially_consistent), QuoteNode(:sequentially_consistent), ex)
end
function make_atomiconce(success_order, fail_order, ex)
    @nospecialize
    is_expr(ex, :(=), 2) || error("@atomiconce expression missing assignment")
    l, val = ex.args[1], esc(ex.args[2])
    is_expr(l, :., 2) || error("@atomiconce expression missing field access")
    ll, lr = esc(l.args[1]), esc(l.args[2])
    return :(setpropertyonce!($ll, $lr, $val, $success_order, $fail_order))
end
