# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Experimental

!!! warning
    Types, methods, or macros defined in this module are experimental and subject
    to change and will not have deprecations. Caveat emptor.
"""
module Experimental

using Base: Threads, sync_varname, is_function_def, @propagate_inbounds
using Base: GenericCondition
using Base.Meta

"""
    Const(A::Array)

Mark an Array as constant/read-only. The invariant guaranteed is that you will not
modify an Array (through another reference) within an `@aliasscope` scope.

!!! warning
    Experimental API. Subject to change without deprecation.
"""
struct Const{T,N} <: DenseArray{T,N}
    a::Array{T,N}
end

Base.IndexStyle(::Type{<:Const}) = IndexLinear()
Base.size(C::Const) = size(C.a)
Base.axes(C::Const) = axes(C.a)
@propagate_inbounds Base.getindex(A::Const, i1::Int, I::Int...) = A.a[i1, I...]

"""
    @aliasscope expr

Allows the compiler to assume that all `Const`s are not being modified through stores
within this scope, even if the compiler can't prove this to be the case.

!!! warning
    Experimental API. Subject to change without deprecation.
"""
macro aliasscope(body)
    sym = gensym()
    quote
        $(Expr(:aliasscope))
        $sym = $(esc(body))
        $(Expr(:popaliasscope))
        $sym
    end
end


function sync_end(c::Channel{Any})
    if !isready(c)
        # there must be at least one item to begin with
        close(c)
        return
    end
    nremaining::Int = 0
    while true
        event = take!(c)
        if event === :__completion__
            nremaining -= 1
            if nremaining == 0
                break
            end
        else
            nremaining += 1
            schedule(Task(()->begin
                try
                    wait(event)
                    put!(c, :__completion__)
                catch e
                    close(c, e)
                end
            end))
        end
    end
    close(c)
    nothing
end

"""
    Experimental.@sync

Wait until all lexically-enclosed uses of [`@async`](@ref), [`@spawn`](@ref Threads.@spawn),
`Distributed.@spawnat` and `Distributed.@distributed`
are complete, or at least one of them has errored. The first exception is immediately
rethrown. It is the responsibility of the user to cancel any still-running operations
during error handling.

!!! Note
    This is different to [`@sync`](@ref) in that errors from wrapped tasks are thrown immediately,
    potentially before all tasks have returned.

!!! Note
    This interface is experimental and subject to change or removal without notice.
"""
macro sync(block)
    var = esc(sync_varname)
    quote
        let $var = Channel(Inf)
            v = $(esc(block))
            sync_end($var)
            v
        end
    end
end

"""
    Experimental.@optlevel n::Int

Set the optimization level (equivalent to the `-O` command line argument)
for code in the current module. Submodules inherit the setting of their
parent module.

Supported values are 0, 1, 2, and 3.

The effective optimization level is the minimum of that specified on the
command line and in per-module settings. If a `--min-optlevel` value is
set on the command line, that is enforced as a lower bound.
"""
macro optlevel(n::Int)
    return Expr(:meta, :optlevel, n)
end

"""
    Experimental.@max_methods n::Int

Set the maximum number of potentially-matching methods considered when running inference
for methods defined in the current module. This setting affects inference of calls with
incomplete knowledge of the argument types.

The benefit of this setting is to avoid excessive compilation and reduce invalidation risks
in poorly-inferred cases. For example, when `@max_methods 2` is set and there are two
potentially-matching methods returning different types inside a function body, then Julia
will compile subsequent calls for both types so that the compiled function body accounts
for both possibilities. Also the compiled code is vulnerable to invalidations that would
happen when either of the two methods gets invalidated. This speculative compilation and
these invalidations can be avoided by setting `@max_methods 1` and allowing the compiled
code to resort to runtime dispatch instead.

Supported values are `1`, `2`, `3`, `4`, and `default` (currently equivalent to `3`).
"""
macro max_methods(n::Int)
    1 <= n <= 4 || error("We must have that `1 <= max_methods <= 4`, but `max_methods = $n`.")
    return Expr(:meta, :max_methods, n)
end

"""
    Experimental.@max_methods n::Int function fname end

Set the maximum number of potentially-matching methods considered when running inference
for the generic function `fname`. Overrides any module-level or global inference settings
for max_methods. This setting is global for the entire generic function (or more precisely
the MethodTable).
"""
macro max_methods(n::Int, fdef::Expr)
    1 <= n <= 255 || error("We must have that `1 <= max_methods <= 255`, but `max_methods = $n`.")
    (fdef.head === :function && length(fdef.args) == 1) || error("Second argument must be a function forward declaration")
    return :(typeof($(esc(fdef))).name.max_methods = $(UInt8(n)))
end

"""
    Experimental.@compiler_options optimize={0,1,2,3} compile={yes,no,all,min} infer={true,false} max_methods={default,1,2,3,4}

Set compiler options for code in the enclosing module. Options correspond directly to
command-line options with the same name, where applicable. The following options
are currently supported:

  * `optimize`: Set optimization level.
  * `compile`: Toggle native code compilation. Currently only `min` is supported, which
    requests the minimum possible amount of compilation.
  * `infer`: Enable or disable type inference. If disabled, implies [`@nospecialize`](@ref).
  * `max_methods`: Maximum number of matching methods considered when running type inference.
"""
macro compiler_options(args...)
    opts = Expr(:block)
    for ex in args
        if isa(ex, Expr) && ex.head === :(=) && length(ex.args) == 2
            if ex.args[1] === :optimize
                push!(opts.args, Expr(:meta, :optlevel, ex.args[2]::Int))
            elseif ex.args[1] === :compile
                a = ex.args[2]
                a = #a === :no  ? 0 :
                    #a === :yes ? 1 :
                    #a === :all ? 2 :
                    a === :min ? 3 : error("invalid argument to \"compile\" option")
                push!(opts.args, Expr(:meta, :compile, a))
            elseif ex.args[1] === :infer
                a = ex.args[2]
                a = a === false || a === :no  ? 0 :
                    a === true  || a === :yes ? 1 : error("invalid argument to \"infer\" option")
                push!(opts.args, Expr(:meta, :infer, a))
            elseif ex.args[1] === :max_methods
                a = ex.args[2]
                a = a === :default ? 3 :
                  a isa Int ? ((1 <= a <= 4) ? a : error("We must have that `1 <= max_methods <= 4`, but `max_methods = $a`.")) :
                  error("invalid argument to \"max_methods\" option")
                push!(opts.args, Expr(:meta, :max_methods, a))
            else
                error("unknown option \"$(ex.args[1])\"")
            end
        else
            error("invalid option syntax")
        end
    end
    return opts
end

"""
    Experimental.@force_compile

Force compilation of the block or function (Julia's built-in interpreter is blocked from executing it).

# Examples

```
julia> occursin("interpreter", string(stacktrace(begin
           # with forced compilation
           Base.Experimental.@force_compile
           backtrace()
       end, true)))
false

julia> occursin("interpreter", string(stacktrace(begin
           # without forced compilation
           backtrace()
       end, true)))
true
```
"""
macro force_compile() Expr(:meta, :force_compile) end

# UI features for errors

"""
    Experimental.register_error_hint(handler, exceptiontype)

Register a "hinting" function `handler(io, exception)` that can
suggest potential ways for users to circumvent errors.  `handler`
should examine `exception` to see whether the conditions appropriate
for a hint are met, and if so generate output to `io`.
Packages should call `register_error_hint` from within their
`__init__` function.

For specific exception types, `handler` is required to accept additional arguments:

- `MethodError`: provide `handler(io, exc::MethodError, argtypes, kwargs)`,
  which splits the combined arguments into positional and keyword arguments.

When issuing a hint, the output should typically start with `\\n`.

If you define custom exception types, your `showerror` method can
support hints by calling [`Experimental.show_error_hints`](@ref).

# Examples

```
julia> module Hinter

       only_int(x::Int)      = 1
       any_number(x::Number) = 2

       function __init__()
           Base.Experimental.register_error_hint(MethodError) do io, exc, argtypes, kwargs
               if exc.f == only_int
                    # Color is not necessary, this is just to show it's possible.
                    print(io, "\\nDid you mean to call ")
                    printstyled(io, "`any_number`?", color=:cyan)
               end
           end
       end

       end
```

Then if you call `Hinter.only_int` on something that isn't an `Int` (thereby triggering a `MethodError`), it issues the hint:

```
julia> Hinter.only_int(1.0)
ERROR: MethodError: no method matching only_int(::Float64)
The function `only_int` exists, but no method is defined for this combination of argument types.
Did you mean to call `any_number`?
Closest candidates are:
    ...
```

!!! compat "Julia 1.5"
    Custom error hints are available as of Julia 1.5.
!!! warning
    This interface is experimental and subject to change or removal without notice.
    To insulate yourself against changes, consider putting any registrations inside an
    `if isdefined(Base.Experimental, :register_error_hint) ... end` block.
"""
function register_error_hint(@nospecialize(handler), @nospecialize(exct::Type))
    list = get!(Vector{Any}, _hint_handlers, exct)
    push!(list, handler)
    return nothing
end

const _hint_handlers = IdDict{Type,Vector{Any}}()

"""
    Experimental.show_error_hints(io, ex, args...)

Invoke all handlers from [`Experimental.register_error_hint`](@ref) for the particular
exception type `typeof(ex)`. `args` must contain any other arguments expected by
the handler for that type.

!!! compat "Julia 1.5"
    Custom error hints are available as of Julia 1.5.
!!! warning
    This interface is experimental and subject to change or removal without notice.
"""
function show_error_hints(io, ex, args...)
    hinters = get(_hint_handlers, typeof(ex), nothing)
    isnothing(hinters) && return
    for handler in hinters
        try
            @invokelatest handler(io, ex, args...)
        catch
            tn = typeof(handler).name
            @error "Hint-handler $handler for $(typeof(ex)) in $(tn.module) caused an error" exception=current_exceptions()
        end
    end
end

# OpaqueClosure
include("opaque_closure.jl")

"""
    Base.Experimental.@overlay mt def

Define a method and add it to the method table `mt` instead of to the global method table.
This can be used to implement a method override mechanism. Regular compilation will not
consider these methods, and you should customize the compilation flow to look in these
method tables (e.g., using [`Core.Compiler.OverlayMethodTable`](@ref)).

!!! note
    Please be aware that when defining overlay methods using `@overlay`, it is not necessary
    to have an original method that corresponds exactly in terms of how the method dispatches.
    This means that the method overlay mechanism enabled by `@overlay` is not implemented by
    replacing the methods themselves, but through an additional and prioritized method
    lookup during the method dispatch.

    Considering this, it is important to understand that in compilations using an overlay
    method table like the following, the method dispatched by `callx(x)` is not the regular
    method `callx(::Float64)`, but the overlay method `callx(x::Real)`:
    ```julia
    callx(::Real) = :real
    @overlay SOME_OVERLAY_MT callx(::Real) = :overlay_real
    callx(::Float64) = :float64

    # some overlay callsite
    let x::Float64
        callx(x) #> :overlay_real
    end
    ```
"""
macro overlay(mt, def)
    inner = Base.unwrap_macrocalls(def)
    is_function_def(inner) || error("@overlay requires a function definition")
    overlay_def!(mt, inner)
    return esc(def)
end

"""
    Base.Experimental.@consistent_overlay mt def

This macro operates almost identically to [`Base.Experimental.@overlay`](@ref), defining a
new overlay method. The key difference with this macro is that it informs the compiler that
the invocation of the overlay method it defines is `:consistent` with a regular,
non-overlayed method call.

More formally, when evaluating a generic function call ``f(x)`` at a specific world age
``i``, if a regular method call ``fᵢ(x)`` is redirected to an overlay method call ``fᵢ′(x)``
defined by this macro, ``fᵢ(x)`` and ``fᵢ′(x)`` are considered `:consistent` if the following
conditions are met:
- If ``fᵢ(x)`` returns a value ``y``, then ``fᵢ′(x)`` also returns some value ``yᵢ``, and ``y ≡ yᵢ`` holds.
- If ``fᵢ(x)`` throws an exception, then ``fᵢ′(x)`` also throws some exception.

For a detailed definition of `:consistent`-cy, consult the corresponding section in
[`Base.@assume_effects`](@ref).

!!! note
    Note that the requirements for `:consistent`-cy include not only that the return values
    are egal, but also that the manner of termination is the same. However, it's important
    to aware that when they throw exceptions, the exceptions themselves don't necessarily
    have to be egal. In other words, if ``fᵢ(x)`` throws an exception, ``fᵢ′(x)`` is
    required to also throw one, but the exact exceptions may differ.

!!! note
    Please note that the `:consistent`-cy requirement applies not to method itself but to
    _method invocation_. This means that for the use of `@consistent_overlay`, it is
    necessary for method invocations with the native regular compilation and those with
    a compilation with overlay method table to be `:consistent`.

    For example, it is important to understand that, `@consistent_overlay` can be used like
    the following:
    ```julia
    callsin(x::Real) = x < 0 ? error(x) : sin(x)
    @consistent_overlay SOME_OVERLAY_MT callsin(x::Float64) =
        x < 0 ? error_somehow(x) : sin(x)
    ```
    However, be aware that this `@consistent_overlay` will immediately become invalid if a
    new method for `callsin` is defined subsequently, such as:
    ```julia
    callsin(x::Float64) = cos(x)
    ```

    This specifically implies that the use of `@consistent_overlay` should be restricted as
    much as possible to cases where a regular method with a concrete signature is replaced
    by an overlay method with the same concrete signature.

    This constraint is closely related to the note in [`Base.Experimental.@overlay`](@ref);
    you are advised to consult that as well.
"""
macro consistent_overlay(mt, def)
    inner = Base.unwrap_macrocalls(def)
    is_function_def(inner) || error("@consistent_overlay requires a function definition")
    overlay_def!(mt, inner)
    override = Base.EffectsOverride(; consistent_overlay=true)
    Base.pushmeta!(def::Expr, Base.form_purity_expr(override))
    return esc(def)
end

function overlay_def!(mt, @nospecialize ex)
    arg1 = ex.args[1]
    if isexpr(arg1, :call)
        arg1.args[1] = Expr(:overlay, mt, arg1.args[1])
    elseif isexpr(arg1, :(::))
        overlay_def!(mt, arg1)
    elseif isexpr(arg1, :where)
        overlay_def!(mt, arg1)
    else
        error("@overlay requires a function definition")
    end
    return ex
end

let new_mt(name::Symbol, mod::Module) = begin
        ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), mod, "@MethodTable")
        ccall(:jl_new_method_table, Any, (Any, Any), name, mod)
    end
    @eval macro MethodTable(name::Symbol)
        esc(:(const $name = $$new_mt($(quot(name)), $(__module__))))
    end
end

"""
    Base.Experimental.@MethodTable name

Create a new MethodTable in the current module, bound to `name`. This method table can be
used with the [`Base.Experimental.@overlay`](@ref) macro to define methods for a function
without adding them to the global method table.
"""
:@MethodTable

"""
    Base.Experimental.make_io_thread()

Create a new thread that will run the Julia IO loop. This can potentially reduce the latency of some
IO operations as they no longer depend on the main thread to run it. This does mean that code that uses
this as implicit synchronization needs to be checked for correctness.
"""
function make_io_thread()
    tid = UInt[0]
    threadwork = @cfunction function(arg::Ptr{Cvoid})
            current_task().donenotify = Base.ThreadSynchronizer() #TODO: Should this happen by default in adopt thread?
            Base.errormonitor(current_task()) # this may not go particularly well if the IO loop is dead, but try anyways
            @ccall jl_set_io_loop_tid((Threads.threadid() - 1)::Int16)::Cvoid
            wait() # spin uv_run as long as needed
            nothing
        end Cvoid (Ptr{Cvoid},)
    err = @ccall uv_thread_create(tid::Ptr{UInt}, threadwork::Ptr{Cvoid}, C_NULL::Ptr{Cvoid})::Cint
    err == 0 || Base.uv_error("uv_thread_create", err)
    @ccall uv_thread_detach(tid::Ptr{UInt})::Cint
    err == 0 || Base.uv_error("uv_thread_detach", err)
    # n.b. this does not wait for the thread to start or to take ownership of the event loop
end

"""
    Base.Experimental.entrypoint(f, argtypes::Tuple)

Mark a method for inclusion when the `--trim` option is specified.
"""
function entrypoint(@nospecialize(f), @nospecialize(argtypes::Tuple))
    entrypoint(Tuple{Core.Typeof(f), argtypes...})
end

function entrypoint(@nospecialize(argt::Type))
    ccall(:jl_add_entrypoint, Int32, (Any,), argt)
    nothing
end

"""
    Base.Experimental.disable_new_worlds()

Mark that no new worlds (methods additions, deletions, etc) are permitted to be created at
any future time, allowing for lower latencies for some operations and slightly lower memory
usage, by eliminating the tracking of those possible invalidation.
"""
disable_new_worlds() = ccall(:jl_disable_new_worlds, Cvoid, ())

### Task metrics

"""
    Base.Experimental.task_metrics(::Bool)

Enable or disable the collection of per-task metrics.
A `Task` created when `Base.Experimental.task_metrics(true)` is in effect will have
[`Base.Experimental.task_running_time_ns`](@ref) and [`Base.Experimental.task_wall_time_ns`](@ref)
timing information available.

!!! note
    Task metrics can be enabled at start-up via the `--task-metrics=yes` command line option.
"""
function task_metrics(b::Bool)
    if b
        ccall(:jl_task_metrics_enable, Cvoid, ())
    else
        ccall(:jl_task_metrics_disable, Cvoid, ())
    end
    return nothing
end

"""
    Base.Experimental.task_running_time_ns(t::Task)::Union{UInt64, Nothing}

Return the total nanoseconds that the task `t` has spent running.
This metric is only updated when `t` yields or completes unless `t` is the current task, in
which it will be updated continuously.
See also [`Base.Experimental.task_wall_time_ns`](@ref).

Returns `nothing` if task timings are not enabled.
See [`Base.Experimental.task_metrics`](@ref).

!!! note "This metric is from the Julia scheduler"
    A task may be running on an OS thread that is descheduled by the OS
    scheduler, this time still counts towards the metric.

!!! compat "Julia 1.12"
    This method was added in Julia 1.12.
"""
function task_running_time_ns(t::Task=current_task())
    t.metrics_enabled || return nothing
    if t == current_task()
        # These metrics fields can't update while we're running.
        # But since we're running we need to include the time since we last started running!
        return t.running_time_ns + (time_ns() - t.last_started_running_at)
    else
        return t.running_time_ns
    end
end

"""
    Base.Experimental.task_wall_time_ns(t::Task)::Union{UInt64, Nothing}

Return the total nanoseconds that the task `t` was runnable.
This is the time since the task first entered the run queue until the time at which it
completed, or until the current time if the task has not yet completed.
See also [`Base.Experimental.task_running_time_ns`](@ref).

Returns `nothing` if task timings are not enabled.
See [`Base.Experimental.task_metrics`](@ref).

!!! compat "Julia 1.12"
    This method was added in Julia 1.12.
"""
function task_wall_time_ns(t::Task=current_task())
    t.metrics_enabled || return nothing
    start_at = t.first_enqueued_at
    start_at == 0 && return UInt64(0)
    end_at = t.finished_at
    end_at == 0 && return time_ns() - start_at
    return end_at - start_at
end

# wait_with_timeout
#
# A version of `wait(c::Condition)` that additionally allows the
# specification of a timeout. This is experimental as it will likely
# be dropped when a cancellation framework is added.
#
# The parallel behavior of wait_with_timeout is specified here. There
# are three concurrent entities that can interact:
# 1. Task W: the task that calls wait_with_timeout.
# 2. Task T: the task created to handle a timeout.
# 3. Task N: the task that notifies the Condition being waited on.
#
# Typical flow:
# - W enters the Condition's wait queue.
# - W creates T and stops running (calls wait()).
# - T, when scheduled, waits on a Timer.
# - Two common outcomes:
#   - N notifies the Condition.
#     - W starts running, closes the Timer, sets waiter_left and returns
#       the notify'ed value.
#     - The closed Timer throws an EOFError to T which simply ends.
#   - The Timer expires.
#     - T starts running and locks the Condition.
#     - T confirms that waiter_left is unset and that W is still in the
#       Condition's wait queue; it then removes W from the wait queue,
#       sets dosched to true and unlocks the Condition.
#     - If dosched is true, T schedules W with the special :timed_out
#       value.
#     - T ends.
#     - W runs and returns :timed_out.
#
# Some possible interleavings:
# - N notifies the Condition but the Timer expires and T starts running
#   before W:
#   - W closing the expired Timer is benign.
#   - T will find that W is no longer in the Condition's wait queue
#     (which is protected by a lock) and will not schedule W.
# - N notifies the Condition; W runs and calls wait on the Condition
#   again before the Timer expires:
#   - W sets waiter_left before leaving. When T runs, it will find that
#     waiter_left is set and will not schedule W.
#
# The lock on the Condition's wait queue and waiter_left together
# ensure proper synchronization and behavior of the tasks involved.

"""
    wait_with_timeout(c::GenericCondition; first::Bool=false, timeout::Real=0.0)

Wait for [`notify`](@ref) on `c` and return the `val` parameter passed to `notify`.

If the keyword `first` is set to `true`, the waiter will be put _first_
in line to wake up on `notify`. Otherwise, `wait` has first-in-first-out (FIFO) behavior.

If `timeout` is specified, cancel the `wait` when it expires and return
`:timed_out`. The minimum value for `timeout` is 0.001 seconds, i.e. 1
millisecond.
"""
function wait_with_timeout(c::GenericCondition; first::Bool=false, timeout::Real=0.0)
    ct = current_task()
    Base._wait2(c, ct, first)
    token = Base.unlockall(c.lock)

    timer::Union{Timer, Nothing} = nothing
    waiter_left::Union{Threads.Atomic{Bool}, Nothing} = nothing
    if timeout > 0.0
        timer = Timer(timeout)
        waiter_left = Threads.Atomic{Bool}(false)
        # start a task to wait on the timer
        t = Task() do
            try
                wait(timer)
            catch e
                # if the timer was closed, the waiting task has been scheduled; do nothing
                e isa EOFError && return
            end
            dosched = false
            lock(c.lock)
            # Confirm that the waiting task is still in the wait queue and remove it. If
            # the task is not in the wait queue, it must have been notified already so we
            # don't do anything here.
            if !waiter_left[] && ct.queue === c.waitq
                dosched = true
                Base.list_deletefirst!(c.waitq, ct)
            end
            unlock(c.lock)
            # send the waiting task a timeout
            dosched && schedule(ct, :timed_out)
        end
        t.sticky = false
        Threads._spawn_set_thrpool(t, :interactive)
        schedule(t)
    end

    try
        res = wait()
        if timer !== nothing
            close(timer)
            waiter_left[] = true
        end
        return res
    catch
        q = ct.queue; q === nothing || Base.list_deletefirst!(q::IntrusiveLinkedList{Task}, ct)
        rethrow()
    finally
        Base.relockall(c.lock, token)
    end
end

end # module
