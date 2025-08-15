module Signals

using Base: AsyncCondition
using Base.Intrinsics: atomic_pointerset

export signal_abbrev, signal_name
public register_handler, deregister_handler

"""
    signal_abbrev(signum::Integer) -> Union{String,Nothing}

Returns the signal abbreviation (e.g. "TERM") associated with the signal number for standard
signals on this architecture. If the signal number is invalid `nothing` will be returned
instead.
"""
function signal_abbrev(signum::Integer)
    abbrev = ccall(:jl_sigabbrev, Cstring, (Cint,), signum)
    abbrev != C_NULL || return nothing
    return @static Sys.isbsd() ? uppercase(unsafe_string(abbrev)) : unsafe_string(abbrev)
end

"""
    signal_name(signum::Integer) -> Union{String,Nothing}

Returns the signal name (e.g. "SIGTERM") associated with the signal number for standard
signals on this architecture. If the signal number is invalid `nothing` will be returned
instead.
"""
function signal_name(signum::Integer)
    abbrev = signal_abbrev(signum)
    !isnothing(abbrev) || return nothing
    return string("SIG", abbrev)
end

# Generate the `SIG*` constants for standard POSIX signals. We need to generate these
# constants as the associated signal numbers are architecture specific.
for signum in 1:31
    signame = signal_name(signum)

    if !isnothing(signame)
        sigsym = Symbol(signame)
        @eval begin
            const $sigsym = $signum
            export $sigsym
        end
    end
end

Base.kill(pid::Integer, signum::Integer) = ccall(:kill, Cvoid, (Cint, Cint), pid, signum)
Base.kill(signum::Integer) = kill(getpid(), signum)

const _SIGNAL_HANDLER_LOCK = Base.ReentrantLock()
const _SIGNAL_HANDLER = Dict{Cint,Base.Callable}()

const _SIGNAL_ROUTER_TASK_LOCK = Base.ReentrantLock()
const _SIGNAL_ROUTER_TASK = Ref{Task}()

function _initialize_signal_router()
    condition = AsyncCondition()
    signal_router_task = Threads.@spawn signal_router(condition)
    errormonitor(signal_router_task)

    # Allow C code to notify the `AsyncCondition`.
    # jl_signal_router_condition_ptr = cglobal(:jl_signal_router_condition, Ptr{Cvoid})
    # atomic_pointerset(jl_signal_router_condition_ptr, condition.handle, :release)
    ccall(:jl_set_signal_router_condition, Cvoid, (Ptr{Cvoid},), condition.handle)
    return signal_router_task
end

"""
    signal_router() -> Nothing

Routes signals to user-defined signal handler functions via the `SIGNAL_CONDITION`.
Typically, this function is run in a separate thread and user-defined signal handlers
are run within that thread.
"""
function signal_router(condition::AsyncCondition)
    while isopen(condition)
        wait(condition)  # Wait until notified by `jl_signal_router_condition`
        signum = ccall(:jl_consume_user_signal, Cint, ())

        # Process all queued signals while the thread is active
        while signum != -1
            signal_handler = @lock _SIGNAL_HANDLER_LOCK begin
                get(_SIGNAL_HANDLER, signum, nothing)
            end

            if !isnothing(signal_handler)
                invokelatest(signal_handler, signum)
            end

            signum = ccall(:jl_consume_user_signal, Cint, ())
        end
    end
    return nothing
end

"""
    register_handler(handler, signum::Integer) -> Nothing

Registers a Julia function as a signal handler for the given signal. The provided signal
handler will respond to process-directed signals.

The signals `SIGKILL` and `SIGSTOP` cannot be caught and attempting to register a handler
will throw an `ArgumentError`.
"""
function register_handler(handler, signum::Integer)
    if Sys.isunix() && (signum == SIGKILL || signum == SIGSTOP)
        throw(ArgumentError("$(signal_name(signum)) is impossible to catch"))
    end

    # Initialize the `signal_router` task if this is the first time a user signal handler
    # is being registered
    @lock _SIGNAL_ROUTER_TASK_LOCK begin
        if !isassigned(_SIGNAL_ROUTER_TASK)
            _SIGNAL_ROUTER_TASK[] = _initialize_signal_router()
        end
    end

    @lock _SIGNAL_HANDLER_LOCK begin
        _SIGNAL_HANDLER[signum] = handler
    end
    ccall(:jl_register_user_signal, Cvoid, (Cint,), signum)
    return nothing
end

"""
    deregister_handler(signum::Integer) -> Nothing

Disassociates the Julia function from being triggered when this process receives the given
signal and restores the default Julia signal handler.
"""
function deregister_handler(signum::Integer)
    @lock _SIGNAL_HANDLER_LOCK begin
        if haskey(_SIGNAL_HANDLER, signum)
            delete!(_SIGNAL_HANDLER, signum)
        end
    end
    ccall(:jl_deregister_user_signal, Cvoid, (Cint,), signum)
    return nothing
end

end
