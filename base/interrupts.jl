# Internal methods, only to be used to change to a different global interrupt handler
function _register_global_interrupt_handler(handler::Task)
    handler_ptr = Base.pointer_from_objref(handler)
    slot_ptr = cglobal(:jl_interrupt_handler, Ptr{Cvoid})
    Intrinsics.atomic_pointerset(slot_ptr, handler_ptr, :release)
end
function _unregister_global_interrupt_handler()
    slot_ptr = cglobal(:jl_interrupt_handler, Ptr{Cvoid})
    Intrinsics.atomic_pointerset(slot_ptr, C_NULL, :release)
end

const INTERRUPT_HANDLERS_LOCK = Threads.ReentrantLock()
const INTERRUPT_HANDLERS = Dict{Module,Vector{Task}}()
const INTERRUPT_HANDLER_RUNNING = Threads.Atomic{Bool}(false)

"""
    register_interrupt_handler(mod::Module, handler::Task)

Registers the task `handler` to handle interrupts (such as from Ctrl-C).
Handlers are expected to sit idly within a `wait()` call or similar. When an
interrupt is received by Ctrl-C or manual SIGINT, one of two actions may
happen:

If the REPL is not running (such as when running `julia myscript.jl`), then all
registered interrupt handlers will be woken with an `InterruptException()`, and
the handler may take whatever actions are necessary to gracefully interrupt any
associated running computations. It is expected that the handler will spawn
tasks to perform the graceful interrupt, so that the handler task may return
quickly to again calling `wait()` to catch future user interrupts.

If the REPL is running, then the user will be presented with a terminal menu
which will allow them to do one of:
- Ignore the interrupt (do nothing)
- Activate all handlers for all modules
- Activate all handlers for a specific module
- Disable this interrupt handler logic (see below for details)
- Exit Julia gracefully (with `exit`)
- Exit Julia forcefully (with a `ccall` to `abort`)

Note that if the interrupt handler logic is disabled by the above menu option,
Julia will fall back to the old Ctrl-C handling behavior, which has the
potential to cause crashes and undefined behavior (but can also interrupt more
kinds of code). If desired, the interrupt handler logic can be re-enabled by
calling `start_repl_interrupt_handler()`, which will disable the old Ctrl-C
handling behavior.

To unregister a previously-registered handler, use
[`unregister_interrupt_handler`](@ref).

!!! warn
    Non-yielding tasks may block interrupt handlers from running; this means
    that once an interrupt handler is registered, code like `while true end`
    may become un-interruptible.
"""
function register_interrupt_handler(mod::Module, handler::Task)
    if ccall(:jl_generating_output, Cint, ()) == 1
        throw(ConcurrencyViolationError("Interrupt handlers cannot be registered during precompilation.\nPlease register your handler later (possibly in your module's `__init__`)."))
    end
    lock(INTERRUPT_HANDLERS_LOCK) do
        handlers = get!(Vector{Task}, INTERRUPT_HANDLERS, mod)
        push!(handlers, handler)
    end
end

"""
    unregister_interrupt_handler(mod::Module, handler::Task)

Unregisters the interrupt handler task `handler`; see
[`register_interrupt_handler`](@ref) for further details.
"""
function unregister_interrupt_handler(mod::Module, handler::Task)
    if ccall(:jl_generating_output, Cint, ()) == 1
        throw(ConcurrencyViolationError("Interrupt handlers cannot be unregistered during precompilation."))
    end
    lock(INTERRUPT_HANDLERS_LOCK) do
        handlers = get!(Vector{Task}, INTERRUPT_HANDLERS, mod)
        deleteat!(handlers, findall(==(handler), handlers))
    end
end

function _throwto_interrupt!(task::Task)
    if task.state == :runnable
        task._isexception = true
        task.result = InterruptException()
        try
            schedule(task)
        catch
        end
    end
end

# Simple (no TUI) interrupt handler

function simple_interrupt_handler()
    last_time = 0.0
    while true
        try
            # Wait to be interrupted
            wait()
        catch err
            if !(err isa InterruptException)
                rethrow(err)
            end

            # Force-interrupt root task if two interrupts in quick succession (< 1s)
            now_time = time()
            diff_time = now_time - last_time
            last_time = now_time
            if diff_time < 1
                _throwto_interrupt!(Base.roottask)
            end

            # Interrupt all handlers
            lock(INTERRUPT_HANDLERS_LOCK) do
                for mod in keys(INTERRUPT_HANDLERS)
                    for handler in INTERRUPT_HANDLERS[mod]
                        if handler.state == :runnable
                            _throwto_interrupt!(handler)
                        end
                    end
                end
            end
        end
    end
end
function simple_interrupt_handler_checked()
    try
        simple_interrupt_handler()
    catch err
        # Some internal error, make sure we start a new handler
        Threads.atomic_xchg!(INTERRUPT_HANDLER_RUNNING, false)
        _unregister_global_interrupt_handler()
        start_simple_interrupt_handler()
        rethrow()
    end
    # Clean exit
    Threads.atomic_xchg!(INTERRUPT_HANDLER_RUNNING, false)
    _unregister_global_interrupt_handler()
end
function start_simple_interrupt_handler(; force::Bool=false)
    if (Threads.atomic_cas!(INTERRUPT_HANDLER_RUNNING, false, true) == false) || force
        simple_interrupt_handler_task = errormonitor(Threads.@spawn simple_interrupt_handler_checked())
        _register_global_interrupt_handler(simple_interrupt_handler_task)
    end
end

# REPL (TUI) interrupt handler

function repl_interrupt_handler()
    invokelatest(REPL_MODULE_REF[]) do REPL
        TerminalMenus = REPL.TerminalMenus

        root_menu = TerminalMenus.RadioMenu(
            [
             "Interrupt all",
             "Interrupt only...",
             "Interrupt root task (REPL/script)",
             "Ignore it",
             "Stop handling interrupts",
             "Exit Julia",
             "Force-exit Julia",
            ]
        )

        while true
            try
                # Wait to be interrupted
                wait()
            catch err
                if !(err isa InterruptException)
                    rethrow(err)
                end

                # Display root menu
                @label display_root
                choice = TerminalMenus.request("Interrupt received, select an action:", root_menu)
                if choice == 1
                    lock(INTERRUPT_HANDLERS_LOCK) do
                        for mod in keys(INTERRUPT_HANDLERS)
                            for handler in INTERRUPT_HANDLERS[mod]
                                if handler.state == :runnable
                                    _throwto_interrupt!(handler)
                                end
                            end
                        end
                    end
                elseif choice == 2
                    # Display modules menu
                    mods = lock(INTERRUPT_HANDLERS_LOCK) do
                        collect(keys(INTERRUPT_HANDLERS))
                    end
                    mod_menu = TerminalMenus.RadioMenu(vcat(map(string, mods), "Go Back"))
                    @label display_mods
                    choice = TerminalMenus.request("Select a library to interrupt:", mod_menu)
                    if choice > length(mods) || choice == -1
                        @goto display_root
                    else
                        lock(INTERRUPT_HANDLERS_LOCK) do
                            for handler in INTERRUPT_HANDLERS[mods[choice]]
                                _throwto_interrupt!(handler)
                            end
                        end
                        @goto display_mods
                    end
                elseif choice == 3
                    # Force-interrupt root task
                    _throwto_interrupt!(Base.roottask)
                elseif choice == 4 || choice == -1
                    # Do nothing
                elseif choice == 5
                    # Exit handler (caller will unregister us)
                    return
                elseif choice == 6
                    # Exit Julia cleanly
                    exit()
                elseif choice == 7
                    # Force an exit
                    ccall(:abort, Cvoid, ())
                end
            end
        end
    end
end
function repl_interrupt_handler_checked()
    try
        repl_interrupt_handler()
    catch err
        # Some internal error, make sure we start a new handler
        Threads.atomic_xchg!(INTERRUPT_HANDLER_RUNNING, false)
        _unregister_global_interrupt_handler()
        start_repl_interrupt_handler()
        rethrow()
    end
    # Clean exit
    Threads.atomic_xchg!(INTERRUPT_HANDLER_RUNNING, false)
    _unregister_global_interrupt_handler()
end
function start_repl_interrupt_handler(; force::Bool=false)
    if (Threads.atomic_cas!(INTERRUPT_HANDLER_RUNNING, false, true) == false) || force
        repl_interrupt_handler_task = errormonitor(Threads.@spawn repl_interrupt_handler_checked())
        _register_global_interrupt_handler(repl_interrupt_handler_task)
    end
end
