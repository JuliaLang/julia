# [Task Cancellation](@id man-cancellation)

Long-running work sometimes needs to be stopped before it finishes: the user pressed ^C, a
request timed out, a sibling computation already produced the answer, or the application is
shutting down. Julia models this with *cancellation tokens*. Cancellation is cooperative:
requesting it does not destroy anything by itself. Instead, blocking operations governed by a
cancelled token stop waiting and throw, and computational code can opt in by placing explicit
cancellation points. Cancelled code therefore unwinds through ordinary exception handling —
`finally` blocks run, resources are released, and the caller sees a
[`Base.CancellationRequest`](@ref).

## Tokens and token sources

Cancellation is requested through a [`Base.CancellationTokenSource`](@ref) and observed
through a [`Base.CancellationToken`](@ref). The split separates the two capabilities: whoever
holds the *source* can cancel; whoever holds a *token* can only be cancelled (or query whether
that has happened). A function that receives a token cannot cancel the operations of other
code sharing it.

```jldoctest
julia> src = Base.CancellationTokenSource();

julia> tok = Base.CancellationToken(src);

julia> Base.iscancelled(tok)
false

julia> Base.cancel!(src)
true

julia> Base.iscancelled(tok)
true
```

Cancellation is *level-triggered* and permanent: a source, once cancelled, stays cancelled.
There is no way to reset it — code that wants to accept new work after a cancellation creates
a fresh source. [`Base.cancel!`](@ref) is idempotent and safe to call from any task or thread,
including from inside a signal-driven context like a timeout timer.

Sources form a tree: a source created with a parent token is cancelled whenever any of its
ancestors is cancelled, while cancelling the child leaves the parent untouched. This is how an
application composes shutdown: one root source for the whole service, a child per request, a
grandchild per subtask.

```jldoctest
julia> parent = Base.CancellationTokenSource();

julia> child = Base.CancellationTokenSource(Base.CancellationToken(parent));

julia> Base.cancel!(parent);

julia> Base.iscancelled(child)
true
```

More generally, a source may be linked under *several* parent tokens at once, making the
structure a directed acyclic graph: the linked source is cancelled as soon as any one of its
parents is, at the highest severity requested among them. Use this when one operation must
respect two independent lifetimes — say, a per-request scope and the connection it arrived on:

```jldoctest
julia> request = Base.CancellationTokenSource(); connection = Base.CancellationTokenSource();

julia> linked = Base.CancellationTokenSource(Base.CancellationToken(request),
                                             Base.CancellationToken(connection));

julia> Base.cancel!(connection);

julia> Base.iscancelled(linked)
true
```

A source created under an already-cancelled parent is born cancelled. There is no operation
for detaching a source from the graph: a child stays attached for exactly as long as it is
*reachable*. Held tokens, tasks blocked under it and running work governed by it all keep it
alive; once nothing can observe a source's cancellation any more, it is garbage collected and
thereby drops out of the graph.

## Cancelling blocking operations

Blocking operations throughout Base — [`wait`](@ref), [`sleep`](@ref), [`lock`](@ref),
[`Channel`](@ref) operations, stream and file-descriptor I/O, [`run`](@ref), the Sockets and
FileWatching APIs — accept a `cancel` keyword argument. While the given token's source is
cancelled, the operation throws a [`Base.CancellationRequest`](@ref) instead of blocking; if
cancellation strikes mid-wait, the operation stops waiting and throws:

```jldoctest
julia> src = Base.CancellationTokenSource();

julia> Base.cancel!(src);

julia> sleep(10; cancel = Base.CancellationToken(src))
ERROR: CancellationRequest: Safe Cancellation (CANCEL_REQUEST_SAFE)
[...]
```

Passing a token explicitly is the exception rather than the rule, though: the `cancel`
keyword's default is the *scoped token* described next, so most code is cancellable without
mentioning cancellation at all. The other explicit form, `cancel = nothing`, makes a specific
operation non-cancellable.

## The scoped token

Threading a token through every call would be invasive, so the governing token of a
computation travels implicitly, in the [`Base.CANCEL_TOKEN`](@ref) [scoped
value](@ref scoped-values). Establish it with the standard scoped-value API
([`with`](@ref Base.ScopedValues.with) or [`@with`](@ref Base.ScopedValues.@with)); every
blocking operation
inside the scope — however deeply nested, and including all tasks spawned inside — defaults
to it:

```julia
using Base.ScopedValues

src = Base.CancellationTokenSource()
t = with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
    Threads.@spawn begin
        for item in items
            result = fetch_and_process(item)   # any blocking call in here observes `src`
            put!(results, result)
        end
    end
end

# ... later, from any other task (e.g. a timeout, or user request):
Base.cancel!(src)

wait(t)   # throws TaskFailedException wrapping the CancellationRequest
```

The current scope's token can be read back with `Base.CANCEL_TOKEN[]`, for example to hand
it across a boundary that does not preserve dynamic scope (a `ccall` callback, a queue
consumed by unrelated tasks, another process).

Structured-concurrency blocks participate automatically: an [`@sync`](@ref) block forms a
cancellation scope of its own, nested in the enclosing one. Cancelling an enclosing scope
therefore reaches every task spawned inside the block, and the block re-throws the
cancellation once its children have unwound.

## Severities

[`Base.cancel!`](@ref) takes a severity, and repeated cancellation of the same source may
*escalate* (never de-escalate) through three levels:

  * [`Base.CANCEL_REQUEST_SAFE`](@ref) (the default): unwind cooperatively. In-flight I/O is
    cancelled through its safe teardown paths, and cleanup code runs to completion. This is
    the level to try first, and the only one that guarantees a consistent program state
    afterwards.
  * [`Base.CANCEL_REQUEST_ABANDON_EXTERNAL`](@ref): additionally stop *waiting* for external
    resources (in-flight I/O, subprocesses, foreign calls) to acknowledge the cancellation.
    Use when safe cancellation appears stuck on an unresponsive peer.
  * [`Base.CANCEL_REQUEST_ABANDON_ALL`](@ref): give up on the affected tasks entirely; tasks
    that have not responded are frozen in place and never run again. Locks and other
    resources they hold are leaked. This is a last resort for recovering a session, not a
    normal cancellation mechanism.

Waiters that already unwound at a lower severity are re-notified when the severity escalates.
The severity a piece of code was cancelled at is recorded in the thrown
[`Base.CancellationRequest`](@ref).

## Cancellation is level-triggered: cleanup and shielding

Catching a `CancellationRequest` does not make the cancellation go away — as long as the
governing token remains cancelled, every subsequent cancellable operation in that scope keeps
throwing. This is deliberate: it makes "cancelled" a state of the computation rather than a
one-shot event that could be accidentally swallowed by an over-broad `catch`.

The consequence: cleanup that must *block* (flushing a log, awaiting a confirmation, taking a
lock) while its own scope is being cancelled must *shield* itself, either per operation with
`cancel = nothing`, or for a whole block by scoping the token out with
`Base.CANCEL_TOKEN => nothing`:

```jldoctest
julia> using Base.ScopedValues

julia> src = Base.CancellationTokenSource(); Base.cancel!(src);

julia> with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
           # ... the cancelled computation would unwind through here ...
           sleep(0.01; cancel = nothing)  # shielded: completes despite the cancelled scope
           "cleanup done"
       end
"cleanup done"
```

Simple resource release usually needs no shielding: [`close`](@ref) is designed to complete
under a cancelled scope, and non-blocking cleanup is unaffected. Shield the exceptional
blocking cleanup step, not entire `finally` blocks.

## Reacting to cancellation: watcher tasks

Every operation above treats cancellation of the governing token as an *interruption*. Code
that instead wants to perform an action when cancellation strikes — closing a listening
socket to unblock an accept loop, telling a foreign library to stop, flipping a flag polled
by external code — can wait for a token directly: `wait(tok)` on a
[`Base.CancellationToken`](@ref) blocks until the token's source is cancelled and returns
the [`Base.CancellationRequest`](@ref) as an ordinary value, immediately if it already is.
Spawning a task that performs such a wait is the cancellation-callback pattern:

```julia
using Base.ScopedValues

src = Base.CancellationTokenSource(Base.CANCEL_TOKEN[])   # fires with the enclosing scope
watcher = Threads.@spawn with(Base.CANCEL_TOKEN => nothing) do   # shielded watcher
    wait(Base.CancellationToken(src))
    stop_engine!(engine)    # e.g. unblock foreign code that cannot observe tokens itself
end
try
    run_engine(engine)      # the work the callback protects
finally
    Base.cancel!(src)       # normal completion also releases the watcher
    wait(watcher)
end
```

Three points make this pattern reliable. The watched source is created as a *child* of the
enclosing scope, so cancelling the enclosing scope (a ^C, a timeout) fires it — but so does
the `finally`, which guarantees the watcher is released when the work completes normally
instead of lingering forever. The watcher runs its *entire body* under a shield
(`Base.CANCEL_TOKEN => nothing`): both the wait — whose whole purpose is to survive into the
cancellation — and the action, which the spawned task would otherwise perform under the
inherited, by-then-cancelled scope, where its own blocking operations would throw instead of
running. And the action itself should be of a kind that is harmless on the normal-completion
path (like [`close`](@ref) or an idempotent "stop" request), since the watcher runs in both
cases; check [`Base.iscancelled`](@ref) on the enclosing token when the two cases must be
distinguished.

When not shielded, `wait(tok)` takes the ordinary `cancel` keyword argument and an unrelated
governing token interrupts it like any other blocking operation. Waiting on the very token
that governs the wait is refused with an `ArgumentError`: completing and interrupting such
a wait would be the same event.

## Compute-bound code

A computation that never blocks never observes cancellation on its own. Loops that can run
for a long time opt in with [`Base.@cancel_check`](@ref), which throws the pending
`CancellationRequest` if the scope's token has been cancelled:

```julia
function solve!(model; cancel = Base.CANCEL_TOKEN[])
    while !converged(model)
        Base.@cancel_check cancel
        step!(model)
    end
    return model
end
```

The one-argument form checks the given token directly; the zero-argument form
`Base.@cancel_check` looks up the scoped token at each check. In addition to the check
itself, a cancellation point marks the surrounding compiled code as safely interruptible, so
the runtime can also deliver a cancellation *asynchronously* to code executing between two
checks of the same computation — one check ahead of a long-running compiled region is often
enough. Placing a check per iteration of an outer loop is a good default; extremely hot inner
loops can rely on an outer check.

Code blocked in a foreign call (e.g. a long-running C library) cannot be unwound safely from
the outside, but a library that supports external interruption can integrate with
cancellation through the `cancel_handler` option of [`@ccall`](@ref):

```julia
# void solver_stop(solver_t *);   -- async-signal-safe, per the library docs
function cancellable_solve!(solver::Ptr{Cvoid})
    @ccall cancel_handler=(STOP_HANDLER[], solver) libsolver.solve(solver::Ptr{Cvoid})::Cint
end
```

Here `STOP_HANDLER[]` holds a C-callable pointer (e.g. from `@cfunction`) to a
`handler(state, severity)` function. If the governing token is cancelled while the annotated
call is running, the runtime invokes the handler on the thread executing the call, like a
signal handler — at an arbitrary point of the foreign code, on the same stack, resuming the
call when the handler returns. The handler's job is only to make the foreign call return
early (set a flag, call the library's own cancellation entry point); the pending cancellation
is then thrown by the annotated call itself right after it returns, so a partially computed
result never escapes. The handler must observe signal-handler
discipline — no allocation, locks, yields or I/O — and must tolerate spurious and repeated
invocation; see [`@ccall`](@ref) for the full contract. This is the hook by which BLAS
wrappers can make a long matrix multiplication respond to ^C.

A library that has been audited for asynchronous unwinding can go further with
`@ccall reset_safe=true ...`, which lets a cancellation unwind the foreign computation at
an arbitrary instruction instead of waiting for it to return. `BigInt` (GMP) arithmetic is
annotated this way — combined with allocation hooks that briefly defer a cancellation
delivered while inside the allocator itself, a bignum loop with no cancellation points of
its own cancels cleanly at the first ^C.

See [Long-Running Foreign Calls: GC and Cancellation](@ref man-foreign-calls-gc-cancellation)
for the wrapper author's guide to these annotations (including `gc_safe`, which keeps the
rest of the runtime — GC and the ^C machinery included — responsive while a foreign call
runs) and a complete worked example.

## Interactive sessions and ^C

In the REPL, every evaluation runs under a fresh cancellation scope, and pressing ^C cancels
it at `CANCEL_REQUEST_SAFE` — interrupting whatever the evaluation (and anything it spawned)
is blocked on, or compute at its next cancellation point. If the cancellation has not
completed after a grace period — typically because compute-bound code lacks cancellation
points, or cleanup is stuck — Julia prints a message offering escalation, and further ^C
presses climb the severity ladder described above, ending, if need be, with the current task
being abandoned and the prompt recovered.

A non-interactive script runs entirely under one cancellation scope, and ^C cancels it the
same way. Because cancellation is level-triggered, a script that catches the
`CancellationRequest` remains in a cancelled scope: subsequent cancellable operations keep
throwing unless the script establishes a fresh token scope for the work it wants to continue
with. (Re-arming ^C itself onto such a fresh scope is possible but currently an internal
interface.)
