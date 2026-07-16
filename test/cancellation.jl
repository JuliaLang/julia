# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: cancel!, CancellationRequest, CancellationToken, CancellationTokenSource,
    CANCEL_REQUEST_SAFE, CANCEL_REQUEST_ABANDON_EXTERNAL, CANCEL_REQUEST_ABANDON_ALL,
    CANCEL_TOKEN
using Base.ScopedValues: with, ScopedValue

# Start `f` as an @async-style (sticky, co-scheduled) task governed by a
# fresh cancellation source; returns (task, source).
function cancellable(f)
    src = CancellationTokenSource()
    t = with(() -> @async(f()), CANCEL_TOKEN => CancellationToken(src))
    return t, src
end

# Threads.@spawn-style variant (non-sticky, explicitly on the default pool -
# a compute-bound victim must not land on the interactive/io thread).
function cancellable_spawn(f)
    src = CancellationTokenSource()
    t = with(() -> Threads.@spawn(f()), CANCEL_TOKEN => CancellationToken(src))
    return t, src
end

# whether `t` is parked (its wait links are enqueued on some waitee)
# Lock-contention parking uses its own link set (`lock_queue`), disjoint
# from the condition-wait node (`wait_queue`); a task is parked if it is on
# either.
is_parked(t::Task) = t.wait_queue !== nothing || t.lock_queue !== nothing
parked_on(t::Task, @nospecialize(x)) = t.wait_queue === x || t.lock_queue === x

const collatz_code = quote
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n ÷ 2)
    function find_collatz_counterexample()
        i = 1
        while true
            j = i
            while true
                Base.@cancel_check
                j = collatz(j)
                j == 1 && break
                j == i && error("$j is a collatz counterexample")
            end
            i += 1
        end
    end
    @noinline function find_collatz_counterexample_inner()
        i = 1
        while true
            j = i
            while true
                j = collatz(j)
                j == 1 && break
                j == i && return j
            end
            i += 1
        end
    end
    function find_collatz_counterexample2()
        # A single cancellation point at function entry; interrupting the inner
        # (checkless) loop requires the reset_ctx mechanism.
        Base.@cancel_check
        return find_collatz_counterexample_inner()
    end
end
eval(collatz_code)

# wait a little, so cancellation targets are (most likely) started and parked
spin(n=4) = for _ in 1:n; yield(); end

@testset "cancellation token tree semantics" begin
    # cancel! marks the whole subtree, level-triggered
    root = CancellationTokenSource()
    child = CancellationTokenSource(CancellationToken(root))
    grandchild = CancellationTokenSource(CancellationToken(child))
    @test !Base.iscancelled(grandchild)
    @test cancel!(root)
    @test Base.iscancelled(root) && Base.iscancelled(child) && Base.iscancelled(grandchild)
    @test !cancel!(root) # idempotent at the same severity

    # a source attached under an already-cancelled parent is born cancelled
    late = CancellationTokenSource(CancellationToken(child))
    @test Base.iscancelled(late)
    @test Base.cancel_severity(late) === CANCEL_REQUEST_SAFE

    # escalation is monotonic and propagates down
    @test cancel!(root, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.cancel_severity(grandchild) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test !cancel!(grandchild, CANCEL_REQUEST_SAFE) # never de-escalates

    # children are held weakly: a child that becomes unreachable simply
    # drops out of the tree, while an escaped token keeps its source
    # attached (cancellation still reaches whoever can observe it)
    @noinline function make_children(root)
        CancellationTokenSource(CancellationToken(root)) # unreachable after return
        c = CancellationTokenSource(CancellationToken(root))
        return CancellationToken(c) # only the token escapes
    end
    root2 = CancellationTokenSource()
    kept = CancellationTokenSource(CancellationToken(root2))
    escaped_tok = make_children(root2)
    GC.gc()
    cancel!(root2) # the delivery walk prunes the collected child
    @test Base.iscancelled(kept)
    @test Base.iscancelled(escaped_tok)
    kids = root2.children::Vector{WeakRef}
    @test length(kids) == 2 # kept + escaped; the dead child was pruned
    @test all(w -> w.value !== nothing, kids)

    # cancellation is uniformly level-triggered: after catching the request,
    # unshielded waits under the cancelled scope keep throwing; shielded
    # cleanup proceeds
    src = CancellationTokenSource()
    phase = Ref{Any}(:init)
    t = with(CANCEL_TOKEN => CancellationToken(src)) do
        @async try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
            phase[] = :caught
            rethrew = try
                sleep(1000)
                false
            catch e2
                e2 isa CancellationRequest
            end
            sleep(0.01; cancel=nothing) # shielded cleanup is permitted
            phase[] = rethrew ? :done : :no_retrigger
        end
    end
    spin()
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test phase[] === :done

    # an internal teardown re-park (min_severity) is woken only by escalation
    srcm = CancellationTokenSource()
    cancel!(srcm)
    inner = @async sleep(5)
    tm = @async Base._wait(inner, CancellationToken(srcm); min_severity=0x01)
    spin()
    cancel!(srcm, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test_throws TaskFailedException wait(tm)
    @test tm.result isa CancellationRequest

    # linked sources: a source with several parents is cancelled by any of
    # them (the graph is a DAG, not just a tree)
    la = CancellationTokenSource()
    lb = CancellationTokenSource()
    linked = CancellationTokenSource(CancellationToken(la), CancellationToken(lb))
    @test !Base.iscancelled(CancellationToken(linked))
    @test cancel!(lb)
    @test Base.iscancelled(CancellationToken(linked))
    @test !Base.iscancelled(CancellationToken(la))
    # escalation propagates through the other parent too
    @test cancel!(la, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.cancel_severity(linked) === CANCEL_REQUEST_ABANDON_EXTERNAL

    # born cancelled at the highest severity among the parents
    lc = CancellationTokenSource()
    ld = CancellationTokenSource()
    cancel!(ld, CANCEL_REQUEST_ABANDON_EXTERNAL)
    born = CancellationTokenSource(CancellationToken(lc), CancellationToken(ld))
    @test Base.cancel_severity(born) === CANCEL_REQUEST_ABANDON_EXTERNAL

    # a diamond converges: the shared descendant is cancelled (once) from
    # the root, and a watcher parked on it completes normally
    droot = CancellationTokenSource()
    dl = CancellationTokenSource(CancellationToken(droot))
    dr = CancellationTokenSource(CancellationToken(droot))
    dd = CancellationTokenSource(CancellationToken(dl), CancellationToken(dr))
    dt = @async wait(CancellationToken(dd); cancel=nothing)
    @test timedwait(() -> parked_on(dt, dd), 10.0) == :ok
    cancel!(droot)
    @test timedwait(() -> istaskdone(dt), 10.0) == :ok
    @test fetch(dt) isa CancellationRequest

    # duplicate parents collapse to the single-parent form
    dup = CancellationTokenSource(CancellationToken(droot), CancellationToken(droot))
    @test dup.parents === droot
    @test Base.iscancelled(CancellationToken(dup)) # born under the cancelled root

    # a blocking operation under a linked scope is interrupted via either parent
    lp1 = CancellationTokenSource()
    lp2 = CancellationTokenSource()
    lchild = CancellationTokenSource(CancellationToken(lp1), CancellationToken(lp2))
    lt = @async sleep(1000; cancel=CancellationToken(lchild))
    @test timedwait(() -> is_parked(lt), 10.0) == :ok
    cancel!(lp2)
    @test timedwait(() -> istaskdone(lt), 10.0) == :ok
    @test lt.result isa CancellationRequest

    # the current scoped token is discoverable, and `=> nothing` scopes it out
    tok = CancellationToken(CancellationTokenSource())
    @test with(() -> CANCEL_TOKEN[], CANCEL_TOKEN => tok) === tok
    @test with(() -> CANCEL_TOKEN[], CANCEL_TOKEN => nothing) === nothing
    # an unrelated nested scope inherits the governing token
    inherited = with(CANCEL_TOKEN => tok) do
        with(() -> CANCEL_TOKEN[], ScopedValue(0) => 1)
    end
    @test inherited === tok
end

# Park a shielded watcher on a fresh child of `parent`; the child source
# escapes this frame only through the parked watcher task.
@noinline function _spawn_watcher_on_child(parent)
    child = CancellationTokenSource(CancellationToken(parent))
    t = @async wait(CancellationToken(child); cancel=nothing)
    @assert timedwait(() -> parked_on(t, child), 10.0) == :ok
    return t
end

@testset "waiting for a token as an event" begin
    # an already-cancelled token: immediate value return, no throw
    src = CancellationTokenSource()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    req = wait(CancellationToken(src); cancel=nothing)
    @test req isa CancellationRequest
    @test req.request == CANCEL_REQUEST_ABANDON_EXTERNAL.request

    # a parked watcher is completed (not interrupted) by the cancellation
    src = CancellationTokenSource()
    t = @async wait(CancellationToken(src); cancel=nothing)
    @test timedwait(() -> parked_on(t, src), 10.0) == :ok
    @test !istaskdone(t)
    cancel!(src)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test !istaskfailed(t)
    @test fetch(t) isa CancellationRequest
    @test src.watchers === nothing

    # ancestor cancellation reaches a watcher on a descendant source
    parent = CancellationTokenSource()
    child = CancellationTokenSource(CancellationToken(parent))
    t = @async wait(CancellationToken(child); cancel=nothing)
    @test timedwait(() -> parked_on(t, child), 10.0) == :ok
    cancel!(parent)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test fetch(t) isa CancellationRequest

    # ordinary cancel semantics: the governing token (inherited from the
    # scope) interrupts the wait, leaving the watched token untouched
    watched = CancellationTokenSource()
    gov = CancellationTokenSource()
    t = with(CANCEL_TOKEN => CancellationToken(gov)) do
        @async wait(CancellationToken(watched))
    end
    @test timedwait(() -> parked_on(t, watched), 10.0) == :ok
    cancel!(gov)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    @test !Base.iscancelled(CancellationToken(watched))
    @test watched.watchers === nothing

    # a watcher governed by an *ancestor* of the watched source is
    # interrupted, not completed: the ancestor's own drain claims it before
    # descending to the watched child (this is why callback watchers shield)
    outer = CancellationTokenSource()
    inner = CancellationTokenSource(CancellationToken(outer))
    t = with(CANCEL_TOKEN => CancellationToken(outer)) do
        @async wait(CancellationToken(inner))
    end
    @test timedwait(() -> parked_on(t, inner), 10.0) == :ok
    cancel!(outer)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)

    # waiting under the same token is refused, explicitly and inherited
    srcs = CancellationTokenSource()
    toks = CancellationToken(srcs)
    @test_throws ArgumentError wait(toks; cancel=toks)
    @test_throws ArgumentError with(() -> wait(toks), CANCEL_TOKEN => toks)

    # the callback pattern: a shielded watcher performs its action during
    # the cancellation
    src = CancellationTokenSource()
    fired = Base.Event()
    watcher = @async begin
        wait(CancellationToken(src); cancel=nothing)
        notify(fired)
    end
    @test timedwait(() -> parked_on(watcher, src), 10.0) == :ok
    cancel!(src)
    wait(fired; cancel=nothing)
    wait(watcher)
    @test !istaskfailed(watcher)

    # a parked watcher keeps the watched source attached to the tree
    # (observability == reachability), so an ancestor cancellation still
    # reaches it after a GC
    parent2 = CancellationTokenSource()
    t2 = _spawn_watcher_on_child(parent2)
    GC.gc()
    cancel!(parent2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test fetch(t2) isa CancellationRequest
end

@testset "cancellation of waiting tasks" begin
    # A task spawned under an already-cancelled scope starts but observes the
    # cancellation before running any user code
    src = CancellationTokenSource()
    body_ran = Ref(false)
    t = with(CANCEL_TOKEN => CancellationToken(src)) do
        @task (body_ran[] = true)
    end
    @test cancel!(src)
    schedule(t)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    @test t.result isa CancellationRequest
    @test !body_ran[]
    @test_throws TaskFailedException wait(t)

    # Cancellation of `sleep`
    t, src = cancellable(() -> sleep(1000))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest

    # After catching the request, cleanup that must block shields itself
    t2, src2 = cancellable() do
        try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
            sleep(0.01; cancel=nothing) # shielded: parking for cleanup
            return :cleanup_ok
        end
    end
    spin()
    cancel!(src2)
    @test fetch(t2) === :cleanup_ok

    # Cancellation of a task blocked on a Channel
    c = Channel{Int}(0)
    t, src = cancellable(() -> take!(c))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # The channel remains usable
    t2 = @async take!(c)
    put!(c, 7)
    @test fetch(t2) == 7

    # Cancelling a scope reaches a task waiting on another task; the waited-on
    # task (in the same scope) is cancelled through the same tree
    local t_in
    t, src = cancellable() do
        t_in = @async sleep(1000)
        wait(t_in)
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test timedwait(() -> istaskdone(t_in), 10.0) == :ok
    @test istaskfailed(t_in)

    # ... but a task waited on from a *different* scope is unaffected by the
    # waiter's cancellation
    t_out = @async sleep(5)
    t, src = cancellable(() -> wait(t_out))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test !istaskdone(t_out)
    wait(t_out)
    @test istaskdone(t_out) && !istaskfailed(t_out)
end

@testset "cancellation of lock and condition waits" begin
    # Task blocked in lock(::ReentrantLock)
    lk = ReentrantLock()
    lock(lk)
    t, src = cancellable(() -> lock(lk))
    spin()
    # let it spin through the fast path and park
    @test timedwait(() -> is_parked(t), 5.0) == :ok
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # the lock remains functional
    unlock(lk)
    @test trylock(lk)
    unlock(lk)
    t2 = @async (lock(lk); unlock(lk); true)
    @test fetch(t2)

    # Task blocked in put! on a full channel
    c = Channel{Int}(1)
    put!(c, 1)
    t, src = cancellable(() -> put!(c, 2))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @test take!(c) == 1
    put!(c, 3) # channel remains functional
    @test take!(c) == 3

    # Task blocked in wait(::Threads.Condition)
    cond = Threads.Condition()
    t, src = cancellable(() -> @lock cond wait(cond))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @lock cond notify(cond) # still functional (no waiters)

    # Task blocked in wait(::Base.Process); the process itself keeps running
    p = run(`sleep 1000`; wait=false)
    t, src = cancellable(() -> wait(p))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @test process_running(p)
    kill(p); wait(p)

    # Task blocked in waitany; the awaited tasks live in different scopes and
    # remain unaffected by the waiter's cancellation
    t1, src1 = cancellable(() -> sleep(1000))
    t2, src2 = cancellable(() -> sleep(1000))
    t, src = cancellable(() -> waitany([t1, t2]))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    @test !istaskdone(t1) && !istaskdone(t2)
    # their own scopes' cancellation reaches them
    cancel!(src1); cancel!(src2)
    @test_throws TaskFailedException wait(t1)
    @test_throws TaskFailedException wait(t2)
end

@testset "cancellation of stdlib waits (Sockets, FileWatching, Semaphore)" begin
    # Base.Semaphore: a cancelled acquire does not leak a permit
    sem = Base.Semaphore(1)
    Base.acquire(sem)
    t, src = cancellable(() -> Base.acquire(sem))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    Base.release(sem)
    Base.acquire(sem) # the permit is still available
    Base.release(sem)

    # Sockets.accept
    Sockets = Base.require(Base.PkgId(Base.UUID("6462fe0b-24de-5631-8697-dd941f90decc"), "Sockets"))
    port, server = Sockets.listenany(Sockets.localhost, 0)
    t, src = cancellable(() -> Sockets.accept(server))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # the server keeps accepting afterwards
    t2 = @async Sockets.accept(server)
    sock = Sockets.connect(Sockets.localhost, port)
    @test fetch(t2) isa Sockets.TCPSocket
    close(sock); close(server)

    # FileWatching: fd polling and file watching
    FileWatching = Base.require(Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"))
    if !Sys.iswindows() # fd polling requires a socket on Windows (ENOTSOCK)
        p = Pipe()
        Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
        fd = Base._fd(p.out)
        t, src = cancellable(() -> FileWatching.wait(fd; readable=true)) # nothing is ever written
        spin()
        cancel!(src)
        @test_throws TaskFailedException wait(t)
        @test t.result isa CancellationRequest
        close(p)
    end

    path = tempname()
    touch(path)
    t, src = cancellable(() -> FileWatching.watch_file(path, 100.0)) # the file never changes
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    rm(path)

    # Distributed: a (local) never-fulfilled Future wait; remote waits go
    # through the same channel-based wait path on the caller side
    Distributed = Base.require(Base.PkgId(Base.UUID("8ba89e20-285c-5b6f-9357-94700520ee1b"), "Distributed"))
    fut = Distributed.Future()
    t, src = cancellable(() -> fetch(fut))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    put!(fut, 1) # the future remains usable
    @test fetch(fut) == 1
end

@testset "explicit cancel keyword arguments" begin
    Sockets = Base.require(Base.PkgId(Base.UUID("6462fe0b-24de-5631-8697-dd941f90decc"), "Sockets"))
    FileWatching = Base.require(Base.PkgId(Base.UUID("7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"), "FileWatching"))
    cancelled_src = CancellationTokenSource()
    cancel!(cancelled_src)
    ctok = CancellationToken(cancelled_src)

    # a pre-cancelled token throws at entry, before any side effect
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    @test_throws CancellationRequest read(p.out, 10; cancel=ctok)
    @test_throws CancellationRequest read(p.out; cancel=ctok)
    @test_throws CancellationRequest read(p.out, String; cancel=ctok)
    @test_throws CancellationRequest read(p.out, UInt8; cancel=ctok)
    @test_throws CancellationRequest read!(p.out, zeros(UInt8, 4); cancel=ctok)
    @test_throws CancellationRequest readbytes!(p.out, zeros(UInt8, 4); cancel=ctok)
    @test_throws CancellationRequest readline(p.out; cancel=ctok)
    @test_throws CancellationRequest readuntil(p.out, 0x0a; cancel=ctok)
    @test_throws CancellationRequest readavailable(p.out; cancel=ctok)
    @test_throws CancellationRequest eof(p.out; cancel=ctok)
    @test_throws CancellationRequest write(p.in, zeros(UInt8, 8); cancel=ctok)
    @test_throws CancellationRequest write(p.in, "hello"; cancel=ctok)
    @test_throws CancellationRequest write(p.in, "a", "b"; cancel=ctok)
    @test_throws CancellationRequest flush(p.in; cancel=ctok)
    @test_throws CancellationRequest sleep(10; cancel=ctok)
    @test_throws CancellationRequest wait(Timer(10); cancel=ctok)
    @test_throws CancellationRequest run(`sleep 5`; cancel=ctok)
    @test_throws CancellationRequest success(`sleep 5`; cancel=ctok)
    @test_throws CancellationRequest read(`sleep 5`; cancel=ctok)
    @test_throws CancellationRequest readchomp(`sleep 5`; cancel=ctok)
    @test_throws CancellationRequest Sockets.getalladdrinfo("localhost"; cancel=ctok)
    @test_throws CancellationRequest Sockets.getaddrinfo("localhost"; cancel=ctok)
    @test_throws CancellationRequest Sockets.getnameinfo(Sockets.localhost; cancel=ctok)
    @test_throws CancellationRequest FileWatching.watch_file(tempdir(), 5.0; cancel=ctok)
    @test_throws CancellationRequest FileWatching.poll_fd(Base._fd(p.out), 5.0; readable=true, cancel=ctok)

    # `cancel = nothing` shadows an (already cancelled) outer scope
    write(p.in, "ab\n")
    with(CANCEL_TOKEN => ctok) do
        @test read(p.out, 2; cancel=nothing) == b"ab"
    end
    close(p)

    # live cancellation through an explicit token: blocked read
    p2 = Pipe()
    Base.link_pipe!(p2, reader_supports_async=true, writer_supports_async=true)
    src = CancellationTokenSource()
    t = @async read(p2.out, 10; cancel=CancellationToken(src))
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    close(p2)

    # live cancellation: blocked write
    p3 = Pipe()
    Base.link_pipe!(p3, reader_supports_async=true, writer_supports_async=true)
    src3 = CancellationTokenSource()
    big = zeros(UInt8, 200_000_000)
    t3 = @async write(p3.in, big; cancel=CancellationToken(src3))
    sleep(0.5)
    cancel!(src3)
    @test_throws TaskFailedException wait(t3)
    @test t3.result isa CancellationRequest
    close(p3)

    # live cancellation: Sockets.accept and recv with explicit tokens
    port, server = Sockets.listenany(Sockets.localhost, 0)
    src4 = CancellationTokenSource()
    t4 = @async Sockets.accept(server; cancel=CancellationToken(src4))
    spin()
    cancel!(src4)
    @test_throws TaskFailedException wait(t4)
    @test t4.result isa CancellationRequest
    close(server)

    udp = Sockets.UDPSocket()
    Sockets.bind(udp, Sockets.localhost, 0)
    src5 = CancellationTokenSource()
    t5 = @async Sockets.recv(udp; cancel=CancellationToken(src5))
    spin()
    cancel!(src5)
    @test_throws TaskFailedException wait(t5)
    @test t5.result isa CancellationRequest
    close(udp)

    # live cancellation: FileWatching.watch_file with an explicit token
    path = tempname()
    touch(path)
    src6 = CancellationTokenSource()
    t6 = @async FileWatching.watch_file(path, 100.0; cancel=CancellationToken(src6))
    spin()
    cancel!(src6)
    @test_throws TaskFailedException wait(t6)
    @test t6.result isa CancellationRequest
    rm(path)

    # live cancellation: run with an explicit token; the child process is
    # not reaped by the cancelled wait
    src7 = CancellationTokenSource()
    t7 = @async run(`sleep 5`; cancel=CancellationToken(src7))
    sleep(0.5)
    cancel!(src7)
    @test_throws TaskFailedException wait(t7)
    @test t7.result isa CancellationRequest
end

@testset "structured cancellation of @sync" begin
    t, src = cancellable() do
        @sync begin
            @async sleep(1000)
            @async sleep(1000)
        end
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CompositeException
    @test length(t.result.exceptions) == 2
end

@testset "escalation during @sync teardown keeps awaiting internal tasks" begin
    # A SAFE cancellation parks the @sync teardown on a child that has no
    # cancellation points; an ABANDON_EXTERNAL escalation must re-arm that
    # wait - internal tasks are still awaited at ABANDON_EXTERNAL - rather
    # than unwind the @sync while the child is still running.
    stop = Ref(false)
    started = Base.Event()
    t, src = cancellable() do
        @sync begin
            @async begin
                notify(started)
                while !stop[]
                    yield() # no cancellation points: ignores SAFE/ABANDON_EXTERNAL
                end
            end
        end
    end
    wait(started)
    cancel!(src)
    spin(20)
    @test !istaskdone(t)
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    spin(20)
    @test !istaskdone(t)
    stop[] = true
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    @test istaskfailed(t)
    req = t.result
    @test req isa CancellationRequest
    @test req.request == CANCEL_REQUEST_ABANDON_EXTERNAL.request
end

@testset "unfriendly cancellation modes" begin
    # Acknowledgment preserves the request's severity.
    seen = Ref{Any}(nothing)
    t, src = cancellable() do
        try
            sleep(1000)
        catch e
            seen[] = (e, Base.ambient_cancel_severity(), Base.abandoning_external_waits())
            rethrow()
        end
    end
    spin()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test timedwait(() -> istaskdone(t), 10.0) == :ok
    e, sev, abandoning = seen[]
    @test e === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test sev === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test abandoning

    # SAFE acknowledgments report SAFE severity and permit external waits.
    seen2 = Ref{Any}(nothing)
    t2, src2 = cancellable() do
        try
            sleep(1000)
        catch
            seen2[] = (Base.ambient_cancel_severity(), Base.abandoning_external_waits())
            rethrow()
        end
    end
    spin()
    cancel!(src2)
    @test timedwait(() -> istaskdone(t2), 10.0) == :ok
    @test seen2[] === (CANCEL_REQUEST_SAFE, false)

    # ABANDON_ALL freezes a parked task immediately: no unwind, no cleanup.
    cleanup_ran = Ref(false)
    t3, src3 = cancellable() do
        try
            sleep(1000)
        finally
            cleanup_ran[] = true
        end
    end
    spin()
    @test cancel!(src3, CANCEL_REQUEST_ABANDON_ALL)
    @test istaskdone(t3)
    @test t3.state === :abandoned
    @test istaskfailed(t3)
    @test !cleanup_ran[]
    @test_throws TaskFailedException wait(t3)

    # A task spawned into an ABANDON_ALL-cancelled scope never runs its body.
    src4 = CancellationTokenSource()
    body_ran = Ref(false)
    t4 = with(() -> @task(body_ran[] = true), CANCEL_TOKEN => CancellationToken(src4))
    @test cancel!(src4, CANCEL_REQUEST_ABANDON_ALL)
    schedule(t4)
    @test timedwait(() -> istaskdone(t4), 10.0) == :ok
    @test !body_ran[]

    # ABANDON_EXTERNAL interrupts a blocked stream write without waiting for
    # the write's cancellation to complete.
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        big = zeros(UInt8, 200_000_000)
        tw, srcw = cancellable(() -> write(p, big))
        spin()
        cancel!(srcw, CANCEL_REQUEST_ABANDON_EXTERNAL)
        @test timedwait(() -> istaskdone(tw), 10.0) == :ok
        @test istaskfailed(tw)
        @test tw.result === CANCEL_REQUEST_ABANDON_EXTERNAL
    finally
        close(p)
    end

    # Episode classification for the ^C escalation ladder.
    src = CancellationTokenSource()
    @test Base.sigint_active_severity(src) === nothing
    @test cancel!(src)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_SAFE
    @test cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_EXTERNAL
    @test cancel!(src, CANCEL_REQUEST_ABANDON_ALL)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_ALL
    # severities never de-escalate
    @test !cancel!(src, CANCEL_REQUEST_SAFE)
    @test Base.sigint_active_severity(src) === CANCEL_REQUEST_ABANDON_ALL
end

@testset "unfriendly cancellation of Experimental.@sync" begin
    # ABANDON_EXTERNAL propagates through the token tree to the children.
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t, src = cancellable() do
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(src, CANCEL_REQUEST_ABANDON_EXTERNAL)
    @test_throws TaskFailedException wait(t)
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])

    # ABANDON_ALL freezes the parent and the children alike (they are all
    # parked under the cancelled subtree).
    t3 = Ref{Task}()
    tp, srcp = cancellable() do
        Base.Experimental.@sync begin
            t3[] = @async sleep(1000)
        end
    end
    spin()
    @test cancel!(srcp, CANCEL_REQUEST_ABANDON_ALL)
    @test tp.state === :abandoned
    @test timedwait(() -> istaskdone(t3[]), 10.0) == :ok
    @test t3[].state === :abandoned
end

@testset "structured cancellation of Experimental.@sync" begin
    t1 = Ref{Task}(); t2 = Ref{Task}()
    t, src = cancellable() do
        Base.Experimental.@sync begin
            t1[] = @async sleep(1000)
            t2[] = @async sleep(1000)
        end
    end
    spin()
    cancel!(src)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # cancellation propagated to the children
    @test timedwait(() -> istaskdone(t1[]) && istaskdone(t2[]), 10.0) == :ok
    @test istaskfailed(t1[]) && istaskfailed(t2[])
end

@testset "cancellation of blocked stream writes" begin
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        # A write far exceeding the OS pipe buffer blocks until cancelled
        big = zeros(UInt8, 200_000_000)
        t, src = cancellable(() -> write(p, big))
        sleep(0.5)
        @test parked_on(t, p.in)
        cancel!(src)
        @test_throws TaskFailedException wait(t)
        @test t.result isa CancellationRequest
    finally
        close(p)
    end
end

@testset "cancellation of closewrite (shutdown) waits" begin
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        # A blocked write keeps the shutdown request (which queues behind it)
        # from completing; the closewrite wait must still be interruptible.
        big = zeros(UInt8, 200_000_000)
        tw, srcw = cancellable(() -> write(p, big))
        sleep(0.5)
        @test parked_on(tw, p.in)
        ts, srcs = cancellable(() -> closewrite(p.in))
        @test timedwait(() -> parked_on(ts, p.in), 5.0) == :ok
        cancel!(srcs)
        @test_throws TaskFailedException wait(ts)
        @test ts.result isa CancellationRequest
        cancel!(srcw)
        @test_throws TaskFailedException wait(tw)
    finally
        close(p)
    end
end

@testset "cancelled scopes are level-triggered" begin
    t, src = cancellable() do
        try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
        end
        # The scope stays cancelled: unshielded blocking operations keep
        # throwing until the task leaves the scope or shields.
        rethrew = try
            sleep(0.01)
            false
        catch e
            e isa CancellationRequest
        end
        # Shielded IO still works, and the severity remains observable.
        sleep(0.01; cancel=nothing)
        rethrew && Base.ambient_cancel_severity() === CANCEL_REQUEST_SAFE
    end
    spin()
    cancel!(src)
    @test fetch(t)
end

# Tests that need real thread parallelism (asynchronous interruption through
# the reset_ctx mechanism, task abandonment) always run with 2 threads,
# regardless of how the test driver was started.
@testset "threaded cancellation (subprocess with -t2)" begin
    cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no --threads=2 $(joinpath(@__DIR__, "cancellation_exec.jl"))`
    p = run(pipeline(cmd, stdout=stdout, stderr=stderr), wait=false)
    # A cancellation-delivery regression can wedge the child completely (a
    # surviving spin loop blocks GC's stop-the-world, which also blocks all
    # signal processing), in which case not even SIGTERM gets through.
    # SIGKILL it rather than hanging the test suite. The budget is generous:
    # on an oversubscribed CI box the child's compute-heavy testsets alone
    # can take several minutes.
    if timedwait(() -> process_exited(p), 600.0) !== :ok
        kill(p, Base.SIGKILL)
    end
    wait(p)
    @test success(p)
end

# On Windows uv_kill(SIGINT) terminates the child outright instead of
# delivering a console ^C, so none of these scenarios can run there.
Sys.isunix() && @testset "^C" begin
    function run_with_sigint(code::String, delays; forcekill::Bool=false,
                             open_stdin::Bool=false, threads::Int=0)
        # A readiness marker printed from user code proves the runtime is up
        # (signal handling armed, the script started) before any SIGINT is
        # sent - on a loaded machine startup alone can outlast the first delay
        # and an early SIGINT kills the child with no output at all.
        code = "println(\"CHILD-READY\")\n" * code
        out = Pipe()
        cmd = threads > 0 ?
            `$(Base.julia_cmd()) --startup-file=no --threads=$threads -e $code` :
            `$(Base.julia_cmd()) --startup-file=no -e $code`
        inpipe = open_stdin ? Pipe() : devnull
        p = run(pipeline(cmd, stdin=inpipe, stdout=out, stderr=out), wait=false)
        close(out.in)
        open_stdin && close(inpipe.out)
        readuntil(out, "CHILD-READY\n") # returns early (at EOF) if the child dies
        reader = @async read(out, String)
        killer = @async begin
            for d in delays
                sleep(d)
                process_running(p) && kill(p, Base.SIGINT)
            end
            if forcekill
                # e.g. an abandoned script has nothing left to run and idles
                sleep(3)
                process_running(p) && kill(p, Base.SIGKILL)
            end
        end
        wait(p)
        open_stdin && close(inpipe.in)
        wait(killer)
        return fetch(reader), p
    end

    # Catching ^C in a script: continuing requires re-arming a fresh ^C
    # epoch (the script's cancelled scope stays cancelled otherwise)
    output, p = run_with_sigint("""
        try
            sleep(100)
            println("FAIL: not cancelled")
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println("caught: ", typeof(e))
                println("continued")
                sleep(0.1) # cancellable operations work again
            end
        end
    """, [1.0])
    @test occursin("caught: Base.CancellationRequest", output)
    @test occursin("continued", output)
    @test p.exitcode == 0

    # Uncaught ^C produces a proper error report
    output, p = run_with_sigint("sleep(100)", [1.0])
    @test occursin("CancellationRequest: Safe Cancellation (CANCEL_REQUEST_SAFE)", output)
    @test p.exitcode == 1

    # ^C propagates through @sync, cancelling compute-bound and sleeping tasks
    output, p = run_with_sigint("""
        $(string(collatz_code))
        try
            @sync begin
                @async sleep(10000)
                @async find_collatz_counterexample()
            end
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println(typeof(e))
            end
        end
        """, [1.5])
    @test occursin("CompositeException", output)
    @test p.exitcode == 0

    # Escalation: an unresponsive process warns after 1s, and a second ^C
    # abandons the stuck task; with the interactive evaluator gone, the
    # process exits like an uncaught ^C
    output, p = run_with_sigint("""
        x = Ref(1.0)
        while true
            x[] = x[] * 1.0000001 + 0.1
        end
        """, [1.0, 2.5]; forcekill=true)
    @test occursin("failed to acknowledge SIGINT", output)
    @test occursin("Abandoning the current task", output)
    @test p.exitcode == 128 + 2

    # ^C with a stray @async task pending is catchable and the script exits
    # cleanly - historically a "fatal: error thrown and no exception handler
    # available" (issues #29369, #45055)
    output, p = run_with_sigint("""
        @async println("Hello!")
        try
            println("Hit ctrl-c!")
            sleep(10)
        catch err
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                showerror(stdout, err); println()
                println("done")
            end
        end
    """, [1.0])
    @test occursin("Hello!", output)
    @test occursin("CancellationRequest", output)
    @test occursin("done", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 0

    # ^C during a blocked read from stdin reports and exits - historically a
    # fatal unhandled InterruptException on the second press (issue #43451)
    output, p = run_with_sigint("read(stdin)", [1.0]; open_stdin=true)
    @test occursin("CancellationRequest", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 1

    # A rapid second press while the first cancellation is still unwinding
    # or reporting must not crash the process (issue #50045). The second
    # press may cancel the error-report epoch itself, in which case the
    # fallback note appears instead of the report.
    output, p = run_with_sigint("sleep(100)", [1.0, 0.1])
    @test occursin("CancellationRequest", output) ||
        occursin("displaying the error report failed", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 1

    # A catch-all loop that swallows every CancellationRequest cannot hide
    # from ^C (issue #4037): while the scope stays cancelled the request is
    # re-thrown at every blocking operation (the warning shows the
    # delivered-but-not-completed flavor), and the escalation ladder still
    # progresses to the point of abandoning the task. The abandonment rung
    # itself is a hail mary that may leave the process inconsistent, so this
    # asserts only that it is reached and announced - not any process
    # behavior after the freeze (the watchdog reaps the process).
    output, p = run_with_sigint("""
        while true
            try
                sleep(10)
            catch
            end
        end
    """, [1.0, 2.5, 2.5]; forcekill=true)
    @test occursin("Cancellation is in progress, but has not completed", output)
    # single-threaded sessions reach the abandonment through the C-side
    # direct path; threaded ones through the listener's rung - both announce
    # with "Abandoning the current task"
    @test occursin("Abandoning the current task", output)

    # ^C stops a swarm of print-flooding tasks and the script continues
    # (issue #47839)
    output, p = run_with_sigint("""
        ts = [@async (while true; println("hi"); end) for _ in 1:20]
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                for t in ts
                    try; wait(t); catch; end
                end
                println("ALL-STOPPED")
            end
        end
    """, [1.5])
    @test occursin("ALL-STOPPED", output)
    @test !occursin("fatal", output)
    @test p.exitcode == 0

    # ^C on a Threads.@threads loop raises a catchable CompositeException
    # instead of killing the process (issue #56462)
    output, p = run_with_sigint("""
        try
            Threads.@threads for i in 1:8
                sleep(100)
            end
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                println("caught: ", typeof(e))
                println("session-alive")
            end
        end
    """, [1.5]; threads=4)
    @test occursin("caught: CompositeException", output)
    @test occursin("session-alive", output)
    @test !occursin("fatal", output)
    @test !occursin("attempt to switch to exited task", output)
    @test p.exitcode == 0

    # A watcher task on the ^C episode token is the supported shape for a
    # user-defined interrupt handler (superseding the design of #49541): the
    # ^C completes - rather than unwinds - its wait, and its reaction runs
    # under its own shielded scope
    output, p = run_with_sigint("""
        tok = Base.CANCEL_TOKEN[]
        w = Threads.@spawn Base.ScopedValues.with(Base.CANCEL_TOKEN => nothing) do
            req = wait(tok)
            println("HANDLER-RAN ", typeof(req))
        end
        try
            sleep(100)
        catch e
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.sigint_new_episode!()) do
                wait(w)
                println("DONE")
            end
        end
    """, [1.0])
    @test occursin("HANDLER-RAN Base.CancellationRequest", output)
    @test occursin("DONE", output)
    @test p.exitcode == 0
end

if Sys.isunix()
    @testset "^C escalation ladder in the REPL (pty), $(isempty(tflags) ? "default threads" : join(tflags, " "))" for tflags in ([], ["-t2"])
        isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
        pts, ptm = Main.FakePTYs.open_fake_pty()
        env = copy(ENV)
        env["TERM"] = "dumb"
        env["JULIA_HISTORY"] = tempname()
        # Cover both thread topologies: the default session (a single default
        # thread, which the rescued backend monopolizes in episode 2 - only
        # the interactive-pool listener can escalate) and -t2 (the victim and
        # the listener compete inside a wider default pool).
        p = run(detach(setenv(`$(Base.julia_cmd()) -i -q --startup-file=no --color=no $tflags`, env)),
                pts, pts, pts; wait=false)
        ccall(:close, Cint, (Cint,), pts) # only the child owns the pts now

        transcript_lock = ReentrantLock()
        transcript = UInt8[]
        reader = @async try
            while true
                chunk = readavailable(ptm)
                isempty(chunk) && break
                @lock transcript_lock append!(transcript, chunk)
            end
        catch # pty closes when the child exits
        end
        cursor = Ref(1)
        snapshot() = @lock transcript_lock String(copy(transcript))
        function expect(needle::String; timeout::Real=30.0)
            status = timedwait(timeout; pollint=0.05) do
                idx = findnext(needle, snapshot(), cursor[])
                idx === nothing && return false
                cursor[] = last(idx) + 1
                return true
            end
            if status !== :ok
                @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
            end
            @test status == :ok
        end
        sendline(s) = write(ptm, s * "\n")
        # Wait until every needle appears at-or-after the cursor, in any
        # order, then advance the cursor past all of them: the single-thread
        # endgame's messages (rescue warning, error display, fresh prompt)
        # interleave nondeterministically.
        function expect_all(needles::String...; timeout::Real=30.0)
            last_end = Ref(cursor[])
            status = timedwait(timeout; pollint=0.05) do
                s = snapshot()
                stop = cursor[]
                for needle in needles
                    idx = findnext(needle, s, cursor[])
                    idx === nothing && return false
                    stop = max(stop, last(idx) + 1)
                end
                last_end[] = stop
                return true
            end
            if status !== :ok
                @error "expect_all timed out" needles tail=snapshot()[max(1, cursor[]):end]
            else
                cursor[] = last_end[]
            end
            @test status == :ok
        end

        expect("julia> ")
        # In a child with a single thread IN TOTAL, the julia-side listener -
        # the engine of the graded SAFE -> ABANDON_EXTERNAL -> ABANDON_ALL
        # ladder - is starved while the victim monopolizes it: a repeat press
        # re-offers the first rung, and the third press reaches the C-side
        # direct abandonment instead of the listener's rungs. An unadorned
        # `julia -i` session has an interactive-pool thread besides the
        # default one, and its interactive-pool listener runs the full
        # ladder; only an explicit JULIA_NUM_THREADS=1 (CI's test workers)
        # leaves no spare thread. Probe the child's total count directly.
        sendline("print(\"NTQ\", Threads.nthreads(:default) + Threads.nthreads(:interactive), \"QTN\")")
        @test timedwait(30.0; pollint=0.05) do
            m = match(r"NTQ(\d+)QTN", snapshot(), cursor[])
            m === nothing && return false
            cursor[] = m.offset + lastindex(m.match)
            return true
        end === :ok
        m = match(r"NTQ(\d+)QTN", snapshot())
        single_threaded = m !== nothing && m[1] == "1"
        expect("julia> ")
        # A task that acknowledges SAFE cancellation but hangs in its cleanup:
        # walks the full escalation ladder with a guided message per rung.
        # Two episodes: the second exercises the ladder on a *rescued*
        # session (fresh backend task, abandoned root task).
        for episode in 1:2
            sendline("println(\"EVAL-START\"); try; sleep(1000); finally; x = Ref(1.0); while x[] > 0; x[] = x[] * 1.0000001 + 0.1; end; end")
            expect("EVAL-START") # the evaluation is running (robust under load)
            sleep(0.5)           # ... and parked in sleep(1000)
            kill(p, Base.SIGINT) # press 1: SAFE, delivered silently
            expect("Press ^C again to also stop waiting for external resources"; timeout=6.0)
            if episode == 1
                # On-demand thread backtraces during the episode (^T sends
                # SIGINFO where the tty supports it - BSD/mac; SIGUSR1 elsewhere)
                kill(p, Sys.isbsd() ? Base.SIGINFO : Base.SIGUSR1)
                expect("signal ("; timeout=10.0) # the backtrace dump header
            end
            if single_threaded
                kill(p, Base.SIGINT) # press 2: retried, re-offering rung 1
                expect("Press ^C again to also stop waiting for external resources"; timeout=6.0)
                kill(p, Base.SIGINT) # press 3: C-side direct abandonment
                expect_all("Abandoning the current task", "CancellationRequest", "julia> ")
            else
                kill(p, Base.SIGINT) # press 2: ABANDON_EXTERNAL
                expect("No longer waiting for external resources")
                expect("Press ^C again to forcibly abandon"; timeout=6.0)
                kill(p, Base.SIGINT) # press 3: ABANDON_ALL freezes the task
                expect("Abandoning the current task")
                expect("CancellationRequest")
                expect("julia> ")
            end
            # the rescued session works
            sendline("$episode + $episode")
            expect(string(2episode))
            expect("julia> ")
            # The rescued backend closes the completed episode, standing the
            # escalation timer down: no stray warnings after the prompt
            # (regression test for an errant "failed to acknowledge" print
            # from the still-armed rescue timer of the final ^C press).
            sleep(1.5)
            @test !occursin("WARNING", snapshot()[cursor[]:end])
        end
        # ... and the session exits cleanly on ^D
        write(ptm, "\x04") # ^D (EOF)
        @test timedwait(() -> process_exited(p), 15.0) == :ok
        # A wedged session (e.g. an uninterrupted spin victim after ladder
        # failures) never exits: kill it rather than letting success(p) below
        # hang the whole suite.
        process_exited(p) || kill(p, Base.SIGKILL)
        @test success(p)
        close(ptm)
        wait(reader)
    end

    @testset "^C in the REPL (pty)" begin
        isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
        pts, ptm = Main.FakePTYs.open_fake_pty()

        # Interactive julia on the pty; drive it like a user pressing ^C.
        env = copy(ENV)
        env["TERM"] = "dumb"
        env["JULIA_HISTORY"] = tempname()
        p = run(detach(setenv(`$(Base.julia_cmd()) -i -q --startup-file=no --color=no`, env)),
                pts, pts, pts; wait=false)
        ccall(:close, Cint, (Cint,), pts) # only the child owns the pts now

        transcript_lock = ReentrantLock()
        transcript = UInt8[]
        reader = @async try
            while true
                chunk = readavailable(ptm)
                isempty(chunk) && break
                @lock transcript_lock append!(transcript, chunk)
            end
        catch # pty closes when the child exits
        end
        cursor = Ref(1)
        snapshot() = @lock transcript_lock String(copy(transcript))
        function expect(needle::String; timeout::Real=30.0)
            status = timedwait(timeout; pollint=0.05) do
                idx = findnext(needle, snapshot(), cursor[])
                idx === nothing && return false
                cursor[] = last(idx) + 1
                return true
            end
            if status !== :ok
                @error "expect timed out" needle tail=snapshot()[max(1, cursor[]):end]
            end
            @test status == :ok
        end
        sendline(s) = write(ptm, s * "\n")

        expect("julia> ")

        # a SIGINT at an idle prompt (^C or an external `kill -INT`) must
        # not disturb the session (issue #42072)
        kill(p, Base.SIGINT)
        sleep(0.5)
        sendline("20 + 21")
        expect("41")
        expect("julia> ")

        # ^C interrupts a sleeping REPL evaluation and reports it
        sendline("println(\"EVAL-1\"); sleep(1000)")
        expect("EVAL-1") # the evaluation is running (robust under load)
        sleep(0.5)       # ... and parked in sleep(1000)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")

        # the REPL evaluates normally afterwards
        sendline("6 * 7")
        expect("42")
        expect("julia> ")

        # a spinning evaluation triggers the escalation warning; the second
        # ^C abandons it and the REPL is rescued with a fresh backend
        sendline("println(\"EVAL-2\"); xr = Ref(1.0); while true; xr[] = xr[] * 1.0000001 + 0.1; end")
        expect("EVAL-2") # the evaluation is running (robust under load)
        sleep(0.5)       # ... and spinning
        kill(p, Base.SIGINT)
        expect("failed to acknowledge SIGINT"; timeout=15.0)
        kill(p, Base.SIGINT)
        expect("Abandoning the current task")
        expect("julia> ")

        # the rescued REPL still evaluates
        sendline("3 + 4")
        expect("7")
        expect("julia> ")

        # and ^C still works after the rescue
        sendline("println(\"EVAL-3\"); sleep(1000)")
        expect("EVAL-3")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")

        # a background task from an earlier evaluation belongs to an earlier
        # ^C epoch: interrupting the current evaluation leaves it running
        # (issue #25790)
        sendline("global bgc = Ref(0); global bg = @async while true; sleep(0.01); bgc[] += 1; end; println(\"BG-UP\")")
        expect("BG-UP")
        expect("julia> ")
        sendline("println(\"EVAL-4\"); sleep(1000)")
        expect("EVAL-4")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")
        sendline("print(\"bg-done=\", istaskdone(bg)); c0 = bgc[]; sleep(0.3); println(\"; bg-alive=\", bgc[] > c0)")
        expect("bg-done=false; bg-alive=true")
        expect("julia> ")

        # ^C during an in-evaluation terminal read recovers the prompt
        # (the class of issue #58105's "Install package?" prompt)
        sendline("println(\"EVAL-5\"); readline()")
        expect("EVAL-5")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")

        # ^C while parked in a server accept recovers, leaving the server
        # usable (the class of issue #58689)
        sendline("using Sockets; global srv = listen(Sockets.localhost, 0); println(\"LISTENING\"); accept(srv)")
        expect("LISTENING")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")
        sendline("println(\"srv-open=\", isopen(srv)); close(srv)")
        expect("srv-open=true")
        expect("julia> ")

        # cancelling a BigInt computation never yanks control out of libgmp
        # in an unsafe spot the way the old asynchronous InterruptException
        # delivery could (corrupting the heap - issue #56545): the loop is
        # deliberately checkless - delivery lands either on an MPZ entry
        # point's own cancellation point, asynchronously inside audited
        # libgmp compute (unwound via the reset region published across the
        # annotated call), or inside the allocation hooks (deferred and
        # chained into the reset on exit) - and BigInt arithmetic in the
        # session works correctly afterwards
        sendline("println(\"EVAL-6\"); let b = big(3); while true; b = b*b % (big(10)^200); end; end")
        expect("EVAL-6")
        sleep(0.5)
        kill(p, Base.SIGINT)
        expect("CancellationRequest")
        expect("julia> ")
        sendline("println(string(factorial(big(30))))")
        expect("265252859812191058636308480000000")
        expect("julia> ")

        sendline("exit()")
        # Never let success(p) hang the suite on a wedged session (see the
        # ladder testset's cleanup).
        if timedwait(() -> process_exited(p), 60.0) !== :ok
            kill(p, Base.SIGKILL)
        end
        @test success(p)
        close(ptm)
        wait(reader)
    end
end

@testset "cancelled condition waiter reacquiring a contended lock" begin
    # A cancelled `wait(::Threads.Condition)` must rethrow the
    # CancellationRequest after reacquiring the condition lock, even when
    # the reacquire is contended: lock parking uses its own Task link set,
    # so the waiter's stale (lazily collected) condition-queue entry must
    # not corrupt the lock's queue ("val already in a list").
    cond = Threads.Condition()
    src = Base.CancellationTokenSource()
    waiter_result = Channel{Any}(1)
    waiter = Threads.@spawn begin
        try
            Base.ScopedValues.with(Base.CANCEL_TOKEN => Base.CancellationToken(src)) do
                lock(cond)
                try
                    wait(cond)
                finally
                    unlock(cond)
                end
            end
            put!(waiter_result, :completed)
        catch e
            put!(waiter_result, e)
        end
    end
    # wait until the waiter is parked on the condition
    @test timedwait(10) do
        lock(cond)
        parked = !isempty(cond)
        unlock(cond)
        parked
    end === :ok
    # a holder keeps the condition lock while the cancellation is delivered
    held = Base.Event()
    release = Base.Event()
    holder = Threads.@spawn begin
        lock(cond)
        notify(held)
        wait(release; cancel=nothing)
        unlock(cond)
    end
    wait(held)
    Base.cancel!(src)
    # give the woken waiter time to reach the contended reacquire and park
    sleep(0.5)
    notify(release)
    v = fetch(waiter)
    result = take!(waiter_result)
    @test result isa Base.CancellationRequest
    # the condition lock must be intact and uncontended afterwards
    @test trylock(cond.lock)
    unlock(cond.lock)
    wait(holder)
end
