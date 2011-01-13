show(t::Task) = print("Task")

start(t::Task) = yieldto(t)
done(t::Task, val) = task_done(t)
next(t::Task, val) = (val, yieldto(t))

Runnable_Q = Queue()
Waiting_Set = idtable()

available(s::IOStream) =
    ccall(:jl_read_avail, Int32, (Ptr{Void},), s.ios) != 0

function schedule()
    global Runnable_Q
    global Waiting_Set

    while !(isempty(Runnable_Q) && isempty(Waiting_Set))
        task = false
        if !isempty(Runnable_Q)
            # run a runnable task
            task = pop(Runnable_Q)
            # invariant: current_task is not in Runnable_Q or Waiting_Set
            yieldto(task)
        end

        # make waiting tasks runnable where possible
        to_wake = Queue()
        for (t, ios) = Waiting_Set
            if available(ios)
                enq(to_wake, t)
                if !is(t, task)
                    enq(Runnable_Q, t)
                end
            end
        end
        for t = to_wake
            del(Waiting_Set, t)
        end

        # make the task we last ran runnable again, at the back of the queue
        if isa(task, Task)
            if !task_done(task) && !has(Waiting_Set, task)
                enq(Runnable_Q, task)
            end
        end
    end
end

yield() = yieldto(scheduler)

function io_wait(s::IOStream)
    Waiting_Set[current_task()] = s
    yield()
end

function spawn(thunk)
    t = Task(thunk)
    enq(Runnable_Q, t)
    t
end

function make_scheduled(t::Task)
    if !has(Waiting_Set, t)
        for x = Runnable_Q
            if is(x, t)
                return t
            end
        end
        enq(Runnable_Q, t)
    end
    t
end

function wait(t::Task)
    assert(!is(t,current_task()))
    make_scheduled(t)
    while !task_done(t)
        yield()
    end
    yieldto(t)  # return last value
end

# num bytes available without blocking
nb_available(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Void},), s.ios)

# non-blocking read
function nbread(s::IOStream, what)
    n = sizeof(what)
    if nb_available(s) < n
        # yield, asking the scheduler to return to us when data is available
        # on s.
        io_wait(s)
    end
    return read(s, what)
end
