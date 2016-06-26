.. currentmodule:: Base

******************************
 Tasks and Parallel Computing
******************************

Tasks
-----

.. function:: Task(func)

   .. Docstring generated from Julia source

   Create a ``Task`` (i.e. thread, or coroutine) to execute the given function (which must be callable with no arguments). The task exits when this function returns.

.. function:: yieldto(task, arg = nothing)

   .. Docstring generated from Julia source

   Switch to the given task. The first time a task is switched to, the task's function is called with no arguments. On subsequent switches, ``arg`` is returned from the task's last call to ``yieldto``\ . This is a low-level call that only switches tasks, not considering states or scheduling in any way. Its use is discouraged.

.. function:: current_task()

   .. Docstring generated from Julia source

   Get the currently running ``Task``\ .

.. function:: istaskdone(task) -> Bool

   .. Docstring generated from Julia source

   Tell whether a task has exited.

.. function:: istaskstarted(task) -> Bool

   .. Docstring generated from Julia source

   Tell whether a task has started executing.

.. function:: consume(task, values...)

   .. Docstring generated from Julia source

   Receive the next value passed to ``produce`` by the specified task. Additional arguments may be passed, to be returned from the last ``produce`` call in the producer.

.. function:: produce(value)

   .. Docstring generated from Julia source

   Send the given value to the last ``consume`` call, switching to the consumer task. If the next ``consume`` call passes any values, they are returned by ``produce``\ .

.. function:: yield()

   .. Docstring generated from Julia source

   Switch to the scheduler to allow another scheduled task to run. A task that calls this function is still runnable, and will be restarted immediately if there are no other runnable tasks.

.. function:: task_local_storage(symbol)

   .. Docstring generated from Julia source

   Look up the value of a symbol in the current task's task-local storage.

.. function:: task_local_storage(symbol, value)

   .. Docstring generated from Julia source

   Assign a value to a symbol in the current task's task-local storage.

.. function:: task_local_storage(body, symbol, value)

   .. Docstring generated from Julia source

   Call the function ``body`` with a modified task-local storage, in which ``value`` is assigned to ``symbol``\ ; the previous value of ``symbol``\ , or lack thereof, is restored afterwards. Useful for emulating dynamic scoping.

.. function:: Condition()

   .. Docstring generated from Julia source

   Create an edge-triggered event source that tasks can wait for. Tasks that call ``wait`` on a ``Condition`` are suspended and queued. Tasks are woken up when ``notify`` is later called on the ``Condition``\ . Edge triggering means that only tasks waiting at the time ``notify`` is called can be woken up. For level-triggered notifications, you must keep extra state to keep track of whether a notification has happened. The ``Channel`` type does this, and so can be used for level-triggered events.

.. function:: notify(condition, val=nothing; all=true, error=false)

   .. Docstring generated from Julia source

   Wake up tasks waiting for a condition, passing them ``val``\ . If ``all`` is ``true`` (the default), all waiting tasks are woken, otherwise only one is. If ``error`` is ``true``\ , the passed value is raised as an exception in the woken tasks.

.. function:: schedule(t::Task, [val]; error=false)

   .. Docstring generated from Julia source

   Add a task to the scheduler's queue. This causes the task to run constantly when the system is otherwise idle, unless the task performs a blocking operation such as ``wait``\ .

   If a second argument is provided, it will be passed to the task (via the return value of ``yieldto``\ ) when it runs again. If ``error`` is ``true``\ , the value is raised as an exception in the woken task.

.. function:: @schedule

   .. Docstring generated from Julia source

   Wrap an expression in a ``Task`` and add it to the local machine's scheduler queue.

.. function:: @task

   .. Docstring generated from Julia source

   Wrap an expression in a ``Task`` without executing it, and return the ``Task``\ . This only creates a task, and does not run it.

.. function:: sleep(seconds)

   .. Docstring generated from Julia source

   Block the current task for a specified number of seconds. The minimum sleep time is 1 millisecond or input of ``0.001``\ .

.. function:: ReentrantLock()

   .. Docstring generated from Julia source

   Creates a reentrant lock. The same task can acquire the lock as many times as required. Each lock must be matched with an unlock.

.. function:: lock(l::ReentrantLock)

   .. Docstring generated from Julia source

   Associates ``l`` with the current task. If ``l`` is already locked by a different task, waits for it to become available. The same task can acquire the lock multiple times. Each "lock" must be matched by an "unlock"

.. function:: unlock(l::ReentrantLock)

   .. Docstring generated from Julia source

   Releases ownership of the lock by the current task. If the lock had been acquired before, it just decrements an internal counter and returns immediately.

.. function:: Channel{T}(sz::Int)

   .. Docstring generated from Julia source

   Constructs a ``Channel`` that can hold a maximum of ``sz`` objects of type ``T``\ . ``put!`` calls on a full channel block till an object is removed with ``take!``\ .

   Other constructors:

   * ``Channel()`` - equivalent to ``Channel{Any}(32)``
   * ``Channel(sz::Int)`` equivalent to ``Channel{Any}(sz)``

General Parallel Computing Support
----------------------------------

.. function:: addprocs(n::Integer; exeflags=``) -> List of process identifiers

   .. Docstring generated from Julia source

   Launches workers using the in-built ``LocalManager`` which only launches workers on the local host. This can be used to take advantage of multiple cores. ``addprocs(4)`` will add 4 processes on the local machine.

.. function:: addprocs() -> List of process identifiers

   .. Docstring generated from Julia source

   Equivalent to ``addprocs(Sys.CPU_CORES)``

   Note that workers do not run a ``.juliarc.jl`` startup script, nor do they synchronize their global state (such as global variables, new method definitions, and loaded modules) with any of the other running processes.

.. function:: addprocs(machines; keyword_args...) -> List of process identifiers

   .. Docstring generated from Julia source

   Add processes on remote machines via SSH. Requires ``julia`` to be installed in the same location on each node, or to be available via a shared file system.

   ``machines`` is a vector of machine specifications.  Worker are started for each specification.

   A machine specification is either a string ``machine_spec`` or a tuple - ``(machine_spec, count)``\ .

   ``machine_spec`` is a string of the form ``[user@]host[:port] [bind_addr[:port]]``\ . ``user`` defaults to current user, ``port`` to the standard ssh port. If ``[bind_addr[:port]]`` is specified, other workers will connect to this worker at the specified ``bind_addr`` and ``port``\ .

   ``count`` is the number of workers to be launched on the specified host. If specified as ``:auto`` it will launch as many workers as the number of cores on the specific host.

   Keyword arguments:

   * ``tunnel``\ : if ``true`` then SSH tunneling will be used to connect to the worker from the             master process. Default is ``false``\ .

   * ``sshflags``\ : specifies additional ssh options, e.g.

   .. code-block:: julia

       sshflags=`-i /home/foo/bar.pem`

   * ``max_parallel``\ : specifies the maximum number of workers connected to in parallel at a host.                   Defaults to 10.

   * ``dir``\ : specifies the working directory on the workers. Defaults to the host's current          directory (as found by ``pwd()``\ )

   * ``exename``\ : name of the ``julia`` executable. Defaults to ``"$JULIA_HOME/julia"`` or              ``"$JULIA_HOME/julia-debug"`` as the case may be.

   * ``exeflags``\ : additional flags passed to the worker processes.

   * ``topology``\ : Specifies how the workers connect to each other. Sending a message             between unconnected workers results in an error.

   * ``topology=:all_to_all``  :  All processes are connected to each other.                       This is the default.

   * ``topology=:master_slave``  :  Only the driver process, i.e. pid 1 connects to the                         workers. The workers do not connect to each other.

   * ``topology=:custom``  :  The ``launch`` method of the cluster manager specifes the                   connection topology via fields ``ident`` and ``connect_idents`` in                   ``WorkerConfig``\ . A worker with a cluster manager identity ``ident``                   will connect to all workers specified in ``connect_idents``\ .

   Environment variables :

   If the master process fails to establish a connection with a newly launched worker within 60.0 seconds, the worker treats it a fatal situation and terminates. This timeout can be controlled via environment variable ``JULIA_WORKER_TIMEOUT``\ . The value of ``JULIA_WORKER_TIMEOUT`` on the master process, specifies the number of seconds a newly launched worker waits for connection establishment.

.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   .. Docstring generated from Julia source

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster manager implemented in package ``ClusterManagers``\ .

   The number of seconds a newly launched worker waits for connection establishment from the master can be specified via variable ``JULIA_WORKER_TIMEOUT`` in the worker process's environment. Relevant only when using TCP/IP as transport.

.. function:: nprocs()

   .. Docstring generated from Julia source

   Get the number of available processes.

.. function:: nworkers()

   .. Docstring generated from Julia source

   Get the number of available worker processes. This is one less than ``nprocs()``\ . Equal to ``nprocs()`` if ``nprocs() == 1``\ .

.. function:: procs()

   .. Docstring generated from Julia source

   Returns a list of all process identifiers.

.. function:: workers()

   .. Docstring generated from Julia source

   Returns a list of all worker process identifiers.

.. function:: rmprocs(pids...)

   .. Docstring generated from Julia source

   Removes the specified workers.

.. function:: interrupt([pids...])

   .. Docstring generated from Julia source

   Interrupt the current executing task on the specified workers. This is equivalent to pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.

.. function:: myid()

   .. Docstring generated from Julia source

   Get the id of the current process.

.. function:: asyncmap(f, c...) -> collection

   .. Docstring generated from Julia source

   Transform collection ``c`` by applying ``@async f`` to each element.

   For multiple collection arguments, apply f elementwise.

.. function:: pmap([::AbstractWorkerPool], f, c...; distributed=true, batch_size=1, on_error=nothing, retry_n=0, retry_max_delay=DEFAULT_RETRY_MAX_DELAY, retry_on=DEFAULT_RETRY_ON) -> collection

   .. Docstring generated from Julia source

   Transform collection ``c`` by applying ``f`` to each element using available workers and tasks.

   For multiple collection arguments, apply f elementwise.

   Note that ``f`` must be made available to all worker processes; see :ref:`Code Availability and Loading Packages <man-parallel-computing-code-availability>` for details.

   If a worker pool is not specified, all available workers, i.e., the default worker pool is used.

   By default, ``pmap`` distributes the computation over all specified workers. To use only the local process and distribute over tasks, specify ``distributed=false``\ . This is equivalent to ``asyncmap``\ .

   ``pmap`` can also use a mix of processes and tasks via the ``batch_size`` argument. For batch sizes greater than 1, the collection is split into multiple batches, which are distributed across workers. Each such batch is processed in parallel via tasks in each worker. The specified ``batch_size`` is an upper limit, the actual size of batches may be smaller and is calculated depending on the number of workers available and length of the collection.

   Any error stops pmap from processing the remainder of the collection. To override this behavior you can specify an error handling function via argument ``on_error`` which takes in a single argument, i.e., the exception. The function can stop the processing by rethrowing the error, or, to continue, return any value which is then returned inline with the results to the caller.

   Failed computation can also be retried via ``retry_on``\ , ``retry_n``\ , ``retry_max_delay``\ , which are passed through to ``retry`` as arguments ``retry_on``\ , ``n`` and ``max_delay`` respectively. If batching is specified, and an entire batch fails, all items in the batch are retried.

   The following are equivalent:

   * ``pmap(f, c; distributed=false)`` and ``asyncmap(f,c)``
   * ``pmap(f, c; retry_n=1)`` and ``asyncmap(retry(remote(f)),c)``
   * ``pmap(f, c; retry_n=1, on_error=e->e)`` and ``asyncmap(x->try retry(remote(f))(x) catch e; e end, c)``

.. function:: remotecall(func, id, args...; kwargs...)

   .. Docstring generated from Julia source

   Call a function asynchronously on the given arguments on the specified process. Returns a ``Future``\ . Keyword arguments, if any, are passed through to ``func``\ .

.. function:: Future()

   .. Docstring generated from Julia source

   Create a ``Future`` on the local machine.

.. function:: Future(n)

   .. Docstring generated from Julia source

   Create a ``Future`` on process ``n``\ .

.. function:: RemoteChannel()

   .. Docstring generated from Julia source

   Make an reference to a ``Channel{Any}(1)`` on the local machine.

.. function:: RemoteChannel(n)

   .. Docstring generated from Julia source

   Make an reference to a ``Channel{Any}(1)`` on process ``n``\ .

.. function:: RemoteChannel(f::Function, pid)

   .. Docstring generated from Julia source

   Create references to remote channels of a specific size and type. ``f()`` is a function that when executed on ``pid`` must return an implementation of an ``AbstractChannel``\ .

   For example, ``RemoteChannel(()->Channel{Int}(10), pid)``\ , will return a reference to a channel of type ``Int`` and size 10 on ``pid``\ .

.. function:: wait([x])

   .. Docstring generated from Julia source

   Block the current task until some event occurs, depending on the type of the argument:

   * ``RemoteChannel`` : Wait for a value to become available on the specified remote channel.
   * ``Future`` : Wait for a value to become available for the specified future.
   * ``Channel``\ : Wait for a value to be appended to the channel.
   * ``Condition``\ : Wait for ``notify`` on a condition.
   * ``Process``\ : Wait for a process or process chain to exit. The ``exitcode`` field of a process   can be used to determine success or failure.
   * ``Task``\ : Wait for a ``Task`` to finish, returning its result value. If the task fails with an   exception, the exception is propagated (re-thrown in the task that called ``wait``\ ).
   * ``RawFD``\ : Wait for changes on a file descriptor (see ``poll_fd`` for keyword arguments and return code)

   If no argument is passed, the task blocks for an undefined period. A task can only be restarted by an explicit call to ``schedule`` or ``yieldto``\ .

   Often ``wait`` is called within a ``while`` loop to ensure a waited-for condition is met before proceeding.

.. function:: fetch(x)

   .. Docstring generated from Julia source

   Waits and fetches a value from ``x`` depending on the type of ``x``\ . Does not remove the item fetched:

   * ``Future``\ : Wait for and get the value of a Future. The fetched value is cached locally.   Further calls to ``fetch`` on the same reference return the cached value. If the remote value   is an exception, throws a ``RemoteException`` which captures the remote exception and backtrace.
   * ``RemoteChannel``\ : Wait for and get the value of a remote reference. Exceptions raised are   same as for a ``Future`` .
   * ``Channel`` : Wait for and get the first available item from the channel.

.. function:: remotecall_wait(func, id, args...; kwargs...)

   .. Docstring generated from Julia source

   Perform ``wait(remotecall(...))`` in one message. Keyword arguments, if any, are passed through to ``func``\ .

.. function:: remotecall_fetch(func, id, args...; kwargs...)

   .. Docstring generated from Julia source

   Perform ``fetch(remotecall(...))`` in one message.  Keyword arguments, if any, are passed through to ``func``\ . Any remote exceptions are captured in a ``RemoteException`` and thrown.

.. function:: put!(RemoteChannel, value)

   .. Docstring generated from Julia source

   Store a value to the remote channel. If the channel is full, blocks until space is available. Returns its first argument.

.. function:: put!(Future, value)

   .. Docstring generated from Julia source

   Store a value to a future. Future's are write-once remote references. A ``put!`` on an already set ``Future`` throws an Exception. All asynchronous remote calls return ``Future``\ s and set the value to the return value of the call upon completion.

.. function:: put!(Channel, value)

   .. Docstring generated from Julia source

   Appends an item to the channel. Blocks if the channel is full.

.. function:: take!(RemoteChannel)

   .. Docstring generated from Julia source

   Fetch a value from a remote channel, also removing it in the processs.

.. function:: take!(Channel)

   .. Docstring generated from Julia source

   Removes and returns a value from a ``Channel``\ . Blocks till data is available.

.. function:: isready(r::RemoteChannel)

   .. Docstring generated from Julia source

   Determine whether a ``RemoteChannel`` has a value stored to it. Note that this function can cause race conditions, since by the time you receive its result it may no longer be true. However, it can be safely used on a ``Future`` since they are assigned only once.

.. function:: isready(r::Future)

   .. Docstring generated from Julia source

   Determine whether a ``Future`` has a value stored to it.

   If the argument ``Future`` is owned by a different node, this call will block to wait for the answer. It is recommended to wait for ``r`` in a separate task instead, or to use a local ``Channel`` as a proxy:

   .. code-block:: julia

       c = Channel(1)
       @async put!(c, remotecall_fetch(long_computation, p))
       isready(c)  # will not block

.. function:: close(Channel)

   .. Docstring generated from Julia source

   Closes a channel. An exception is thrown by:

   * ``put!`` on a closed channel.
   * ``take!`` and ``fetch`` on an empty, closed channel.

.. function:: WorkerPool(workers)

   .. Docstring generated from Julia source

   Create a WorkerPool from a vector of worker ids.

.. function:: CachingPool(workers::Vector{Int})

   .. Docstring generated from Julia source

   An implementation of an ``AbstractWorkerPool``\ . ``remote``\ , ``remotecall_fetch``\ , ``pmap`` and other remote calls which execute functions remotely, benefit from caching the serialized/deserialized functions on the worker nodes, especially for closures which capture large amounts of data.

   The remote cache is maintained for the lifetime of the returned ``CachingPool`` object. To clear the cache earlier, use ``clear!(pool)``\ .

   For global variables, only the bindings are captured in a closure, not the data. ``let`` blocks can be used to capture global data.

   For example:

   .. code-block:: julia

       const foo=rand(10^8);
       wp=CachingPool(workers())
       let foo=foo
           pmap(wp, i->sum(foo)+i, 1:100);
       end

   The above would transfer ``foo`` only once to each worker.

.. function:: default_worker_pool()

   .. Docstring generated from Julia source

   WorkerPool containing idle ``workers()`` (used by ``remote(f)``\ ).

.. function:: remote([::AbstractWorkerPool], f) -> Function

   .. Docstring generated from Julia source

   Returns a lambda that executes function ``f`` on an available worker using ``remotecall_fetch``\ .

.. function:: remotecall(f, pool::AbstractWorkerPool, args...; kwargs...)

   .. Docstring generated from Julia source

   Call ``f(args...; kwargs...)`` on one of the workers in ``pool``\ . Returns a ``Future``\ .

.. function:: remotecall_wait(f, pool::AbstractWorkerPool, args...; kwargs...)

   .. Docstring generated from Julia source

   Call ``f(args...; kwargs...)`` on one of the workers in ``pool``\ . Waits for completion, returns a ``Future``\ .

.. function:: remotecall_fetch(f, pool::AbstractWorkerPool, args...; kwargs...)

   .. Docstring generated from Julia source

   Call ``f(args...; kwargs...)`` on one of the workers in ``pool``\ . Waits for completion and returns the result.

.. function:: timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   .. Docstring generated from Julia source

   Waits till ``testcb`` returns ``true`` or for ``secs`` seconds, whichever is earlier. ``testcb`` is polled every ``pollint`` seconds.

.. function:: @spawn

   .. Docstring generated from Julia source

   Creates a closure around an expression and runs it on an automatically-chosen process, returning a ``Future`` to the result.

.. function:: @spawnat

   .. Docstring generated from Julia source

   Accepts two arguments, ``p`` and an expression. A closure is created around the expression and run asynchronously on process ``p``\ . Returns a ``Future`` to the result.

.. function:: @fetch

   .. Docstring generated from Julia source

   Equivalent to ``fetch(@spawn expr)``\ .

.. function:: @fetchfrom

   .. Docstring generated from Julia source

   Equivalent to ``fetch(@spawnat p expr)``\ .

.. function:: @async

   .. Docstring generated from Julia source

   Like ``@schedule``\ , ``@async`` wraps an expression in a ``Task`` and adds it to the local machine's scheduler queue. Additionally it adds the task to the set of items that the nearest enclosing ``@sync`` waits for. ``@async`` also wraps the expression in a ``let x=x, y=y, ...`` block to create a new scope with copies of all variables referenced in the expression.

.. function:: @sync

   .. Docstring generated from Julia source

   Wait until all dynamically-enclosed uses of ``@async``\ , ``@spawn``\ , ``@spawnat`` and ``@parallel`` are complete. All exceptions thrown by enclosed async operations are collected and thrown as a ``CompositeException``\ .

.. function:: @parallel

   .. Docstring generated from Julia source

   A parallel for loop of the form :

   .. code-block:: julia

       @parallel [reducer] for var = range
           body
       end

   The specified range is partitioned and locally executed across all workers. In case an optional reducer function is specified, ``@parallel`` performs local reductions on each worker with a final reduction on the calling process.

   Note that without a reducer function, ``@parallel`` executes asynchronously, i.e. it spawns independent tasks on all available workers and returns immediately without waiting for completion. To wait for completion, prefix the call with ``@sync``\ , like :

   .. code-block:: julia

       @sync @parallel for var = range
           body
       end

.. function:: @everywhere

   .. Docstring generated from Julia source

   Execute an expression on all processes. Errors on any of the processes are collected into a ``CompositeException`` and thrown. For example :

   .. code-block:: julia

       @everywhere bar=1

   will define ``bar`` under module ``Main`` on all processes.

   Unlike ``@spawn`` and ``@spawnat``\ , ``@everywhere`` does not capture any local variables. Prefixing ``@everywhere`` with ``@eval`` allows us to broadcast local variables using interpolation :

   .. code-block:: julia

       foo = 1
       @eval @everywhere bar=$foo

.. function:: clear!(pool::CachingPool) -> pool

   .. Docstring generated from Julia source

   Removes all cached functions from all participating workers.

.. function:: Base.remoteref_id(r::AbstractRemoteRef) -> (whence, id)

   .. Docstring generated from Julia source

   A low-level API which returns the unique identifying tuple for a remote reference. A reference id is a tuple of two elements - pid where the reference was created from and a one-up number from that node.

.. function:: Base.channel_from_id(refid) -> c

   .. Docstring generated from Julia source

   A low-level API which returns the backing AbstractChannel for an id returned by ``remoteref_id``\ . The call is valid only on the node where the backing channel exists.

.. function:: Base.worker_id_from_socket(s::IO) -> pid

   .. Docstring generated from Julia source

   A low-level API which given a ``IO`` connection, returns the pid of the worker it is connected to. This is useful when writing custom ``serialize`` methods for a type, which optimizes the data written out depending on the receiving process id.

.. function:: Base.cluster_cookie([cookie]) -> cookie

   .. Docstring generated from Julia source

   Returns the cluster cookie. If a cookie is passed, also sets it as the cluster cookie.

Shared Arrays
-------------

.. function:: SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

   .. Docstring generated from Julia source

   Construct a ``SharedArray`` of a bitstype ``T`` and size ``dims`` across the processes specified by ``pids`` - all of which have to be on the same host.

   If ``pids`` is left unspecified, the shared array will be mapped across all processes on the current host, including the master. But, ``localindexes`` and ``indexpids`` will only refer to worker processes. This facilitates work distribution code to use workers for actual computation with the master process acting as a driver.

   If an ``init`` function of the type ``initfn(S::SharedArray)`` is specified, it is called on all the participating workers.

.. function:: SharedArray(filename::AbstractString, T::Type, dims::NTuple, [offset=0]; mode=nothing, init=false, pids=Int[])

   .. Docstring generated from Julia source

   Construct a ``SharedArray`` backed by the file ``filename``\ , with element type ``T`` (must be a ``bitstype``\ ) and size ``dims``\ , across the processes specified by ``pids`` - all of which have to be on the same host. This file is mmapped into the host memory, with the following consequences:

   * The array data must be represented in binary format (e.g., an ASCII   format like CSV cannot be supported)

   * Any changes you make to the array values (e.g., ``A[3] = 0``\ ) will   also change the values on disk

   If ``pids`` is left unspecified, the shared array will be mapped across all processes on the current host, including the master. But, ``localindexes`` and ``indexpids`` will only refer to worker processes. This facilitates work distribution code to use workers for actual computation with the master process acting as a driver.

   ``mode`` must be one of ``"r"``\ , ``"r+"``\ , ``"w+"``\ , or ``"a+"``\ , and defaults to ``"r+"`` if the file specified by ``filename`` already exists, or ``"w+"`` if not. If an ``init`` function of the type ``initfn(S::SharedArray)`` is specified, it is called on all the participating workers. You cannot specify an ``init`` function if the file is not writable.

   ``offset`` allows you to skip the specified number of bytes at the beginning of the file.

.. function:: procs(S::SharedArray)

   .. Docstring generated from Julia source

   Get the vector of processes that have mapped the shared array.

.. function:: sdata(S::SharedArray)

   .. Docstring generated from Julia source

   Returns the actual ``Array`` object backing ``S``\ .

.. function:: indexpids(S::SharedArray)

   .. Docstring generated from Julia source

   Returns the index of the current worker into the ``pids`` vector, i.e., the list of workers mapping the SharedArray

.. function:: localindexes(S::SharedArray)

   .. Docstring generated from Julia source

   Returns a range describing the "default" indexes to be handled by the current process.  This range should be interpreted in the sense of linear indexing, i.e., as a sub-range of ``1:length(S)``\ .  In multi-process contexts, returns an empty range in the parent process (or any process for which ``indexpids`` returns 0).

   It's worth emphasizing that ``localindexes`` exists purely as a convenience, and you can partition work on the array among workers any way you wish.  For a SharedArray, all indexes should be equally fast for each worker process.

Multi-Threading
---------------

This experimental interface supports Julia's multi-threading
capabilities. Types and function described here might (and likely
will) change in the future.

.. function:: Threads.Atomic{T}

   .. Docstring generated from Julia source

   Holds a reference to an object of type ``T``\ , ensuring that it is only accessed atomically, i.e. in a thread-safe manner.

   Only certain "simple" types can be used atomically, namely the bitstypes integer and float-point types. These are ``Int8``\ ...``Int128``\ , ``UInt8``\ ...``UInt128``\ , and ``Float16``\ ...``Float64``\ .

   New atomic objects can be created from a non-atomic values; if none is specified, the atomic object is initialized with zero.

   Atomic objects can be accessed using the ``[]`` notation:

   .. code-block:: julia

       x::Atomic{Int}
       x[] = 1
       val = x[]

   Atomic operations use an ``atomic_`` prefix, such as ``atomic_add!``\ , ``atomic_xchg!``\ , etc.

.. function:: Threads.atomic_cas!{T}(x::Atomic{T}, cmp::T, newval::T)

   .. Docstring generated from Julia source

   Atomically compare-and-set ``x``

   Atomically compares the value in ``x`` with ``cmp``\ . If equal, write ``newval`` to ``x``\ . Otherwise, leaves ``x`` unmodified. Returns the old value in ``x``\ . By comparing the returned value to ``cmp`` (via ``===``\ ) one knows whether ``x`` was modified and now holds the new value ``newval``\ .

   For further details, see LLVM's ``cmpxchg`` instruction.

   This function can be used to implement transactional semantics. Before the transaction, one records the value in ``x``\ . After the transaction, the new value is stored only if ``x`` has not been modified in the mean time.

.. function:: Threads.atomic_xchg!{T}(x::Atomic{T}, newval::T)

   .. Docstring generated from Julia source

   Atomically exchange the value in ``x``

   Atomically exchanges the value in ``x`` with ``newval``\ . Returns the old value.

   For further details, see LLVM's ``atomicrmw xchg`` instruction.

.. function:: Threads.atomic_add!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically add ``val`` to ``x``

   Performs ``x[] += val`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw add`` instruction.

.. function:: Threads.atomic_sub!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically subtract ``val`` from ``x``

   Performs ``x[] -= val`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw sub`` instruction.

.. function:: Threads.atomic_and!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically bitwise-and ``x`` with ``val``

   Performs ``x[] &= val`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw and`` instruction.

.. function:: Threads.atomic_nand!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically bitwise-nand (not-and) ``x`` with ``val``

   Performs ``x[] = ~(x[] & val)`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw nand`` instruction.

.. function:: Threads.atomic_or!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically bitwise-or ``x`` with ``val``

   Performs ``x[] |= val`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw or`` instruction.

.. function:: Threads.atomic_xor!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically bitwise-xor (exclusive-or) ``x`` with ``val``

   Performs ``x[] $= val`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw xor`` instruction.

.. function:: Threads.atomic_max!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically store the maximum of ``x`` and ``val`` in ``x``

   Performs ``x[] = max(x[], val)`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw min`` instruction.

.. function:: Threads.atomic_min!{T}(x::Atomic{T}, val::T)

   .. Docstring generated from Julia source

   Atomically store the minimum of ``x`` and ``val`` in ``x``

   Performs ``x[] = min(x[], val)`` atomically. Returns the old (!) value.

   For further details, see LLVM's ``atomicrmw max`` instruction.

.. function:: Threads.atomic_fence()

   .. Docstring generated from Julia source

   Insert a sequential-consistency memory fence

   Inserts a memory fence with sequentially-consistent ordering semantics. There are algorithms where this is needed, i.e. where an acquire/release ordering is insufficient.

   This is likely a very expensive operation. Given that all other atomic operations in Julia already have acquire/release semantics, explicit fences should not be necessary in most cases.

   For further details, see LLVM's ``fence`` instruction.

Cluster Manager Interface
-------------------------

This interface provides a mechanism to launch and manage Julia workers on different cluster environments.
LocalManager, for launching additional workers on the same host and SSHManager, for launching on remote
hosts via ssh are present in Base. TCP/IP sockets are used to connect and transport messages
between processes. It is possible for Cluster Managers to provide a different transport.

.. function:: launch(manager::FooManager, params::Dict, launched::Vector{WorkerConfig}, launch_ntfy::Condition)

   .. Docstring generated from Julia source

   Implemented by cluster managers. For every Julia worker launched by this function, it should append a ``WorkerConfig`` entry to ``launched`` and notify ``launch_ntfy``\ . The function MUST exit once all workers, requested by ``manager`` have been launched. ``params`` is a dictionary of all keyword arguments ``addprocs`` was called with.

.. function:: manage(manager::FooManager, pid::Int, config::WorkerConfig. op::Symbol)

   .. Docstring generated from Julia source

   Implemented by cluster managers. It is called on the master process, during a worker's lifetime, with appropriate ``op`` values:

   * with ``:register``\ /``:deregister`` when a worker is added / removed from the Julia worker pool.
   * with ``:interrupt`` when ``interrupt(workers)`` is called. The :class:`ClusterManager`   should signal the appropriate worker with an interrupt signal.
   * with ``:finalize`` for cleanup purposes.

.. function:: kill(manager::FooManager, pid::Int, config::WorkerConfig)

   .. Docstring generated from Julia source

   Implemented by cluster managers. It is called on the master process, by ``rmprocs``\ . It should cause the remote worker specified by ``pid`` to exit. ``Base.kill(manager::ClusterManager.....)`` executes a remote ``exit()`` on ``pid``

.. function:: init_worker(manager::FooManager)

   .. Docstring generated from Julia source

   Called by cluster managers implementing custom transports. It initializes a newly launched process as a worker. Command line argument ``--worker`` has the effect of initializing a process as a worker using TCP/IP sockets for transport.

.. function:: connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

   .. Docstring generated from Julia source

   Implemented by cluster managers using custom transports. It should establish a logical connection to worker with id ``pid``\ , specified by ``config`` and return a pair of ``AsyncStream`` objects. Messages from ``pid`` to current process will be read off ``instrm``\ , while messages to be sent to ``pid`` will be written to ``outstrm``\ . The custom transport implementation must ensure that messages are delivered and received completely and in order. ``Base.connect(manager::ClusterManager.....)`` sets up TCP/IP socket connections in-between workers.

.. function:: Base.process_messages(instrm::AsyncStream, outstrm::AsyncStream)

   .. Docstring generated from Julia source

   Called by cluster managers using custom transports. It should be called when the custom transport implementation receives the first message from a remote worker. The custom transport must manage a logical connection to the remote worker and provide two ``AsyncStream`` objects, one for incoming messages and the other for messages addressed to the remote worker.

