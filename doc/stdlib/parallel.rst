.. currentmodule:: Base

******************************
 Tasks and Parallel Computing
******************************

Tasks
-----

.. function:: Task(func)

   Create a ``Task`` (i.e. thread, or coroutine) to execute the given function (which must be callable with no arguments). The task exits when this function returns.

.. function:: yieldto(task, arg = nothing)

   Switch to the given task. The first time a task is switched to, the task's function is called with no arguments. On subsequent switches, ``arg`` is returned from the task's last call to ``yieldto``. This is a low-level call that only switches tasks, not considering states or scheduling in any way. Its use is discouraged.

.. function:: current_task()

   Get the currently running Task.

.. function:: istaskdone(task) -> Bool

   Tell whether a task has exited.

.. function:: istaskstarted(task) -> Bool

   Tell whether a task has started executing.

.. function:: consume(task, values...)

   Receive the next value passed to ``produce`` by the specified task.
   Additional arguments may be passed, to be returned from the last ``produce`` call
   in the producer.

.. function:: produce(value)

   Send the given value to the last ``consume`` call, switching to the consumer task.
   If the next ``consume`` call passes any values, they are returned by ``produce``.

.. function:: yield()

   Switch to the scheduler to allow another scheduled task to run. A task that calls this function is still runnable, and will be restarted immediately if there are no other runnable tasks.

.. function:: task_local_storage(symbol)

   Look up the value of a symbol in the current task's task-local storage.

.. function:: task_local_storage(symbol, value)

   Assign a value to a symbol in the current task's task-local storage.

.. function:: task_local_storage(body, symbol, value)

   Call the function ``body`` with a modified task-local storage, in which
   ``value`` is assigned to ``symbol``; the previous value of ``symbol``, or
   lack thereof, is restored afterwards. Useful for emulating dynamic scoping.

.. function:: Condition()

   Create an edge-triggered event source that tasks can wait for. Tasks
   that call ``wait`` on a ``Condition`` are suspended and queued.
   Tasks are woken up when ``notify`` is later called on the ``Condition``.
   Edge triggering means that only tasks waiting at the time ``notify`` is
   called can be woken up. For level-triggered notifications, you must
   keep extra state to keep track of whether a notification has happened.
   The ``Channel`` type does this, and so can be used for level-triggered
   events.

.. function:: notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them ``val``.
   If ``all`` is true (the default), all waiting tasks are woken, otherwise
   only one is. If ``error`` is true, the passed value is raised as an
   exception in the woken tasks.

.. function:: schedule(t::Task, [val]; error=false)

   Add a task to the scheduler's queue. This causes the task to run constantly
   when the system is otherwise idle, unless the task performs a blocking
   operation such as ``wait``.

   If a second argument is provided, it will be passed to the task (via the
   return value of ``yieldto``) when it runs again. If ``error`` is true,
   the value is raised as an exception in the woken task.

.. function:: @schedule

   Wrap an expression in a Task and add it to the scheduler's queue.

.. function:: @task

   Wrap an expression in a Task without executing it, and return the Task. This
   only creates a task, and does not run it.

.. function:: sleep(seconds)

   Block the current task for a specified number of seconds. The minimum sleep
   time is 1 millisecond or input of ``0.001``.

.. function:: ReentrantLock()

   Creates a reentrant lock. The same task can acquire the lock as many times
   as required. Each lock must be matched with an unlock.

.. function:: lock(l::ReentrantLock)

   Associates ``l`` with the current task. If ``l`` is already locked by a different
   task, waits for it to become available. The same task can acquire the lock multiple
   times. Each "lock" must be matched by an "unlock"

.. function:: unlock(l::ReentrantLock)

   Releases ownership of the lock by the current task. If the lock had been acquired before,
   it just decrements an internal counter and returns immediately.

.. function:: Channel(T::Type, sz::Int)

   Returns a ``Channel{T}`` of size ``sz``.

   Variants:

        Channel() returns a ``Channel{Any}`` of size typemax(Int).
        Channel(T::Type) returns a ``Channel{T}`` of size typemax(Int).
        Channel(sz::Int) returns a ``Channel{Any}`` of size ``sz``.

.. function:: ``open_channel(; pid::Int=myid(), T::Type=Any, sz::Int=typemax(Int))``

   Returns a ``ChannelRef`` which is a reference to a channel on process ``pid``,
   capable of holding a maximum number ``sz`` objects of type ``T``.


General Parallel Computing Support
----------------------------------

.. function:: addprocs(n::Integer; exeflags=``) -> List of process identifiers

   Launches workers using the in-built ``LocalManager`` which only launches workers on the local host.
   This can be used to take advantage of multiple cores. ``addprocs(4)`` will add 4 processes on the local machine.

.. function:: addprocs() -> List of process identifiers

    Equivalent to ``addprocs(CPU_CORES)``

.. function:: addprocs(machines; tunnel=false, sshflags=``, max_parallel=10, exeflags=``) -> List of process identifiers

   Add processes on remote machines via SSH.
   Requires julia to be installed in the same location on each node, or to be available via a shared file system.

   ``machines`` is a vector of machine specifications.  Worker are started for each specification.

   A machine specification is either a string ``machine_spec`` or a tuple - ``(machine_spec, count)``

   ``machine_spec`` is a string of the form ``[user@]host[:port] [bind_addr[:port]]``. ``user`` defaults
   to current user, ``port`` to the standard ssh port. If ``[bind_addr[:port]]`` is specified, other
   workers will connect to this worker at the specified ``bind_addr`` and ``port``.

   ``count`` is the number of workers to be launched on the specified host. If specified as ``:auto``
   it will launch as many workers as the number of cores on the specific host.


   Keyword arguments:

   ``tunnel`` : if ``true`` then SSH tunneling will be used to connect to the worker from the master process.

   ``sshflags`` : specifies additional ssh options, e.g. :literal:`sshflags=\`-i /home/foo/bar.pem\`` .

   ``max_parallel`` : specifies the maximum number of workers connected to in parallel at a host. Defaults to 10.

   ``dir`` :  specifies the working directory on the workers. Defaults to the host's current directory (as found by `pwd()`)

   ``exename`` :  name of the julia executable. Defaults to "$JULIA_HOME/julia" or "$JULIA_HOME/julia-debug" as the case may be.

   ``exeflags`` :  additional flags passed to the worker processes.

   Environment variables :

   If the master process fails to establish a connection with a newly launched worker within 60.0 seconds,
   the worker treats it a fatal situation and terminates. This timeout can be controlled via environment
   variable ``JULIA_WORKER_TIMEOUT``. The value of ``JULIA_WORKER_TIMEOUT`` on the master process, specifies
   the number of seconds a newly launched worker waits for connection establishment.


.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster manager implemented
   in  package ``ClusterManagers``.

   The number of seconds a newly launched worker waits for connection establishment from the master can be
   specified via variable ``JULIA_WORKER_TIMEOUT`` in the worker process's environment. Relevant only when
   using TCP/IP as transport.


.. function:: nprocs()

   Get the number of available processes.

.. function:: nworkers()

   Get the number of available worker processes. This is one less than nprocs(). Equal to nprocs() if nprocs() == 1.

.. function:: procs()

   Returns a list of all process identifiers.

.. function:: workers()

   Returns a list of all worker process identifiers.

.. function:: rmprocs(pids...)

   Removes the specified workers.

.. function:: interrupt([pids...])

   Interrupt the current executing task on the specified workers. This is
   equivalent to pressing Ctrl-C on the local machine. If no arguments are given,
   all workers are interrupted.

.. function:: myid()

   Get the id of the current process.

.. function:: pmap(f, lsts...; err_retry=true, err_stop=false, pids=workers())

   Transform collections ``lsts`` by applying ``f`` to each element in parallel.
   If ``nprocs() > 1``, the calling process will be dedicated to assigning tasks.
   All other available processes will be used as parallel workers, or on the processes specified by ``pids``.

   If ``err_retry`` is true, it retries a failed application of ``f`` on a different worker.
   If ``err_stop`` is true, it takes precedence over the value of ``err_retry`` and ``pmap`` stops execution on the first error.

.. function:: Future()

   Make an uninitialized Future on the local machine. It can be set via ``put!`` only once.

.. function:: Future(n)

   Make an uninitialized Future on process ``n``.

.. function:: wait([x])

   Block the current task until some event occurs, depending on the type
   of the argument:

   * ``Future``: Wait for a result to become available.

   * ``Channel``: Wait for a value to be appended to the channel.

   * ``ChannelRef``: Wait for a value to be appended to the remote channel. Note that for remote channels, it is
                    possible that a ``take!`` after a successful ``wait`` may still block,
                    since it is possible that another processes has taken a value between the ``wait``
                    and ``take!`` calls.

   * ``Condition``: Wait for ``notify`` on a condition.

   * ``Process``: Wait for a process or process chain to exit. The ``exitcode`` field of a process can be used to determine success or failure.

   * ``Task``: Wait for a ``Task`` to finish, returning its result value. If the task fails with an exception, the exception is propagated (re-thrown in the task that called ``wait``).

   * ``RawFD``: Wait for changes on a file descriptor (see `poll_fd` for keyword arguments and return code)

   If no argument is passed, the task blocks for an undefined period. If the task's
   state is set to ``:waiting``, it can only be restarted by an explicit call to
   ``schedule`` or ``yieldto``. If the task's state is ``:runnable``, it might be
   restarted unpredictably.

   Often ``wait`` is called within a ``while`` loop to ensure a waited-for condition
   is met before proceeding.

.. function:: fetch(x)

    Fetches a value from ``x`` depending on the type of ``x``. Blocks if no data is available.

    * ``Future`` : Wait for and get the value of a future result. The first fetch, caches the returned result locally.
                   Un-fetched, open Futures may be serialized to other processes.
                   Serializing an already fetched, or closed ``Future`` results in an error.

    * ``Channel`` : Fetches, but does not remove, the first available item from the channel.

    * ``ChannelRef`` : Fetches, but does not remove, the first available item from the remote channel.

.. function:: put!(x, val)
    Stores / appends a value depending on the type of ``x``.

    * ``Future`` : Store a value to a Future. Futures can be set only once.
                  A subsequent ``put!`` on the same Future object results in an error.

    * ``Channel`` : Append an item to the channel.

    * ``ChannelRef`` : Append an item to the remote channel.

.. function:: isready(x)

    Tests for the prescence of a value in ``x``, effectively determines if a ``fetch`` or a ``take!``
    would block:

   * ``Future`` : Determines if ``x`` has a value stored to it. If ``x`` is owned
                  by a different node, this call will block to wait for the answer. It is recommended to wait
                  for ``r`` in a separate task instead, or to use a local ``Future`` as a proxy::

                        rr = Future()
                        @async put!(rr, remotecall_fetch(p, long_computation))
                        isready(rr)  # will not block

   * ``Channel`` : Determines if the channel has at least one item.

   * ``ChannelRef`` : Determines if the remote channel has at least one item.

.. function:: take!(x)

    Removes and returns a value from a ``Channel`` or a ``ChannelRef``. Blocks till data is available.
    ``Future`` does not implement a ``take!``

.. function:: close(x)

    Depending on the type of ``x``, the state of the object is changed and optionally resources released.

    * ``Future`` : Optional call. Under normal circumstances the value referred to by a reference is freed
                   when either `fetch` is called or when it is garbage collected. Non-fetched Future's can
                   force the immediate release of the remote reference by calling ``close``.

    * ``Channel`` : Optional call on ``Channels``. Closing a channel will throw a ``InvalidStateException``
                    on subsequent ``put!`` calls. ``take!`` will succeed as long as there is data to be read.
                    A ``for`` loop iterating on a ``Channel`` will terminate once a ``Channel`` is closed
                    and all items processed.

    * ``ChannelRef`` : Releases and frees the remote channel. Must necessarily be called on remote channels
                        to free memory associated with it. Once a channel is freed, subsequent accesses throw a
                        ``InvalidStateException``.

.. function:: remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the specified process. Returns a ``Future``.

.. function:: remotecall_wait(id, func, args...)

   Perform ``wait(remotecall(...))`` in one message.

.. function:: remotecall_fetch(id, func, args...)

   Perform ``fetch(remotecall(...))`` in one message.

.. function:: timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till ``testcb`` returns ``true`` or for ``secs``` seconds, whichever is earlier.
   ``testcb`` is polled every ``pollint`` seconds.

.. function:: @spawn

   Execute an expression on an automatically-chosen process, returning a
   ``Future`` to the result.

.. function:: @spawnat

   Accepts two arguments, ``p`` and an expression, and runs the expression
   asynchronously on process ``p``, returning a ``Future`` to the result.

.. function:: @fetch

   Equivalent to ``fetch(@spawn expr)``.

.. function:: @fetchfrom

   Equivalent to ``fetch(@spawnat p expr)``.

.. function:: @async

   Schedule an expression to run on the local machine, also adding it to the
   set of items that the nearest enclosing ``@sync`` waits for.

.. function:: @sync

   Wait until all dynamically-enclosed uses of ``@async``, ``@spawn``,
   ``@spawnat`` and ``@parallel`` are complete.

.. function:: @parallel

   A parallel for loop of the form ::

        @parallel [reducer] for var = range
            body
        end

   The specified range is partitioned and locally executed across all workers.
   In case an optional reducer function is specified, @parallel performs local
   reductions on each worker with a final reduction on the calling process.

   Note that without a reducer function, @parallel executes asynchronously,
   i.e. it spawns independent tasks on all available workers and returns
   immediately without waiting for completion. To wait for completion, prefix
   the call with ``@sync``, like ::

        @sync @parallel for var = range
            body
        end

Shared Arrays (Experimental, UNIX-only feature)
-----------------------------------------------

.. function:: SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

    Construct a SharedArray of a bitstype ``T``  and size ``dims`` across the processes
    specified by ``pids`` - all of which have to be on the same host.

    If ``pids`` is left unspecified, the shared array will be mapped across all processes
    on the current host, including the master. But, ``localindexes`` and ``indexpids``
    will only refer to worker processes. This facilitates work distribution code to use
    workers for actual computation with the master process acting as a driver.

    If an ``init`` function of the type ``initfn(S::SharedArray)`` is specified,
    it is called on all the participating workers.

    SharedArray's need to be explictly freed via ``close``.

.. function:: procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array

.. function:: sdata(S::SharedArray)

   Returns the actual ``Array`` object backing ``S``

.. function:: indexpids(S::SharedArray)

   Returns the index of the current worker into the ``pids`` vector, i.e., the list of workers mapping
   the SharedArray

.. function:: close(S::SharedArray)

   SharedArray's need to be explictly closed. ``close`` frees all resources associated with the SharedArray and
   needs to be called once. It will typically be called from the driver process which created the array.


Cluster Manager Interface
-------------------------
    This interface provides a mechanism to launch and manage Julia workers on different cluster environments.
    LocalManager, for launching additional workers on the same host and SSHManager, for launching on remote
    hosts via ssh are present in Base. TCP/IP sockets are used to connect and transport messages
    between processes. It is possible for Cluster Managers to provide a different transport.

.. function:: launch(manager::FooManager, params::Dict, launched::Vector{WorkerConfig}, launch_ntfy::Condition)

    Implemented by cluster managers. For every Julia worker launched by this function, it should append a ``WorkerConfig`` entry
    to ``launched`` and notify ``launch_ntfy``. The function MUST exit once all workers, requested by ``manager`` have been launched.
    ``params`` is a dictionary of all keyword arguments ``addprocs`` was called with.

.. function:: manage(manager::FooManager, pid::Int, config::WorkerConfig. op::Symbol)

    Implemented by cluster managers. It is called on the master process, during a worker's lifetime,
    with appropriate ``op`` values:

      - with ``:register``/``:deregister`` when a worker is added / removed
        from the Julia worker pool.
      - with ``:interrupt`` when ``interrupt(workers)`` is called. The
        :class:`ClusterManager` should signal the appropriate worker with an
        interrupt signal.
      - with ``:finalize`` for cleanup purposes.

.. function:: kill(manager::FooManager, pid::Int, config::WorkerConfig)

    Implemented by cluster managers. It is called on the master process, by ``rmprocs``. It should cause the remote worker specified
    by ``pid`` to exit. ``Base.kill(manager::ClusterManager.....)`` executes a remote ``exit()`` on ``pid``

.. function:: init_worker(manager::FooManager)

    Called by cluster managers implementing custom transports. It initializes a newly launched process as a worker.
    Command line argument ``--worker`` has the effect of initializing a process as a worker using TCP/IP sockets
    for transport.

.. function:: connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

    Implemented by cluster managers using custom transports. It should establish a logical connection to worker with id ``pid``,
    specified by ``config`` and return a pair of ``AsyncStream`` objects. Messages from ``pid`` to current process will be read
    off ``instrm``, while messages to be sent to ``pid`` will be written to ``outstrm``. The custom transport implementation
    must ensure that messages are delivered and received completely and in order. ``Base.connect(manager::ClusterManager.....)``
    sets up TCP/IP socket connections in-between workers.


.. function:: Base.process_messages(instrm::AsyncStream, outstrm::AsyncStream)

    Called by cluster managers using custom transports. It should be called when the custom transport implementation receives the
    first message from a remote worker. The custom transport must manage a logical connection to the remote worker and provide two
    AsyncStream objects, one for incoming messages and the other for messages addressed to the remote worker.

