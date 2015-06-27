.. currentmodule:: Base

******************************
 Tasks and Parallel Computing
******************************

Tasks
-----

.. function:: Task(func)

   Create a ``Task`` (i.e. thread, or coroutine) to execute the given function (which must be callable with no arguments). The task exits when this function returns.
   

.. function:: yieldto(task, arg = nothing)

   Switch to the given task. The first time a task is switched to, the task's function is called with no arguments. On subsequent switches, ``arg`` is returned from the task's last call to considering states or scheduling in any way. Its use is discouraged.
   

.. function:: current_task()

   Get the currently running Task.
   

.. function:: istaskdone(task) -> Bool

   Tell whether a task has exited.
   

.. function:: istaskstarted(task) -> Bool

   Tell whether a task has started executing.
   

.. function:: consume(task, values...)

   Receive the next value passed to ``produce`` by the specified task. Additional arguments may be passed, to be returned from the last
   

.. function:: produce(value)

   Send the given value to the last ``consume`` call, switching to the consumer task. If the next ``consume`` call passes any values, they are returned by ``produce``.
   

.. function:: yield()

   Switch to the scheduler to allow another scheduled task to run. A task that calls this function is still runnable, and will be restarted immediately if there are no other runnable tasks.
   

.. function:: task_local_storage(body, symbol, value)

   Call the function ``body`` with a modified task-local storage, in which ``value`` is assigned to ``symbol``; the previous value of emulating dynamic scoping.
   

.. function:: task_local_storage(body, symbol, value)

   Call the function ``body`` with a modified task-local storage, in which ``value`` is assigned to ``symbol``; the previous value of emulating dynamic scoping.
   

.. function:: task_local_storage(body, symbol, value)

   Call the function ``body`` with a modified task-local storage, in which ``value`` is assigned to ``symbol``; the previous value of emulating dynamic scoping.
   

.. function:: Condition()

   Create an edge-triggered event source that tasks can wait for. Tasks that call ``wait`` on a ``Condition`` are suspended and queued. Tasks are woken up when ``notify`` is later called on the time ``notify`` is called can be woken up. For level-triggered notifications, you must keep extra state to keep track of whether a notification has happened. The ``RemoteRef`` type does this, and so can be used for level-triggered events.
   

.. function:: notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them ``val``. If otherwise only one is. If ``error`` is true, the passed value is raised as an exception in the woken tasks.
   

.. function:: schedule(t::Task, [val]; error=false)

   Add a task to the scheduler's queue. This causes the task to run constantly when the system is otherwise idle, unless the task performs a blocking operation such as ``wait``. If a second argument is provided, it will be passed to the task task.
   

.. function:: @schedule()

   Wrap an expression in a Task and add it to the scheduler's queue.
   

.. function:: @task()

   Wrap an expression in a Task without executing it, and return the Task. This only creates a task, and does not run it.
   

.. function:: sleep(seconds)

   Block the current task for a specified number of seconds. The minimum sleep time is 1 millisecond or input of ``0.001``.
   

.. function:: ReentrantLock()

   Creates a reentrant lock. The same task can acquire the lock as many times as required. Each lock must be matched with an unlock.
   

.. function:: lock(l::ReentrantLock)

   Associates ``l`` with the current task. If ``l`` is already locked by a different task, waits for it to become available. The same task can acquire the lock multiple times. Each ``lock`` must be matched by an ``unlock``
   

.. function:: unlock(l::ReentrantLock)

   Releases ownership of the lock by the current task. If the lock had been acquired before, it just decrements an internal counter and returns immediately.
   

General Parallel Computing Support
----------------------------------

.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager. For example Beowulf clusters are  supported via a custom cluster manager implemented in  package ``ClusterManagers``. The number of seconds a newly launched worker waits for connection establishment from the master can be specified via variable Relevant only when using TCP/IP as transport.
   

.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager. For example Beowulf clusters are  supported via a custom cluster manager implemented in  package ``ClusterManagers``. The number of seconds a newly launched worker waits for connection establishment from the master can be specified via variable Relevant only when using TCP/IP as transport.
   

.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager. For example Beowulf clusters are  supported via a custom cluster manager implemented in  package ``ClusterManagers``. The number of seconds a newly launched worker waits for connection establishment from the master can be specified via variable Relevant only when using TCP/IP as transport.
   

.. function:: addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager. For example Beowulf clusters are  supported via a custom cluster manager implemented in  package ``ClusterManagers``. The number of seconds a newly launched worker waits for connection establishment from the master can be specified via variable Relevant only when using TCP/IP as transport.
   

.. function:: nprocs()

   Get the number of available processes.
   

.. function:: nworkers()

   Get the number of available worker processes. This is one less than nprocs(). Equal to nprocs() if nprocs() == 1.
   

.. function:: procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array
   

.. function:: workers()

   Returns a list of all worker process identifiers.
   

.. function:: rmprocs(pids...)

   Removes the specified workers.
   

.. function:: interrupt([pids...])

   Interrupt the current executing task on the specified workers. This is equivalent to pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.
   

.. function:: myid()

   Get the id of the current process.
   

.. function:: pmap(f, lsts...; err_retry=true, err_stop=false, pids=workers())

   Transform collections ``lsts`` by applying ``f`` to each element in parallel. If ``nprocs() > 1``, the calling process will be dedicated to assigning tasks. All other available processes will be used as parallel workers, or on the processes specified by If ``err_retry`` is true, it retries a failed application of ``f`` on a different worker. If ``err_stop`` is true, it takes precedence over the value of ``err_retry`` and ``pmap`` stops execution on the first error.
   

.. function:: remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the specified process. Returns a ``RemoteRef``.
   

.. function:: wait([x])

   Block the current task until some event occurs, depending on the type of the argument: If no argument is passed, the task blocks for an undefined period. If the task's state is set to ``:waiting``, it can only be restarted by an explicit call to ``schedule`` or ``yieldto``. If the task's state is ``:runnable``, it might be restarted unpredictably. Often ``wait`` is called within a ``while`` loop to ensure a waited-for condition is met before proceeding.
   

.. function:: fetch(RemoteRef)

   Wait for and get the value of a remote reference.
   

.. function:: remotecall_wait(id, func, args...)

   Perform ``wait(remotecall(...))`` in one message.
   

.. function:: remotecall_fetch(id, func, args...)

   Perform ``fetch(remotecall(...))`` in one message.
   

.. function:: put!(RemoteRef, value)

   Store a value to a remote reference. Implements ``shared queue of length 1`` semantics: if a value is already present, blocks until the value is removed with ``take!``. Returns its first argument.
   

.. function:: take!(RemoteRef)

   Fetch the value of a remote reference, removing it so that the reference is empty again.
   

.. function:: isready(r::RemoteRef)

   Determine whether a ``RemoteRef`` has a value stored to it. Note that this function can cause race conditions, since by the time you receive its result it may no longer be true. It is recommended that this function only be used on a ``RemoteRef`` that is assigned once. If the argument ``RemoteRef`` is owned by a different node, this call will block to wait for the answer. It is recommended to wait for ``r`` in a separate task instead, or to use a local
   

.. function:: RemoteRef(n)

   Make an uninitialized remote reference on process ``n``.
   

.. function:: RemoteRef(n)

   Make an uninitialized remote reference on process ``n``.
   

.. function:: timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till ``testcb`` returns ``true`` or for ``secs`` seconds, whichever is earlier. ``testcb`` is polled every ``pollint`` seconds.
   

.. function:: @spawn()

   Execute an expression on an automatically-chosen process, returning a ``RemoteRef`` to the result.
   

.. function:: @spawnat()

   Accepts two arguments, ``p`` and an expression, and runs the expression asynchronously on process ``p``, returning a
   

.. function:: @fetch()

   Equivalent to ``fetch(@spawn expr)``.
   

.. function:: @fetchfrom()

   Equivalent to ``fetch(@spawnat p expr)``.
   

.. function:: @async()

   Schedule an expression to run on the local machine, also adding it to the set of items that the nearest enclosing ``@sync`` waits for.
   

.. function:: @sync()

   Wait until all dynamically-enclosed uses of ``@async``, ``@spawn``,
   

.. function:: @parallel()

   A parallel for loop of the form The specified range is partitioned and locally executed across all workers. In case an optional reducer function is specified, reduction on the calling process. Note that without a reducer function, @parallel executes asynchronously, i.e. it spawns independent tasks on all available workers and returns immediately without waiting for completion. To wait for completion, prefix the call with ``@sync``, like
   

Shared Arrays (Experimental, UNIX-only feature)
-----------------------------------------------

.. function:: SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

   Construct a SharedArray of a bitstype ``T``  and size ``dims`` across the processes specified by ``pids`` - all of which have to be on the same host. If ``pids`` is left unspecified, the shared array will be mapped across all processes on the current host, including the master. But, ``localindexes`` and ``indexpids`` will only refer to worker processes. This facilitates work distribution code to use workers for actual computation with the master process acting as a driver. If an ``init`` function of the type ``initfn(S::SharedArray)`` is specified, it is called on all the participating workers.
   

.. function:: procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array
   

.. function:: sdata(S::SharedArray)

   Returns the actual ``Array`` object backing ``S``
   

.. function:: indexpids(S::SharedArray)

   Returns the index of the current worker into the ``pids`` vector, i.e., the list of workers mapping the SharedArray
   

Cluster Manager Interface
-------------------------
    This interface provides a mechanism to launch and manage Julia workers on different cluster environments.
    LocalManager, for launching additional workers on the same host and SSHManager, for launching on remote
    hosts via ssh are present in Base. TCP/IP sockets are used to connect and transport messages
    between processes. It is possible for Cluster Managers to provide a different transport.

.. function:: launch(manager::FooManager, params::Dict, launched::Vector{WorkerConfig}, launch_ntfy::Condition)

   Implemented by cluster managers. For every Julia worker launched by this function, it should append a ``WorkerConfig`` entry to once all workers, requested by ``manager`` have been launched. was called with.
   

.. function:: manage(manager::FooManager, pid::Int, config::WorkerConfig. op::Symbol)

   Implemented by cluster managers. It is called on the master process, during a worker's lifetime, with appropriate ``op`` values:
   

.. function:: kill(manager::FooManager, pid::Int, config::WorkerConfig)

   Implemented by cluster managers. It is called on the master process, by ``rmprocs``. It should cause the remote worker specified by ``pid`` to exit.
   

.. function:: init_worker(manager::FooManager)

   Called by cluster managers implementing custom transports. It initializes a newly launched process as a worker. Command line argument ``--worker`` has the effect of initializing a process as a worker using TCP/IP sockets for transport.
   

.. function:: connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

   Implemented by cluster managers using custom transports. It should establish a logical connection to worker with id ``pid``, specified by ``config`` and return a pair of ``AsyncStream`` objects. Messages from ``pid`` to current process will be read off messages are delivered and received completely and in order. socket connections in-between workers.
   

.. function:: Base.process_messages(instrm::AsyncStream, outstrm::AsyncStream)

    Called by cluster managers using custom transports. It should be called when the custom transport implementation receives the
    first message from a remote worker. The custom transport must manage a logical connection to the remote worker and provide two
    AsyncStream objects, one for incoming messages and the other for messages addressed to the remote worker.

