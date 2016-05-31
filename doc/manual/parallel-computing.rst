.. _man-parallel-computing:

.. currentmodule:: Base

********************
 Parallel Computing
********************

Most modern computers possess more than one CPU, and several computers
can be combined together in a cluster. Harnessing the power of these
multiple CPUs allows many computations to be completed more quickly.
There are two major factors that influence performance: the speed of the
CPUs themselves, and the speed of their access to memory. In a cluster,
it's fairly obvious that a given CPU will have fastest access to the RAM
within the same computer (node). Perhaps more surprisingly, similar
issues are relevant on a typical multicore laptop, due to
differences in the speed of main memory and the
`cache <http://www.akkadia.org/drepper/cpumemory.pdf>`_. Consequently, a
good multiprocessing environment should allow control over the
"ownership" of a chunk of memory by a particular CPU. Julia provides a
multiprocessing environment based on message passing to allow programs
to run on multiple processes in separate memory domains at once.

Julia's implementation of message passing is different from other
environments such as MPI [#mpi2rma]_. Communication in Julia is generally
"one-sided", meaning that the programmer needs to explicitly manage only
one process in a two-process operation. Furthermore, these
operations typically do not look like "message send" and "message
receive" but rather resemble higher-level operations like calls to user
functions.

Parallel programming in Julia is built on two primitives: *remote
references* and *remote calls*. A remote reference is an object that can
be used from any process to refer to an object stored on a particular
process. A remote call is a request by one process to call a certain
function on certain arguments on another (possibly the same) process.

Remote references come in two flavors -``Future`` and ``RemoteChannel``.

A remote call returns a ``Future`` to its result. Remote calls
return immediately; the process that made the call proceeds to its
next operation while the remote call happens somewhere else. You can
wait for a remote call to finish by calling :func:`wait` on the returned
`Future`, and you can obtain the full value of the result using
:func:`fetch`.

On the other hand ``RemoteChannel`` s are rewritable. For example, multiple processes
can co-ordinate their processing by referencing the same remote ``Channel``\ .

Let's try this out. Starting with ``julia -p n`` provides ``n`` worker
processes on the local machine. Generally it makes sense for ``n`` to
equal the number of CPU cores on the machine.

::

    $ ./julia -p 2

    julia> r = remotecall(rand, 2, 2, 2)
    Future(2,1,3,Nullable{Any}())

    julia> s = @spawnat 2 1 .+ fetch(r)
    Future(2,1,6,Nullable{Any}())

    julia> fetch(s)
    2×2 Array{Float64,2}:
     1.60401  1.50111
     1.17457  1.15741

The first argument to :func:`remotecall` is the function to call.
Most parallel programming in Julia does not reference specific processes
or the number of processes available, but :func:`remotecall` is
considered a low-level interface providing finer control. The second
argument to :func:`remotecall` is the index of the process
that will do the work, and the remaining arguments will be passed
to the function being called.

As you can see, in the first line we asked process 2 to
construct a 2-by-2 random matrix, and in the second line we asked it
to add 1 to it. The result of both calculations is available in the
two futures, ``r`` and ``s``. The :obj:`@spawnat` macro
evaluates the expression in the second argument on the process
specified by the first argument.

Occasionally you might want a remotely-computed value immediately. This
typically happens when you read from a remote object to obtain data
needed by the next local operation. The function :func:`remotecall_fetch`
exists for this purpose. It is equivalent to ``fetch(remotecall(...))``
but is more efficient.

::

    julia> remotecall_fetch(getindex, 2, r, 1, 1)
    0.10824216411304866

Remember that :func:`getindex(r,1,1) <getindex>` is :ref:`equivalent <man-array-indexing>` to
``r[1,1]``, so this call fetches the first element of the future ``r``.

The syntax of :func:`remotecall` is not especially convenient. The macro
:obj:`@spawn` makes things easier. It operates on an expression rather than
a function, and picks where to do the operation for you::

    julia> r = @spawn rand(2,2)
    Future(2,1,4,Nullable{Any}())

    julia> s = @spawn 1 .+ fetch(r)
    Future(3,1,5,Nullable{Any}())

    julia> fetch(s)
    1.10824216411304866 1.13798233877923116
    1.12376292706355074 1.18750497916607167

Note that we used ``1 .+ fetch(r)`` instead of ``1 .+ r``. This is because we
do not know where the code will run, so in general a :func:`fetch` might be
required to move ``r`` to the process doing the addition. In this
case, :obj:`@spawn` is smart enough to perform the computation on the
process that owns ``r``, so the :func:`fetch` will be a no-op.

(It is worth noting that :obj:`@spawn` is not built-in but defined in Julia
as a :ref:`macro <man-macros>`. It is possible to define your
own such constructs.)

An important thing to remember is that, once fetched, a ``Future`` will cache its value
locally. Further ``fetch`` calls do not entail a network hop. Once all referencing Futures
have fetched, the remote stored value is deleted.


.. _man-parallel-computing-code-availability:

Code Availability and Loading Packages
--------------------------------------

Your code must be available on any process that runs it. For example,
type the following into the Julia prompt::

    julia> function rand2(dims...)
             return 2*rand(dims...)
           end

    julia> rand2(2,2)
    2×2 Array{Float64,2}:
     0.153756  0.368514
     1.15119   0.918912

    julia> fetch(@spawn rand2(2,2))
    ERROR: On worker 2:
    function rand2 not defined on process 2


Process 1 knew about the function ``rand2``, but process 2 did not.

Most commonly you'll be loading code from files or packages, and you
have a considerable amount of flexibility in controlling which
processes load code.  Consider a file, ``"DummyModule.jl"``, containing
the following code::

    module DummyModule

    export MyType, f

    type MyType
        a::Int
    end

    f(x) = x^2+1

    println("loaded")

    end

Starting julia with ``julia -p 2``, you can use this to verify the following:

- :func:`include("DummyModule.jl") <include>` loads the file on just a single process (whichever one executes the statement).
- ``using DummyModule`` causes the module to be loaded on all processes; however, the module is brought into scope only on the one executing the statement.
- As long as ``DummyModule`` is loaded on process 2, commands like ::

    rr = RemoteChannel(2)
    put!(rr, MyType(7))

  allow you to store an object of type ``MyType`` on process 2 even if ``DummyModule`` is not in scope on process 2.

You can force a command to run on all processes using the :obj:`@everywhere` macro.
For example, :obj:`@everywhere` can also be used to directly define a function on all processes::

    julia> @everywhere id = myid()

    julia> remotecall_fetch(()->id, 2)
    2

A file can also be preloaded on multiple processes at startup, and a driver script can be used to drive the computation::

    julia -p <n> -L file1.jl -L file2.jl driver.jl

Each process has an associated identifier. The process providing the interactive Julia prompt
always has an id equal to 1, as would the Julia process running the driver script in the
example above.
The processes used by default for parallel operations are referred to as "workers".
When there is only one process, process 1 is considered a worker. Otherwise, workers are
considered to be all processes other than process 1.

The base Julia installation has in-built support for two types of clusters:

- A local cluster specified with the ``-p`` option as shown above.

- A cluster spanning machines using the ``--machinefile`` option. This uses a passwordless
  ``ssh`` login to start julia worker processes (from the same path as the current host)
  on the specified machines.

Functions :func:`addprocs`, :func:`rmprocs`, :func:`workers`, and others are available as a programmatic means of
adding, removing and querying the processes in a cluster.

Note that workers do not run a ``.juliarc.jl`` startup script, nor do they synchronize their global state
(such as global variables, new method definitions, and loaded modules) with any of the other running processes.

Other types of clusters can be supported by writing your own custom
:class:`ClusterManager`, as described below in the :ref:`man-clustermanagers`
section.

Data Movement
-------------

Sending messages and moving data constitute most of the overhead in a
parallel program. Reducing the number of messages and the amount of data
sent is critical to achieving performance and scalability. To this end,
it is important to understand the data movement performed by Julia's
various parallel programming constructs.

:func:`fetch` can be considered an explicit data movement operation, since
it directly asks that an object be moved to the local machine.
:obj:`@spawn` (and a few related constructs) also moves data, but this is
not as obvious, hence it can be called an implicit data movement
operation. Consider these two approaches to constructing and squaring a
random matrix::

    # method 1
    A = rand(1000,1000)
    Bref = @spawn A^2
    ...
    fetch(Bref)

    # method 2
    Bref = @spawn rand(1000,1000)^2
    ...
    fetch(Bref)

The difference seems trivial, but in fact is quite significant due to
the behavior of :obj:`@spawn`. In the first method, a random matrix is
constructed locally, then sent to another process where it is squared.
In the second method, a random matrix is both constructed and squared on
another process. Therefore the second method sends much less data than
the first.

In this toy example, the two methods are easy to distinguish and choose
from. However, in a real program designing data movement might require
more thought and likely some measurement. For example, if the first
process needs matrix ``A`` then the first method might be better. Or,
if computing ``A`` is expensive and only the current process has it,
then moving it to another process might be unavoidable. Or, if the
current process has very little to do between the :obj:`@spawn` and
``fetch(Bref)`` then it might be better to eliminate the parallelism
altogether. Or imagine ``rand(1000,1000)`` is replaced with a more
expensive operation. Then it might make sense to add another :obj:`@spawn`
statement just for this step.

Parallel Map and Loops
----------------------

Fortunately, many useful parallel computations do not require data
movement. A common example is a Monte Carlo simulation, where multiple
processes can handle independent simulation trials simultaneously. We
can use :obj:`@spawn` to flip coins on two processes. First, write the
following function in ``count_heads.jl``::

    function count_heads(n)
        c::Int = 0
        for i=1:n
            c += rand(Bool)
        end
        c
    end

The function ``count_heads`` simply adds together ``n`` random bits.
Here is how we can perform some trials on two machines, and add together the
results::

    require("count_heads")

    a = @spawn count_heads(100000000)
    b = @spawn count_heads(100000000)
    fetch(a)+fetch(b)

This example demonstrates a powerful and often-used
parallel programming pattern. Many iterations run independently over
several processes, and then their results are combined using some
function. The combination process is called a *reduction*, since it is
generally tensor-rank-reducing: a vector of numbers is reduced to a
single number, or a matrix is reduced to a single row or column, etc. In
code, this typically looks like the pattern ``x = f(x,v[i])``, where
``x`` is the accumulator, ``f`` is the reduction function, and the
``v[i]`` are the elements being reduced. It is desirable for ``f`` to be
associative, so that it does not matter what order the operations are
performed in.

Notice that our use of this pattern with ``count_heads`` can be
generalized. We used two explicit :obj:`@spawn` statements, which limits
the parallelism to two processes. To run on any number of processes,
we can use a *parallel for loop*, which can be written in Julia like
this::

    nheads = @parallel (+) for i=1:200000000
      Int(rand(Bool))
    end

This construct implements the pattern of assigning iterations to
multiple processes, and combining them with a specified reduction (in
this case ``(+)``). The result of each iteration is taken as the value
of the last expression inside the loop. The whole parallel loop
expression itself evaluates to the final answer.

Note that although parallel for loops look like serial for loops, their
behavior is dramatically different. In particular, the iterations do not
happen in a specified order, and writes to variables or arrays will not
be globally visible since iterations run on different processes. Any
variables used inside the parallel loop will be copied and broadcast to
each process.

For example, the following code will not work as intended::

    a = zeros(100000)
    @parallel for i=1:100000
      a[i] = i
    end

However, this code will not initialize all of ``a``, since each
process will have a separate copy of it. Parallel for loops like these
must be avoided. Fortunately,  `Shared Arrays <#shared-arrays>`_
can be used to get around this limitation::

    a = SharedArray(Float64,10)
    @parallel for i=1:10
      a[i] = i
    end

Using "outside" variables in parallel loops is perfectly reasonable if
the variables are read-only::

    a = randn(1000)
    @parallel (+) for i=1:100000
      f(a[rand(1:end)])
    end

Here each iteration applies ``f`` to a randomly-chosen sample from a
vector ``a`` shared by all processes.

As you could see, the reduction operator can be omitted if it is not needed.
In that case, the loop executes asynchronously, i.e. it spawns independent
tasks on all available workers and returns an array of :class:`Future`
immediately without waiting for completion.
The caller can wait for the :class:`Future` completions at a later
point by calling :func:`fetch` on them, or wait for completion at the end of the
loop by prefixing it with :obj:`@sync`, like ``@sync @parallel for``.

In some cases no reduction operator is needed, and we merely wish to
apply a function to all integers in some range (or, more generally, to
all elements in some collection). This is another useful operation
called *parallel map*, implemented in Julia as the :func:`pmap` function.
For example, we could compute the singular values of several large
random matrices in parallel as follows::

    M = Matrix{Float64}[rand(1000,1000) for i=1:10]
    pmap(svd, M)

Julia's :func:`pmap` is designed for the case where each function call does
a large amount of work. In contrast, ``@parallel for`` can handle
situations where each iteration is tiny, perhaps merely summing two
numbers. Only worker processes are used by both :func:`pmap` and ``@parallel for``
for the parallel computation. In case of ``@parallel for``, the final reduction
is done on the calling process.



Synchronization With Remote References
--------------------------------------

Scheduling
----------

Julia's parallel programming platform uses
:ref:`man-tasks` to switch among
multiple computations. Whenever code performs a communication operation
like :func:`fetch` or :func:`wait`, the current task is suspended and a
scheduler picks another task to run. A task is restarted when the event
it is waiting for completes.

For many problems, it is not necessary to think about tasks directly.
However, they can be used to wait for multiple events at the same time,
which provides for *dynamic scheduling*. In dynamic scheduling, a
program decides what to compute or where to compute it based on when
other jobs finish. This is needed for unpredictable or unbalanced
workloads, where we want to assign more work to processes only when
they finish their current tasks.

As an example, consider computing the singular values of matrices of
different sizes::

    M = Matrix{Float64}[rand(800,800), rand(600,600), rand(800,800), rand(600,600)]
    pmap(svd, M)

If one process handles both 800×800 matrices and another handles both
600×600 matrices, we will not get as much scalability as we could. The
solution is to make a local task to "feed" work to each process when
it completes its current task. For example, consider a simple :func:`pmap`
implementation::

    function pmap(f, lst)
        np = nprocs()  # determine the number of processes available
        n = length(lst)
        results = Vector{Any}(n)
        i = 1
        # function to produce the next work item from the queue.
        # in this case it's just an index.
        nextidx() = (idx=i; i+=1; idx)
        @sync begin
            for p=1:np
                if p != myid() || np == 1
                    @async begin
                        while true
                            idx = nextidx()
                            if idx > n
                                break
                            end
                            results[idx] = remotecall_fetch(f, p, lst[idx])
                        end
                    end
                end
            end
        end
        results
    end

:obj:`@async` is similar to :obj:`@spawn`, but only runs tasks on the
local process. We use it to create a "feeder" task for each process.
Each task picks the next index that needs to be computed, then waits for
its process to finish, then repeats until we run out of indexes. Note
that the feeder tasks do not begin to execute until the main task
reaches the end of the :obj:`@sync` block, at which point it surrenders
control and waits for all the local tasks to complete before returning
from the function. The feeder tasks are able to share state via
:func:`nextidx` because they all run on the same process. No locking is
required, since the threads are scheduled cooperatively and not
preemptively. This means context switches only occur at well-defined
points: in this case, when :func:`remotecall_fetch` is called.


Channels
--------
Channels provide for a fast means of inter-task communication. A
``Channel{T}(n::Int)`` is a shared queue of maximum length ``n``
holding objects of type ``T``. Multiple readers can read off the channel
via ``fetch`` and ``take!``. Multiple writers can add to the channel via
``put!``. ``isready`` tests for the presence of any object in
the channel, while ``wait`` waits for an object to become available.
``close`` closes a Channel. On a closed channel, ``put!`` will fail,
while ``take!`` and ``fetch`` successfully return any existing values
till it is emptied.

A Channel can be used as an iterable object in a ``for`` loop, in which
case the loop runs as long as the channel has data or is open. The loop
variable takes on all values added to the channel. An empty, closed channel
causes the ``for`` loop to terminate.


Remote references and AbstractChannels
--------------------------------------

Remote references always refer to an implementation of an ``AbstractChannel``

A concrete implementation of an ``AbstractChannel`` (like ``Channel``), is required
to implement ``put!``\ , ``take!``\ , ``fetch``\ , ``isready`` and ``wait``\ . The remote object
referred to by a ``Future`` is stored in a ``Channel{Any}(1)``\ , i.e., a channel of size 1
capable of holding objects of ``Any`` type.

``RemoteChannel``\ , which is rewritable, can point to any type and size of channels, or any other
implementation of an ``AbstractChannel``\ .

The constructor ``RemoteChannel(f::Function, pid)`` allows us to construct references to channels holding
more than one value of a specific type. ``f()`` is a function executed on ``pid`` and it must return
an ``AbstractChannel``\ .

For example, ``RemoteChannel(()->Channel{Int}(10), pid)``\ , will return a reference to a channel of type ``Int``
and size 10. The channel exists on worker ``pid``\ .

Methods ``put!``\ , ``take!``\ , ``fetch``\ , ``isready`` and ``wait`` on a ``RemoteChannel`` are proxied onto
the backing store on the remote process.

``RemoteChannel`` can thus be used to refer to user implemented ``AbstractChannel`` objects. A simple
example of this is provided in ``examples/dictchannel.jl`` which uses a dictionary as its remote store.


Remote References and Distributed Garbage Collection
----------------------------------------------------

Objects referred to by remote references can be freed only when *all* held references in the cluster
are deleted.

The node where the value is stored keeps track of which of the workers have a reference to it.
Every time a ``RemoteChannel`` or a (unfetched) ``Future`` is serialized to a worker, the node pointed
to by the reference is notified. And every time a ``RemoteChannel`` or a (unfetched) ``Future``
is garbage collected locally, the node owning the value is again notified.

The notifications are done via sending of "tracking" messages - an "add reference" message when
a reference is serialized to a different process and a "delete reference" message when a reference
is locally garbage collected.

Since ``Future``\ s are write-once and cached locally, the act of ``fetch``\ ing a ``Future`` also updates
reference tracking information on the node owning the value.

The node which owns the value frees it once all references to it are cleared.

With ``Future``\ s, serializing an already fetched ``Future`` to a different node also sends the value
since the original remote store may have collected the value by this time.

It is important to note that *when* an object is locally garbage collected depends
on the size of the object and the current memory pressure in the system.

In case of remote references, the size of the local reference object is quite small, while the value
stored on the remote node may be quite large. Since the local object may not be collected immediately, it is
a good practice to explicitly call ``finalize`` on local instances of a ``RemoteChannel``, or on unfetched
``Future``\ s. Since calling ``fetch`` on a ``Future`` also removes its reference from the remote store, this
is not required on fetched ``Future``\ s. Explicitly calling ``finalize`` results in an immediate message sent to
the remote node to go ahead and remove its reference to the value.

Once finalized, a reference becomes invalid and cannot be used in any further calls.

Shared Arrays
-------------

Shared Arrays use system shared memory to map the same array across
many processes.  While there are some similarities to a `DArray`_,
the behavior of a :class:`SharedArray` is quite different. In a `DArray`_,
each process has local access to just a chunk of the data, and no two
processes share the same chunk; in contrast, in a :class:`SharedArray` each
"participating" process has access to the entire array.  A
:class:`SharedArray` is a good choice when you want to have a large amount
of data jointly accessible to two or more processes on the same machine.

:class:`SharedArray` indexing (assignment and accessing values) works just
as with regular arrays, and is efficient because the underlying memory
is available to the local process.  Therefore, most algorithms work
naturally on :class:`SharedArray`\ s, albeit in single-process mode.  In
cases where an algorithm insists on an :class:`Array` input, the underlying
array can be retrieved from a :class:`SharedArray` by calling :func:`sdata`.
For other :class:`AbstractArray` types, ``sdata`` just returns the object
itself, so it's safe to use :func:`sdata` on any Array-type object.

The constructor for a shared array is of the form::

  SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

which creates a shared array of a bitstype ``T`` and size ``dims``
across the processes specified by ``pids``.  Unlike distributed
arrays, a shared array is accessible only from those participating
workers specified by the ``pids`` named argument (and the creating
process too, if it is on the same host).

If an ``init`` function, of signature ``initfn(S::SharedArray)``, is
specified, it is called on all the participating workers.  You can
arrange it so that each worker runs the ``init`` function on a
distinct portion of the array, thereby parallelizing initialization.

Here's a brief example:

.. doctest::

  julia> addprocs(3)
  3-element Array{Int64,1}:
   2
   3
   4

  julia> S = SharedArray(Int, (3,4), init = S -> S[Base.localindexes(S)] = myid())
  3×4 SharedArray{Int64,2}:
   2  2  3  4
   2  3  3  4
   2  3  4  4

  julia> S[3,2] = 7
  7

  julia> S
  3×4 SharedArray{Int64,2}:
   2  2  3  4
   2  3  3  4
   2  7  4  4

:func:`Base.localindexes` provides disjoint one-dimensional ranges of indexes,
and is sometimes convenient for splitting up tasks among processes.
You can, of course, divide the work any way you wish:

.. doctest::

  julia> S = SharedArray(Int, (3,4), init = S -> S[indexpids(S):length(procs(S)):length(S)] = myid())
  3×4 SharedArray{Int64,2}:
   2  2  2  2
   3  3  3  3
   4  4  4  4

Since all processes have access to the underlying data, you do have to
be careful not to set up conflicts.  For example::

  @sync begin
      for p in procs(S)
          @async begin
              remotecall_wait(fill!, p, S, p)
          end
      end
  end

would result in undefined behavior: because each process fills the
*entire* array with its own ``pid``, whichever process is the last to
execute (for any particular element of ``S``) will have its ``pid``
retained.

As a more extended and complex example, consider running the following
"kernel" in parallel::

    q[i,j,t+1] = q[i,j,t] + u[i,j,t]

In this case, if we try to split up the work using a one-dimensional
index, we are likely to run into trouble: if ``q[i,j,t]`` is near the
end of the block assigned to one worker and ``q[i,j,t+1]`` is near the
beginning of the block assigned to another, it's very likely that
``q[i,j,t]`` will not be ready at the time it's needed for computing
``q[i,j,t+1]``.  In such cases, one is better off chunking the array
manually.  Let's split along the second dimension::

   # This function retuns the (irange,jrange) indexes assigned to this worker
   @everywhere function myrange(q::SharedArray)
       idx = indexpids(q)
       if idx == 0
           # This worker is not assigned a piece
           return 1:0, 1:0
       end
       nchunks = length(procs(q))
       splits = [round(Int, s) for s in linspace(0,size(q,2),nchunks+1)]
       1:size(q,1), splits[idx]+1:splits[idx+1]
   end

   # Here's the kernel
   @everywhere function advection_chunk!(q, u, irange, jrange, trange)
       @show (irange, jrange, trange)  # display so we can see what's happening
       for t in trange, j in jrange, i in irange
           q[i,j,t+1] = q[i,j,t] +  u[i,j,t]
       end
       q
   end

   # Here's a convenience wrapper for a SharedArray implementation
   @everywhere advection_shared_chunk!(q, u) = advection_chunk!(q, u, myrange(q)..., 1:size(q,3)-1)

Now let's compare three different versions, one that runs in a single process::

   advection_serial!(q, u) = advection_chunk!(q, u, 1:size(q,1), 1:size(q,2), 1:size(q,3)-1)

one that uses ``@parallel``::

   function advection_parallel!(q, u)
       for t = 1:size(q,3)-1
           @sync @parallel for j = 1:size(q,2)
               for i = 1:size(q,1)
                   q[i,j,t+1]= q[i,j,t] + u[i,j,t]
               end
           end
       end
       q
   end

and one that delegates in chunks::

   function advection_shared!(q, u)
       @sync begin
           for p in procs(q)
               @async remotecall_wait(advection_shared_chunk!, p, q, u)
           end
       end
       q
   end

If we create SharedArrays and time these functions, we get the following results (with ``julia -p 4``)::

   q = SharedArray(Float64, (500,500,500))
   u = SharedArray(Float64, (500,500,500))

   # Run once to JIT-compile
   advection_serial!(q, u)
   advection_parallel!(q, u)
   advection_shared!(q,u)

   # Now the real results:
   julia> @time advection_serial!(q, u);
   (irange,jrange,trange) = (1:500,1:500,1:499)
    830.220 milliseconds (216 allocations: 13820 bytes)

   julia> @time advection_parallel!(q, u);
      2.495 seconds      (3999 k allocations: 289 MB, 2.09% gc time)

   julia> @time advection_shared!(q,u);
           From worker 2:	(irange,jrange,trange) = (1:500,1:125,1:499)
           From worker 4:	(irange,jrange,trange) = (1:500,251:375,1:499)
           From worker 3:	(irange,jrange,trange) = (1:500,126:250,1:499)
           From worker 5:	(irange,jrange,trange) = (1:500,376:500,1:499)
    238.119 milliseconds (2264 allocations: 169 KB)

The biggest advantage of ``advection_shared!`` is that it minimizes traffic
among the workers, allowing each to compute for an extended time on the
assigned piece.

Shared Arrays and Distributed Garbage Collection
------------------------------------------------

Like remote references, shared arrays are also dependent on garbage collection
on the creating node to release references from all participating workers. Code which
creates many short lived shared array objects would benefit from explicitly
finalizing these objects as soon as possible. This results in both memory and file
handles mapping the shared segment being released sooner.


.. _man-clustermanagers:

ClusterManagers
---------------

The launching, management and networking of julia processes into a logical
cluster is done via cluster managers. A :obj:`ClusterManager` is responsible for

- launching worker processes in a cluster environment
- managing events during the lifetime of each worker
- optionally, a cluster manager can also provide data transport

A julia cluster has the following characteristics:
- The initial julia process, also called the ``master`` is special and has a julia id of 1.
- Only the ``master`` process can add or remove worker processes.
- All processes can directly communicate with each other.

Connections between workers (using the in-built TCP/IP transport) is established in the following manner:

- :func:`addprocs` is called on the master process with a :obj:`ClusterManager` object
- :func:`addprocs` calls the appropriate :func:`launch` method which spawns
  required number of worker processes on appropriate machines
- Each worker starts listening on a free port and writes out its host, port information to :const:`STDOUT`
- The cluster manager captures the stdout's of each worker and makes it available to the master process
- The master process parses this information and sets up TCP/IP connections to each worker
- Every worker is also notified of other workers in the cluster
- Each worker connects to all workers whose julia id is less than its own id
- In this way a mesh network is established, wherein every worker is directly connected with every other worker


While the default transport layer uses plain TCP sockets, it is possible for a julia cluster to provide
its own transport.

Julia provides two in-built cluster managers:

- ``LocalManager``, used when :func:`addprocs` or :func:`addprocs(np::Integer) <addprocs>` are called
- ``SSHManager``, used when :func:`addprocs(hostnames::Array) <addprocs>` is called with a list of hostnames

:class:`LocalManager` is used to launch additional workers on the same host, thereby leveraging multi-core
and multi-processor hardware.

Thus, a minimal cluster manager would need to:

- be a subtype of the abstract :class:`ClusterManager`
- implement :func:`launch`, a method responsible for launching new workers
- implement :func:`manage`, which is called at various events during a worker's lifetime

:func:`addprocs(manager::FooManager) <addprocs>` requires ``FooManager`` to implement::

    function launch(manager::FooManager, params::Dict, launched::Array, c::Condition)
        ...
    end

    function manage(manager::FooManager, id::Integer, config::WorkerConfig, op::Symbol)
        ...
    end

As an example let us see how the :class:`LocalManager`, the manager responsible for
starting workers on the same host, is implemented::

    immutable LocalManager <: ClusterManager
        np::Integer
    end

    function launch(manager::LocalManager, params::Dict, launched::Array, c::Condition)
        ...
    end

    function manage(manager::LocalManager, id::Integer, config::WorkerConfig, op::Symbol)
        ...
    end

The :func:`launch` method takes the following arguments:

- ``manager::ClusterManager`` - the cluster manager :func:`addprocs` is called with
- ``params::Dict`` - all the keyword arguments passed to :func:`addprocs`
- ``launched::Array`` - the array to append one or more ``WorkerConfig`` objects to
- ``c::Condition`` - the condition variable to be notified as and when workers are launched

The :func:`launch` method is called asynchronously in a separate task. The termination of this task
signals that all requested workers have been launched. Hence the :func:`launch` function MUST exit as soon
as all the requested workers have been launched.

Newly launched workers are connected to each other, and the master process, in a all-to-all manner.
Specifying command argument, ``--worker <cookie>`` results in the launched processes initializing themselves
as workers and connections being setup via TCP/IP sockets. Optionally ``--bind-to bind_addr[:port]``
may also be specified to enable other workers to connect to it at the specified ``bind_addr`` and ``port``.
This is useful for multi-homed hosts.

For non-TCP/IP transports, for example, an implementation may choose to use MPI as the transport,
``--worker`` must NOT be specified. Instead newly launched workers should call ``init_worker(cookie)``
before using any of the parallel constructs.

For every worker launched, the :func:`launch` method must add a :class:`WorkerConfig`
object (with appropriate fields initialized) to ``launched`` ::

    type WorkerConfig
        # Common fields relevant to all cluster managers
        io::Nullable{IO}
        host::Nullable{AbstractString}
        port::Nullable{Integer}

        # Used when launching additional workers at a host
        count::Nullable{Union{Int, Symbol}}
        exename::Nullable{AbstractString}
        exeflags::Nullable{Cmd}

        # External cluster managers can use this to store information at a per-worker level
        # Can be a dict if multiple fields need to be stored.
        userdata::Nullable{Any}

        # SSHManager / SSH tunnel connections to workers
        tunnel::Nullable{Bool}
        bind_addr::Nullable{AbstractString}
        sshflags::Nullable{Cmd}
        max_parallel::Nullable{Integer}

        connect_at::Nullable{Any}

        .....
    end

Most of the fields in :class:`WorkerConfig` are used by the inbuilt managers.
Custom cluster managers would typically specify only ``io`` or ``host`` / ``port``:

If ``io`` is specified, it is used to read host/port information. A Julia
worker prints out its bind address and port at startup. This allows Julia
workers to listen on any free port available instead of requiring worker ports
to be configured manually.

If ``io`` is not specified, ``host`` and ``port`` are used to connect.

``count``, ``exename`` and ``exeflags`` are relevant for launching additional workers from a worker.
For example, a cluster manager may launch a single worker per node, and use that to launch
additional workers. ``count`` with an integer value ``n`` will launch a total of ``n`` workers,
while a value of ``:auto`` will launch as many workers as cores on that machine.
``exename`` is the name of the julia executable including the full path.
``exeflags`` should be set to the required command line arguments for new workers.

``tunnel``, ``bind_addr``, ``sshflags`` and ``max_parallel`` are used when a ssh tunnel is
required to connect to the workers from the master process.

``userdata`` is provided for custom cluster managers to store their own worker specific information.


``manage(manager::FooManager, id::Integer, config::WorkerConfig, op::Symbol)`` is called at different
times during the worker's lifetime with appropriate ``op`` values:

- with ``:register``/``:deregister`` when a worker is added / removed
  from the Julia worker pool.
- with ``:interrupt`` when ``interrupt(workers)`` is called. The
  :class:`ClusterManager` should signal the appropriate worker with an
  interrupt signal.
- with ``:finalize`` for cleanup purposes.


Cluster Managers with custom transports
---------------------------------------

Replacing the default TCP/IP all-to-all socket connections with a custom transport layer is a little more involved.
Each julia process has as many communication tasks as the workers it is connected to. For example, consider a julia cluster of
32 processes in a all-to-all mesh network:

- Each julia process thus has 31 communication tasks
- Each task handles all incoming messages from a single remote worker in a message processing loop
- The message processing loop waits on an ``AsyncStream`` object - for example, a TCP socket in the default implementation, reads an entire
  message, processes it and waits for the next one
- Sending messages to a process is done directly from any julia task - not just communication tasks - again, via the appropriate
  ``AsyncStream`` object

Replacing the default transport involves the new implementation to setup connections to remote workers, and to provide appropriate
``AsyncStream`` objects that the message processing loops can wait on. The manager specific callbacks to be implemented are::

    connect(manager::FooManager, pid::Integer, config::WorkerConfig)
    kill(manager::FooManager, pid::Int, config::WorkerConfig)

The default implementation (which uses TCP/IP sockets) is implemented as ``connect(manager::ClusterManager, pid::Integer, config::WorkerConfig)``.

``connect`` should return a pair of ``AsyncStream`` objects, one for reading data sent from worker ``pid``,
and the other to write data that needs to be sent to worker ``pid``. Custom cluster managers can use an in-memory ``BufferStream``
as the plumbing to proxy data between the custom, possibly non-AsyncStream transport and julia's in-built parallel infrastructure.

A ``BufferStream`` is an in-memory ``IOBuffer`` which behaves like an ``AsyncStream``.

Folder ``examples/clustermanager/0mq`` is an example of using ZeroMQ is connect julia workers in a star network with a 0MQ broker in the middle.
Note: The julia processes are still all *logically* connected to each other - any worker can message any other worker directly without any
awareness of 0MQ being used as the transport layer.

When using custom transports:

- julia workers must NOT be started with ``--worker``. Starting with ``--worker`` will result in the newly launched
  workers defaulting to the TCP/IP socket transport implementation
- For every incoming logical connection with a worker, ``Base.process_messages(rd::AsyncStream, wr::AsyncStream)`` must be called.
  This launches a new task that handles reading and writing of messages from/to the worker represented by the ``AsyncStream`` objects
- ``init_worker(cookie, manager::FooManager)`` MUST be called as part of worker process initializaton
- Field ``connect_at::Any`` in :class:`WorkerConfig` can be set by the cluster manager when ``launch`` is called. The value of
  this field is passed in in all ``connect`` callbacks. Typically, it carries information on *how to connect* to a worker. For example,
  the TCP/IP socket transport uses this field to specify the ``(host, port)`` tuple at which to connect to a worker

``kill(manager, pid, config)`` is called to remove a worker from the cluster.
On the master process, the corresponding ``AsyncStream`` objects must be closed by the implementation to ensure proper cleanup. The default
implementation simply executes an ``exit()`` call on the specified remote worker.

``examples/clustermanager/simple`` is an example that shows a simple implementation using unix domain sockets for cluster setup

Network requirements for LocalManager and SSHManager
----------------------------------------------------
Julia clusters are designed to be executed on already secured environments on infrastructure ranging from local laptops,
to departmental clusters or even on the cloud. This section covers network security requirements for the inbuilt ``LocalManager``
and ``SSHManager``:

- The master process does not listen on any port. It only connects out to the workers.

- Each worker binds to only one of the local interfaces and listens on the first free port starting from 9009.

- ``LocalManager``, i.e. ``addprocs(N)``, by default binds only to the loopback interface.
  This means that workers consequently started on remote hosts, or anyone with malicious intentions
  is unable to connect to the cluster. A ``addprocs(4)`` followed by a ``addprocs(["remote_host"])``
  will fail. Some users may need to create a cluster comprised of their local system and a few remote systems.
  This can be done by explicitly requesting ``LocalManager`` to bind to an external network interface via the
  ``restrict`` keyword argument. For example, ``addprocs(4; restrict=false)``.

- ``SSHManager``, i.e. ``addprocs(list_of_remote_hosts)`` launches workers on remote hosts via SSH.
  It is to be noted that by default SSH is only used to launch Julia workers.
  Subsequent master-worker and worker-worker connections use plain, unencrypted TCP/IP sockets. The remote hosts
  must have passwordless login enabled. Additional SSH flags or credentials may be specified via keyword
  argument ``sshflags``.

- ``addprocs(list_of_remote_hosts; tunnel=true, sshflags=<ssh keys and other flags>)`` is useful when we wish to use
  SSH connections for master-worker too. A typical scenario for this is a local laptop running the Julia REPL (i.e., the master)
  with the rest of the cluster on the cloud, say on Amazon EC2. In this case only port 22 needs to be
  opened at the remote cluster coupled with SSH client authenticated via PKI.
  Authentication credentials can be supplied via ``sshflags``, for example ``sshflags=`-e <keyfile>```.

  Note that worker-worker connections are still plain TCP and the local security policy on the remote cluster
  must allow for free connections between worker nodes, at least for ports 9009 and above.

  Securing and encrypting all worker-worker traffic (via SSH), or encrypting individual messages can be done via
  a custom ClusterManager.

Cluster cookie
--------------
All processes in a cluster share the same cookie which, by default, is a randomly generated string on the master process:

- ``Base.cluster_cookie()`` returns the cookie, ``Base.cluster_cookie(cookie)`` sets it.
- All connections are authenticated on both sides to ensure that only workers started by the master are allowed
  to connect to each other.
- The cookie must be passed to the workers at startup via argument ``--worker <cookie>``.
  Custom ClusterManagers can retrieve the cookie on the master by calling
  ``Base.cluster_cookie()``. Cluster managers not using the default TCP/IP transport (and hence not specifying ``--worker``)
  must call ``init_worker(cookie, manager)`` with the same cookie as on the master.

It is to be noted that environments requiring higher levels of security (for example, cookies can be pre-shared and hence not
specified as a startup arg) can implement this via a custom ClusterManager.


Specifying network topology (Experimental)
-------------------------------------------

Keyword argument ``topology`` to ``addprocs`` is used to specify how the workers must be connected to each other:

- ``:all_to_all`` : is the default, where all workers are connected to each other.

- ``:master_slave`` : only the driver process, i.e. pid 1 has connections to the workers.

- ``:custom`` : the ``launch`` method of the cluster manager specifes the connection topology.
  Fields ``ident`` and ``connect_idents`` in ``WorkerConfig`` are used to specify the  same.
  ``connect_idents`` is a list of ``ClusterManager`` provided identifiers to workers that worker
  with identified by ``ident`` must connect to.

Currently sending a message between unconnected workers results in an error. This behaviour, as also the
functionality and interface should be considered experimental in nature and may change in future releases.

.. rubric:: Footnotes

.. [#mpi2rma] In this context, MPI refers to the MPI-1 standard. Beginning with MPI-2, the MPI standards committee introduced a new set of communication mechanisms, collectively referred to as Remote Memory Access (RMA). The motivation for adding RMA to the MPI standard was to facilitate one-sided communication patterns. For additional information on the latest MPI standard, see http://www.mpi-forum.org/docs.

.. _DArray: https://github.com/JuliaParallel/DistributedArrays.jl
