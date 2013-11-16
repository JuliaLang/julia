.. _man-parallel-computing:

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
issues are very relevant on a typical multicore laptop, due to
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
A remote call returns a remote reference to its result. Remote calls
return immediately; the process that made the call proceeds to its
next operation while the remote call happens somewhere else. You can
wait for a remote call to finish by calling ``wait`` on its remote
reference, and you can obtain the full value of the result using
``fetch``. You can store a value to a remote reference using ``put``.

Let's try this out. Starting with ``julia -p n`` provides ``n`` worker
processes on the local machine. Generally it makes sense for ``n`` to
equal the number of CPU cores on the machine.

::

    $ ./julia -p 2

    julia> r = remotecall(2, rand, 2, 2)
    RemoteRef(2,1,5)

    julia> fetch(r)
    2x2 Float64 Array:
     0.60401   0.501111
     0.174572  0.157411

    julia> s = @spawnat 2 1+fetch(r)
    RemoteRef(2,1,7)

    julia> fetch(s)
    2x2 Float64 Array:
     1.60401  1.50111
     1.17457  1.15741

The first argument to ``remotecall`` is the index of the process
that will do the work. Most parallel programming in Julia does not
reference specific processes or the number of processes available,
but ``remotecall`` is considered a low-level interface providing
finer control. The second argument to ``remotecall`` is the function
to call, and the remaining arguments will be passed to this
function. As you can see, in the first line we asked process 2 to
construct a 2-by-2 random matrix, and in the second line we asked it
to add 1 to it. The result of both calculations is available in the
two remote references, ``r`` and ``s``. The ``@spawnat`` macro
evaluates the expression in the second argument on the process
specified by the first argument.

Occasionally you might want a remotely-computed value immediately. This
typically happens when you read from a remote object to obtain data
needed by the next local operation. The function ``remotecall_fetch``
exists for this purpose. It is equivalent to ``fetch(remotecall(...))``
but is more efficient.

::

    julia> remotecall_fetch(2, getindex, r, 1, 1)
    0.10824216411304866

Remember that ``getindex(r,1,1)`` is :ref:`equivalent <man-array-indexing>` to
``r[1,1]``, so this call fetches the first element of the remote
reference ``r``.

The syntax of ``remotecall`` is not especially convenient. The macro
``@spawn`` makes things easier. It operates on an expression rather than
a function, and picks where to do the operation for you::

    julia> r = @spawn rand(2,2)
    RemoteRef(1,1,0)

    julia> s = @spawn 1+fetch(r)
    RemoteRef(1,1,1)

    julia> fetch(s)
    1.10824216411304866 1.13798233877923116
    1.12376292706355074 1.18750497916607167

Note that we used ``1+fetch(r)`` instead of ``1+r``. This is because we
do not know where the code will run, so in general a ``fetch`` might be
required to move ``r`` to the process doing the addition. In this
case, ``@spawn`` is smart enough to perform the computation on the
process that owns ``r``, so the ``fetch`` will be a no-op.

(It is worth noting that ``@spawn`` is not built-in but defined in Julia
as a :ref:`macro <man-macros>`. It is possible to define your
own such constructs.)

One important point is that your code must be available on any process
that runs it. For example, type the following into the julia prompt::

    julia> function rand2(dims...)
             return 2*rand(dims...)
           end

    julia> rand2(2,2)
    2x2 Float64 Array:
     0.153756  0.368514
     1.15119   0.918912

    julia> @spawn rand2(2,2)
    RemoteRef(1,1,1)

    julia> @spawn rand2(2,2)
    RemoteRef(2,1,2)

    julia> exception on 2: in anonymous: rand2 not defined 

Processor 1 knew about the function ``rand2``, but process 2 did not.
To make your code available to all processes, the ``require`` function will
automatically load a source file on all currently available processes::

    julia> require("myfile")

In a cluster, the contents of the file (and any files loaded recursively)
will be sent over the network. It is also useful to execute a statement on all processes. This can be done with the ``@everywhere`` macro::

    julia> @everywhere id = myid()

    julia> remotecall_fetch(2, ()->id)
    2

    @everywhere include("defs.jl")

A file can also be preloaded on multiple processes at startup, and a driver script can be used to drive the computation::

    julia -p <n> -L file1.jl -L file2.jl driver.jl
    
Each process has an associated identifier. The process providing the interactive julia prompt
always has an id equal to 1, as would the julia process running the driver script in the
example above.
The processors used by default for parallel operations are referred to as ``workers``.
When there is only one process, process 1 is considered a worker. Otherwise, workers are
considered to be all processes other than process 1.

The base Julia installation has in-built support for two types of clusters: 

    - A local cluster specified with the ``-p`` option as shown above.  
    
    - And a cluster spanning machines using the ``--machinefile`` option. This uses ``ssh`` to start 
      the worker processes on the specified machines.
    
Functions ``addprocs``, ``rmprocs``, ``workers`` and others, are available as a programmatic means of 
adding, removing and querying the processes in a cluster.

Other types of clusters can be supported by writing your own custom ClusterManager. See section on 
ClusterManagers.

Data Movement
-------------

Sending messages and moving data constitute most of the overhead in a
parallel program. Reducing the number of messages and the amount of data
sent is critical to achieving performance and scalability. To this end,
it is important to understand the data movement performed by Julia's
various parallel programming constructs.

``fetch`` can be considered an explicit data movement operation, since
it directly asks that an object be moved to the local machine.
``@spawn`` (and a few related constructs) also moves data, but this is
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
the behavior of ``@spawn``. In the first method, a random matrix is
constructed locally, then sent to another process where it is squared.
In the second method, a random matrix is both constructed and squared on
another process. Therefore the second method sends much less data than
the first.

In this toy example, the two methods are easy to distinguish and choose
from. However, in a real program designing data movement might require
more thought and very likely some measurement. For example, if the first
process needs matrix ``A`` then the first method might be better. Or,
if computing ``A`` is expensive and only the current process has it,
then moving it to another process might be unavoidable. Or, if the
current process has very little to do between the ``@spawn`` and
``fetch(Bref)`` then it might be better to eliminate the parallelism
altogether. Or imagine ``rand(1000,1000)`` is replaced with a more
expensive operation. Then it might make sense to add another ``@spawn``
statement just for this step.

Parallel Map and Loops
----------------------

Fortunately, many useful parallel computations do not require data
movement. A common example is a Monte Carlo simulation, where multiple
processes can handle independent simulation trials simultaneously. We
can use ``@spawn`` to flip coins on two processes. First, write the
following function in ``count_heads.jl``::

    function count_heads(n)
        c::Int = 0
        for i=1:n
            c += randbool()
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

This example, as simple as it is, demonstrates a powerful and often-used
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
generalized. We used two explicit ``@spawn`` statements, which limits
the parallelism to two processes. To run on any number of processes,
we can use a *parallel for loop*, which can be written in Julia like
this::

    nheads = @parallel (+) for i=1:200000000
      int(randbool())
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

Notice that the reduction operator can be omitted if it is not needed.
However, this code will not initialize all of ``a``, since each
process will have a separate copy if it. Parallel for loops like these
must be avoided. Fortunately, distributed arrays can be used to get
around this limitation, as we will see in the next section.

Using "outside" variables in parallel loops is perfectly reasonable if
the variables are read-only::

    a = randn(1000)
    @parallel (+) for i=1:100000
      f(a[randi(end)])
    end

Here each iteration applies ``f`` to a randomly-chosen sample from a
vector ``a`` shared by all processes.

In some cases no reduction operator is needed, and we merely wish to
apply a function to all integers in some range (or, more generally, to
all elements in some collection). This is another useful operation
called *parallel map*, implemented in Julia as the ``pmap`` function.
For example, we could compute the singular values of several large
random matrices in parallel as follows::

    M = {rand(1000,1000) for i=1:10}
    pmap(svd, M)

Julia's ``pmap`` is designed for the case where each function call does
a large amount of work. In contrast, ``@parallel for`` can handle
situations where each iteration is tiny, perhaps merely summing two
numbers. Only worker processes are used by both ``pmap`` and ``@parallel for`` 
for the parallel computation. In case of ``@parallel for``, the final reduction 
is done on the calling process.



Synchronization With Remote References
--------------------------------------

Scheduling
----------

Julia's parallel programming platform uses
:ref:`man-tasks` to switch among
multiple computations. Whenever code performs a communication operation
like ``fetch`` or ``wait``, the current task is suspended and a
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

    M = {rand(800,800), rand(600,600), rand(800,800), rand(600,600)}
    pmap(svd, M)

If one process handles both 800x800 matrices and another handles both
600x600 matrices, we will not get as much scalability as we could. The
solution is to make a local task to "feed" work to each process when
it completes its current task. This can be seen in the implementation of
``pmap``::

    function pmap(f, lst)
        np = nprocs()  # determine the number of processes available
        n = length(lst)
        results = cell(n)
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
                            results[idx] = remotecall_fetch(p, f, lst[idx])
                        end
                    end
                end
            end
        end
        results
    end

``@async`` is similar to ``@spawn``, but only runs tasks on the
local process. We use it to create a "feeder" task for each process.
Each task picks the next index that needs to be computed, then waits for
its process to finish, then repeats until we run out of indexes. Note
that the feeder tasks do not begin to execute until the main task
reaches the end of the ``@sync`` block, at which point it surrenders
control and waits for all the local tasks to complete before returning
from the function. The feeder tasks are able to share state via
``nextidx()`` because they all run on the same process. No locking is
required, since the threads are scheduled cooperatively and not
preemptively. This means context switches only occur at well-defined
points: in this case, when ``remotecall_fetch`` is called.

Distributed Arrays
------------------

Large computations are often organized around large arrays of data. In
these cases, a particularly natural way to obtain parallelism is to
distribute arrays among several processes. This combines the memory
resources of multiple machines, allowing use of arrays too large to fit
on one machine. Each process operates on the part of the array it
owns, providing a ready answer to the question of how a program should
be divided among machines.

Julia distributed arrays are implemented by the ``DArray`` type. A
``DArray`` has an element type and dimensions just like an ``Array``.
A ``DArray`` can also use arbitrary array-like types to represent the local
chunks that store actual data. The data in a ``DArray`` is distributed by
dividing the index space into some number of blocks in each dimension.

Common kinds of arrays can be constructed with functions beginning with
``d``::

    dzeros(100,100,10)
    dones(100,100,10)
    drand(100,100,10)
    drandn(100,100,10)
    dfill(x, 100,100,10)

In the last case, each element will be initialized to the specified
value ``x``. These functions automatically pick a distribution for you.
For more control, you can specify which processors to use, and how the
data should be distributed::

    dzeros((100,100), workers()[1:4], [1,4])

The second argument specifies that the array should be created on the first
four workers. When dividing data among a large number of processes,
one often sees diminishing returns in performance. Placing ``DArray``\ s
on a subset of processes allows multiple ``DArray`` computations to
happen at once, with a higher ratio of work to communication on each
process.

The third argument specifies a distribution; the nth element of
this array specifies how many pieces dimension n should be divided into.
In this example the first dimension will not be divided, and the second
dimension will be divided into 4 pieces. Therefore each local chunk will be
of size ``(100,25)``. Note that the product of the distribution array must
equal the number of processors.

``distribute(a::Array)`` converts a local array to a distributed array.

``localpart(a::DArray)`` obtains the locally-stored portion
of a ``DArray``.

``myindexes(a::DArray)`` gives a tuple of the index ranges owned by the
local process.

``convert(Array, a::DArray)`` brings all the data to the local processor.

Indexing a ``DArray`` (square brackets) with ranges of indexes always
creates a ``SubArray``, not copying any data.


Constructing Distributed Arrays
-------------------------------

The primitive ``DArray`` constructor has the following somewhat elaborate signature::

    DArray(init, dims[, procs, dist])

``init`` is a function that accepts a tuple of index ranges. This function should
allocate a local chunk of the distributed array and initialize it for the specified
indices. ``dims`` is the overall size of the distributed array.
``procs`` optionally specifies a vector of processor IDs to use.
``dist`` is an integer vector specifying how many chunks the
distributed array should be divided into in each dimension.

The last two arguments are optional, and defaults will be used if they
are omitted.

As an example, here is how to turn the local array constructor ``fill``
into a distributed array constructor::

    dfill(v, args...) = DArray(I->fill(v, map(length,I)), args...)

In this case the ``init`` function only needs to call ``fill`` with the
dimensions of the local piece it is creating.

Distributed Array Operations
----------------------------

At this time, distributed arrays do not have much functionality. Their
major utility is allowing communication to be done via array indexing, which
is convenient for many problems. As an example, consider implementing the
"life" cellular automaton, where each cell in a grid is updated according
to its neighboring cells. To compute a chunk of the result of one iteration,
each processor needs the immediate neighbor cells of its local chunk. The
following code accomplishes this::

    function life_step(d::DArray)
        DArray(size(d),procs(d)) do I
            top   = mod(first(I[1])-2,size(d,1))+1
            bot   = mod( last(I[1])  ,size(d,1))+1
            left  = mod(first(I[2])-2,size(d,2))+1
            right = mod( last(I[2])  ,size(d,2))+1

            old = Array(Bool, length(I[1])+2, length(I[2])+2)
            old[1      , 1      ] = d[top , left]   # left side
            old[2:end-1, 1      ] = d[I[1], left]
            old[end    , 1      ] = d[bot , left]
            old[1      , 2:end-1] = d[top , I[2]]
            old[2:end-1, 2:end-1] = d[I[1], I[2]]   # middle
            old[end    , 2:end-1] = d[bot , I[2]]
            old[1      , end    ] = d[top , right]  # right side
            old[2:end-1, end    ] = d[I[1], right]
            old[end    , end    ] = d[bot , right]

            life_rule(old)
        end
    end

As you can see, we use a series of indexing expressions to fetch
data into a local array ``old``. Note that the ``do`` block syntax is
convenient for passing ``init`` functions to the ``DArray`` constructor.
Next, the serial function ``life_rule`` is called to apply the update rules
to the data, yielding the needed ``DArray`` chunk. Nothing about ``life_rule``
is ``DArray``\ -specific, but we list it here for completeness::

    function life_rule(old)
        m, n = size(old)
        new = similar(old, m-2, n-2)
        for j = 2:n-1
            for i = 2:m-1
                nc = +(old[i-1,j-1], old[i-1,j], old[i-1,j+1],
                       old[i  ,j-1],             old[i  ,j+1],
                       old[i+1,j-1], old[i+1,j], old[i+1,j+1])
                new[i-1,j-1] = (nc == 3 ? 1 :
                                nc == 2 ? old[i,j] :
                                0)
            end
        end
        new
    end


ClusterManagers
---------------

Julia worker processes can also be spawned on arbitrary machines,
enabling Julia's natural parallelism to function quite transparently
in a cluster environment. The ``ClusterManager`` interface provides a
way to specify a means to launch and manage worker processes. For
example, ``ssh`` clusters are also implemented using a ``ClusterManager``::

    immutable SSHManager <: ClusterManager
        launch::Function
        manage::Function
        machines::AbstractVector

        SSHManager(; machines=[]) = new(launch_ssh_workers, manage_ssh_workers, machines)
    end

    function launch_ssh_workers(cman::SSHManager, np::Integer, config::Dict)
        ...
    end

    function manage_ssh_workers(id::Integer, config::Dict, op::Symbol)
        ...
    end

where ``launch_ssh_workers`` is responsible for instantiating new
Julia processes and ``manage_ssh_workers`` provides a means to manage
those processes, e.g. for sending interrupt signals. New processes can
then be added at runtime using ``addprocs``::

    addprocs(5, cman=LocalManager())

which specifies a number of processes to add and a ``ClusterManager`` to
use for launching those processes.

.. rubric:: Footnotes

.. [#mpi2rma] In this context, MPI refers to the MPI-1 standard. Beginning with MPI-2, the MPI standards committee introduced a new set of communication mechanisms, collectively referred to as Remote Memory Access (RMA). The motivation for adding RMA to the MPI standard was to facilitate one-sided communication patterns. For additional information on the latest MPI standard, see http://www.mpi-forum.org/docs.
