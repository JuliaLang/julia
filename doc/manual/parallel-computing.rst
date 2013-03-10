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
to run on multiple processors in separate memory domains at once.

Julia's implementation of message passing is different from other
environments such as MPI. Communication in Julia is generally
"one-sided", meaning that the programmer needs to explicitly manage only
one processor in a two-processor operation. Furthermore, these
operations typically do not look like "message send" and "message
receive" but rather resemble higher-level operations like calls to user
functions.

Parallel programming in Julia is built on two primitives: *remote
references* and *remote calls*. A remote reference is an object that can
be used from any processor to refer to an object stored on a particular
processor. A remote call is a request by one processor to call a certain
function on certain arguments on another (possibly the same) processor.
A remote call returns a remote reference to its result. Remote calls
return immediately; the processor that made the call proceeds to its
next operation while the remote call happens somewhere else. You can
wait for a remote call to finish by calling ``wait`` on its remote
reference, and you can obtain the full value of the result using
``fetch``.

Let's try this out. Starting with ``julia -p n`` provides ``n``
processors on the local machine. Generally it makes sense for ``n`` to
equal the number of CPU cores on the machine.

::

    $ ./julia -p 2

    julia> r = remote_call(2, rand, 2, 2)
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

The first argument to ``remote_call`` is the index of the processor
that will do the work. Most parallel programming in Julia does not
reference specific processors or the number of processors available,
but ``remote_call`` is considered a low-level interface providing
finer control. The second argument to ``remote_call`` is the function
to call, and the remaining arguments will be passed to this
function. As you can see, in the first line we asked processor 2 to
construct a 2-by-2 random matrix, and in the second line we asked it
to add 1 to it. The result of both calculations is available in the
two remote references, ``r`` and ``s``. The ``@spawnat`` macro
evaluates the expression in the second argument on the processor
specified by the first argument.

Occasionally you might want a remotely-computed value immediately. This
typically happens when you read from a remote object to obtain data
needed by the next local operation. The function ``remote_call_fetch``
exists for this purpose. It is equivalent to ``fetch(remote_call(...))``
but is more efficient.

::

    julia> remote_call_fetch(2, getindex, r, 1, 1)
    0.10824216411304866

Remember that ``getindex(r,1,1)`` is :ref:`equivalent <man-array-indexing>` to
``r[1,1]``, so this call fetches the first element of the remote
reference ``r``.

The syntax of ``remote_call`` is not especially convenient. The macro
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
required to move ``r`` to the processor doing the addition. In this
case, ``@spawn`` is smart enough to perform the computation on the
processor that owns ``r``, so the ``fetch`` will be a no-op.

(It is worth noting that ``@spawn`` is not built-in but defined in Julia
as a :ref:`macro <man-macros>`. It is possible to define your
own such constructs.)

One important point is that your code must be available on any processor
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

Processor 1 knew about the function ``rand2``, but processor 2 did not.
To make your code available to all processors, the ``require`` function will
automatically load a source file on all currently available processors::

    julia> require("myfile")

In a cluster, the contents of the file (and any files loaded recursively)
will be sent over the network.

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
constructed locally, then sent to another processor where it is squared.
In the second method, a random matrix is both constructed and squared on
another processor. Therefore the second method sends much less data than
the first.

In this toy example, the two methods are easy to distinguish and choose
from. However, in a real program designing data movement might require
more thought and very likely some measurement. For example, if the first
processor needs matrix ``A`` then the first method might be better. Or,
if computing ``A`` is expensive and only the current processor has it,
then moving it to another processor might be unavoidable. Or, if the
current processor has very little to do between the ``@spawn`` and
``fetch(Bref)`` then it might be better to eliminate the parallelism
altogether. Or imagine ``rand(1000,1000)`` is replaced with a more
expensive operation. Then it might make sense to add another ``@spawn``
statement just for this step.

Parallel Map and Loops
----------------------

Fortunately, many useful parallel computations do not require data
movement. A common example is a monte carlo simulation, where multiple
processors can handle independent simulation trials simultaneously. We
can use ``@spawn`` to flip coins on two processors. First, write the
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
several processors, and then their results are combined using some
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
the parallelism to two processors. To run on any number of processors,
we can use a *parallel for loop*, which can be written in Julia like
this::

    nheads = @parallel (+) for i=1:200000000
      randbool()
    end

This construct implements the pattern of assigning iterations to
multiple processors, and combining them with a specified reduction (in
this case ``(+)``). The result of each iteration is taken as the value
of the last expression inside the loop. The whole parallel loop
expression itself evaluates to the final answer.

Note that although parallel for loops look like serial for loops, their
behavior is dramatically different. In particular, the iterations do not
happen in a specified order, and writes to variables or arrays will not
be globally visible since iterations run on different processors. Any
variables used inside the parallel loop will be copied and broadcast to
each processor.

For example, the following code will not work as intended::

    a = zeros(100000)
    @parallel for i=1:100000
      a[i] = i
    end

Notice that the reduction operator can be omitted if it is not needed.
However, this code will not initialize all of ``a``, since each
processor will have a separate copy if it. Parallel for loops like these
must be avoided. Fortunately, distributed arrays can be used to get
around this limitation, as we will see in the next section.

Using "outside" variables in parallel loops is perfectly reasonable if
the variables are read-only::

    a = randn(1000)
    @parallel (+) for i=1:100000
      f(a[randi(end)])
    end

Here each iteration applies ``f`` to a randomly-chosen sample from a
vector ``a`` shared by all processors.

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
numbers.

..
   Distributed Arrays
   ------------------

   Large computations are often organized around large arrays of data. In
   these cases, a particularly natural way to obtain parallelism is to
   distribute arrays among several processors. This combines the memory
   resources of multiple machines, allowing use of arrays too large to fit
   on one machine. Each processor operates on the part of the array it
   owns, providing a ready answer to the question of how a program should
   be divided among machines.

   A distributed array (or, more generally, a *global object*) is logically
   a single array, but pieces of it are stored on different processors.
   This means whole-array operations such as matrix multiply, scalar\*array
   multiplication, etc. use the same syntax as with local arrays, and the
   parallelism is invisible. In some cases it is possible to obtain useful
   parallelism just by changing a local array to a distributed array.

   Julia distributed arrays are implemented by the ``DArray`` type. A
   ``DArray`` has an element type and dimensions just like an ``Array``,
   but it also needs an additional property: the dimension along which data
   is distributed. There are many possible ways to distribute data among
   processors, but at this time Julia keeps things simple and only allows
   distributing along a single dimension. For example, if a 2-d ``DArray``
   is distributed in dimension 1, it means each processor holds a certain
   range of rows. If it is distributed in dimension 2, each processor holds
   a certain range of columns.

   Common kinds of arrays can be constructed with functions beginning with
   ``d``::

       dzeros(100,100,10)
       dones(100,100,10)
       drand(100,100,10)
       drandn(100,100,10)
       dcell(100,100,10)
       dfill(x, 100,100,10)

   In the last case, each element will be initialized to the specified
   value ``x``. These functions automatically pick a distributed dimension
   for you. To specify the distributed dimension, other forms are
   available::

       drand((100,100,10), 3)
       dzeros(Int64, (100,100), 2)
       dzeros((100,100), 2, [7, 8])

   In the ``drand`` call, we specified that the array should be distributed
   across dimension 3. In the first ``dzeros`` call, we specified an
   element type as well as the distributed dimension. In the second
   ``dzeros`` call, we also specified which processors should be used to
   store the data. When dividing data among a large number of processors,
   one often sees diminishing returns in performance. Placing ``DArray``\ s
   on a subset of processors allows multiple ``DArray`` computations to
   happen at once, with a higher ratio of work to communication on each
   processor.

   ``distribute(a::Array, dim)`` can be used to convert a local array to a
   distributed array, optionally specifying the distributed dimension.
   ``localize(a::DArray)`` can be used to obtain the locally-stored portion
   of a ``DArray``. ``owner(a::DArray, index)`` gives the id of the
   processor storing the given index in the distributed dimension.
   ``myindexes(a::DArray)`` gives a tuple of the indexes owned by the local
   processor. ``convert(Array, a::DArray)`` brings all the data to one
   node.

   A ``DArray`` can be stored on a subset of the available processors.
   Three properties fully describe the distribution of ``DArray`` ``d``.
   ``d.pmap[i]`` gives the processor id that owns piece number ``i`` of the
   array. Piece ``i`` consists of indexes ``d.dist[i]`` through
   ``d.dist[i+1]-1``. ``distdim(d)`` gives the distributed dimension. For
   convenience, ``d.localpiece`` gives the number of the piece owned by the
   local processor (this could also be determined by searching ``d.pmap``).
   The array ``d.pmap`` is also available as ``procs(d)``.

   Indexing a ``DArray`` (square brackets) gathers all of the referenced
   data to a local ``Array`` object.

   Indexing a ``DArray`` with the ``sub`` function creates a "virtual"
   sub-array that leaves all of the data in place. This should be used
   where possible, especially for indexing operations that refer to large
   pieces of the original array.

   ``sub`` itself, naturally, does no communication and so is very
   efficient. However, this does not mean it should be viewed as an
   optimization in all cases. Many situations require explicitly moving
   data to the local processor in order to do a fast serial operation. For
   example, functions like matrix multiply perform many accesses to their
   input data, so it is better to have all the data available locally up
   front.

   Constructing Distributed Arrays
   -------------------------------

   The primitive ``DArray`` constructor is the function ``darray``, which
   has the following somewhat elaborate signature::

       darray(init, type, dims, distdim, procs, dist)

   ``init`` is a function of three arguments that will run on each
   processor, and should return an ``Array`` holding the local data for the
   current processor. Its arguments are ``(T,d,da)`` where ``T`` is the
   element type, ``d`` is the dimensions of the needed local piece, and
   ``da`` is the new ``DArray`` being constructed (though, of course, it is
   not fully initialized).

   ``type`` is the element type.

   ``dims`` is the dimensions of the entire ``DArray``.

   ``distdim`` is the dimension to distribute in.

   ``procs`` is a vector of processor ids to use.

   ``dist`` is a vector giving the first index of each contiguous
   distributed piece, such that the nth piece consists of indexes
   ``dist[n]`` through ``dist[n+1]-1``. If you have a vector ``v`` of the
   sizes of the pieces, ``dist`` can be computed as ``cumsum([1,v])``.

   The last three arguments are optional, and defaults will be used if they
   are omitted. The first argument, the ``init`` function, can also be
   omitted, in which case an uninitialized ``DArray`` is constructed.

   As an example, here is how to turn the local array constructor ``rand``
   into a distributed array constructor::

       drand(args...) = darray((T,d,da)->rand(d), Float64, args...)

   In this case the ``init`` function only needs to call ``rand`` with the
   dimensions of the local piece it is creating. ``drand`` accepts the same
   trailing arguments as ``darray``. ``darray`` also has definitions that
   allow functions like ``drand`` to accept the same arguments as their
   local counterparts, so calls like ``drand(m,n)`` will also work.

   The ``changedist`` function, which changes the distribution of a
   ``DArray``, can be implemented with one call to ``darray`` where the
   ``init`` function uses indexing to gather data from the existing array::

       function changedist(A::DArray, to_dist)
	   return darray((T,sz,da)->A[myindexes(da)...],
			 eltype(A), size(A), to_dist, procs(A))
       end

   It is particularly easy to construct a ``DArray`` where each block is a
   function of a block in an existing ``DArray``. This is done with the
   form ``darray(f, A)``. For example, the unary minus function can be
   implemented as::

       -(A::DArray) = darray(-, A)

   Distributed Array Computations
   ------------------------------

   Whole-array operations (e.g. elementwise operators) are a convenient way
   to use distributed arrays, but they are not always sufficient. To handle
   more complex problems, tasks can be spawned to operate on parts of a
   ``DArray`` and write the results to another ``DArray``. For example,
   here is how you could apply a function ``f`` to each 2-d slice of a 3-d
   ``DArray``::

       function compute_something(A::DArray)
	   B = darray(eltype(A), size(A), 3)
	   for i = 1:size(A,3)
	       @spawnat owner(B,i) B[:,:,i] = f(A[:,:,i])
	   end
	   B
       end

   We used ``@spawnat`` to place each operation near the memory it writes
   to.

   This code works in some sense, but trouble stems from the fact that it
   performs writes asynchronously. In other words, we don't know when the
   result data will be written to the array and become ready for further
   processing. This is known as a "race condition", one of the famous
   pitfalls of parallel programming. Some form of synchronization is
   necessary to wait for the result. As we saw above, ``@spawn`` returns a
   remote reference that can be used to wait for its computation. We could
   use that feature to wait for specific blocks of work to complete::

       function compute_something(A::DArray)
	   B = darray(eltype(A), size(A), 3)
	   deps = cell(size(A,3))
	   for i = 1:size(A,3)
	       deps[i] = @spawnat owner(B,i) B[:,:,i] = f(A[:,:,i])
	   end
	   (B, deps)
       end

   Now a function that needs to access slice ``i`` can perform
   ``wait(deps[i])`` first to make sure the data is available.

   Another option is to use a ``@sync`` block, as follows::

       function compute_something(A::DArray)
	   B = darray(eltype(A), size(A), 3)
	   @sync begin
	       for i = 1:size(A,3)
		   @spawnat owner(B,i) B[:,:,i] = f(A[:,:,i])
	       end
	   end
	   B
       end

   ``@sync`` waits for all spawns performed within it to complete. This
   makes our ``compute_something`` function easy to use, at the price of
   giving up some parallelism (since calls to it cannot overlap with
   subsequent operations).

   Still another option is to use the initial, un-synchronized version of
   the code, and place a ``@sync`` block around a larger set of operations
   in the function calling this one.

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
workloads, where we want to assign more work to processors only when
they finish their current tasks.

As an example, consider computing the singular values of matrices of
different sizes::

    M = {rand(800,800), rand(600,600), rand(800,800), rand(600,600)}
    pmap(svd, M)

If one processor handles both 800x800 matrices and another handles both
600x600 matrices, we will not get as much scalability as we could. The
solution is to make a local task to "feed" work to each processor when
it completes its current task. This can be seen in the implementation of
``pmap``::

    function pmap(f, lst)
        np = nprocs()  # determine the number of processors available
        n = length(lst)
        results = cell(n)
        i = 1
        # function to produce the next work item from the queue.
        # in this case it's just an index.
        next_idx() = (idx=i; i+=1; idx)
        @sync begin
            for p=1:np
                @spawnlocal begin
                    while true
                        idx = next_idx()
                        if idx > n
                            break
                        end
                        results[idx] = remote_call_fetch(p, f, lst[idx])
                    end
                end
            end
        end
        results
    end

``@spawnlocal`` is similar to ``@spawn``, but only runs tasks on the
local processor. We use it to create a "feeder" task for each processor.
Each task picks the next index that needs to be computed, then waits for
its processor to finish, then repeats until we run out of indexes. A
``@sync`` block is used to wait for all the local tasks to complete, at
which point the whole operation is done. Notice that all the feeder
tasks are able to share state via ``next_idx()`` since they all run on
the same processor. However, no locking is required, since the threads
are scheduled cooperatively and not preemptively. This means context
switches only occur at well-defined points (during the ``fetch``
operation).

Sending Instructions To All Processors
--------------------------------------

It is often useful to execute a statement on all processors, particularly
for setup tasks such as loading source files and defining common variables.
This can be done with the ``@everywhere`` macro:

    @everywhere include("defs.jl")
