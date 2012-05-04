# =============================================================================
# load a source file
# =============================================================================
load("file.j")


# =============================================================================
# arrays
# =============================================================================

# defining and changing scalar and array variables
x = 3
x = [1 2 3]     # set x to a 1x3 row vector
x = [1,2,3]     # set x to a 1d vector (implicitly treated as a column)
A = [1 2 3 4; 5 6 7 8; 9 10 11 12];
    # set A to a 3x4 matrix, semicolon hides output at the prompt.
    # inside a source file no output is the default.
# line breaks may also be used:
A = [1  2  3  4
     5  6  7  8
     9 10 11 12];

x[2] = 7    # change x to [1 7 3]
A[2,1] = 0  # chage A[2,1] to 0
x[2:end]    # all of x except the first element

# matrix constructors
rand(n)     # uniform random n-element 1d vector
randn(m,n)  # normal random mxn matrix
# same for ones, zeros
diag(A)     # diagonal of matrix
diagm(x)    # construct a diagonal matrix from a vector

# cell arrays
c = {1, 2, 3}   # 1d
c = {1 2; A x}  # 2d

# solving linear equations
(L,U,P) = lu(A)

# growing 1d arrays
a={}
# ==> Array(Any,(0,))
push(a,1)
# ==> {1}
push(a,2)
# ==> {1,2}
# see also pop(), enq(), insert(), del()

# uninitialized array with a given element type and shape
Array(Int16, (2,2,3))


# =============================================================================
# function definitions, control flow
# =============================================================================

# 1. long form (for multiple statements)
function f1(x, y, n)
    s = 0
    for i=1:n
        s += x
    end
    while n < y < x   # (n<y) && (y<x)
        n *= 2
    end
    s+n    # value of last expression is returned; "return s+n" also works
end

# example of returning multiple values
# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx(a, b)
    if b == 0
        (a, 1, 0)
    else
        m = a % b
        k = div((a-m), b)   # div is integer division
        (g, x, y) = gcdx(b, m)
        (g, y, x-k*y)
    end
end

# 2. short form (for a single expression)
f(x) = 2x-1

# 3. anonymous functions
map(x->x+1, {1,2,3})     # gives {2,3,4}. "x->x+1" is a 1-argument function

()->2        # zero-argument anonymous function
(x,y)->3y-x  # two-argument anonymous function

# conditional within an expression
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

# variable number of arguments
function many_args(x, rest...)
    # "rest" contains a tuple of arguments after the first
    f(rest...)  # they can be passed on to another function like this
end


# =============================================================================
# other syntax
# =============================================================================

# multiple statements on a line
x = 1; a = 2; b = x

"string"
'A'  # character
0xFF # hexadecimal integer constant
0777 # octal integer constant

# pascal's triangle symmetric matrix using a comprehension
S(n) = [nCr(i+j-2,i-1) | i=1:n,j=1:n]

function example()
    # variable declarations
    local x::Int32   # local with declared type
    # global variable. by default, variables that are assigned to in a
    # function are local variables, and variables that are only accessed are
    # global. to assign to a global inside a function, use this declaration.
    global g
    ...

    # iterate over anything
    items = (2,4,6,8)
    for x = items
       ...
    end
end


# =============================================================================
# defining types and generic functions
# =============================================================================

type MyType
    x         # field of any type
    y::Int32  # field with declared type
end

MyType(1,2)   # construct an instance, giving values for each field

function foo(x::MyType, n::Int)  # a specialized method
    return x.x - n*x.y
end

# all operators are actually functions.
# some operators have named equivalents:
a[i,j]    # ref(a, i, j)
a[i,j]=x  # assign(a, x, i, j)
1:2       # colon(1,2)

# type conversions
convert(Int32, 2.5)
int32(2.5)  # some types have convenient conversion functions defined


# =============================================================================
# using files
# =============================================================================

f = open("file")
readline(f)  # read a line
readall(f)   # read to end of file

# writing files - 4 boolean arguments
f = open("file", read, write, create, truncate)
write(f, "hello\n")
close(f)


# =============================================================================
# external processes
# =============================================================================

run(`ls`)   # run a shell command
readall(`ls`)  # run and read all output as a string
name = "file"
run(`ls $name`)  # variables can be interpolated into shell commands


# =============================================================================
# calling a C function
# =============================================================================

libc = dlopen("libc")

function getenv(var::String)
    val = ccall(dlsym(libc, :getenv),
                Ptr{Uint8},    # return type
                (Ptr{Uint8},), # argument types
                cstring(var))  # arguments
    if val == C_NULL
        error("getenv: Undefined variable: ", var)
    end
    string(val)
end


# =============================================================================
# other data structures
# =============================================================================

h = Dict()
h["key"] = val
get(h, "key", val_if_not_found)

s = set(1,3,5,7)
has(s, n)
add(s, k)

s = intset(1,3,5,7)   # more efficient set of integers


# =============================================================================
# parallel computing
# =============================================================================

# julia uses a multi-process remote procedure call (RPC) model, with
# support for shared objects and an elastic process pool.

# julia starts with 1 process. to add processors:

addprocs_local(n)                     # using exec
addprocs_ssh({"host1","host2",...})   # using remote execution
addprocs_sge(n)                       # using Sun Grid Engine batch queue

## high-level interface

# parallel loops: "@parallel F for i=...", where F is a reduction applied to
# the results of all loop iterations. F can be omitted.

function buffon(niter)
    nc =
      @parallel (+) for i=1:niter
          rand() <= sin(rand()*pi()/2) ? 1 : 0
      end
    2/(nc/niter)
end

# parallel map
r = pmap(eig, {A, B, C, D})
# returns remote references to the results

# distributed arrays (type DArray)
A = drandn(2000,2000)  # create distributed normal random matrix

A = distribute(rand(2000,2000))  # distribute a local array

localize(A)   # return local piece of a distributed array

# general distributed array constructor
darray((T, local_size, da)->(...), ET, (rows, cols, ...), dd)
# the first argument is a function computing one piece of the array.
# its arguments are:
#   T          - array element type
#   local_size - the size of the piece to compute
#   da         - the DArray object being constructed
# second argument is the element type
# third argument is the dimensions of the array
# fourth argument is the dimension of distribution
# the 2nd and 4th arguments are optional

# simple example:
transpose{T}(a::DArray{T,2}) = darray((T,d,da)->transpose(localize(a)),
                                      T,
                                      (size(a,2),size(a,1)),
                                      (3-a.distdim))

## low-level interface

# call func(args...) on processor P, returning a remote reference
r = remote_call(P, func, args...)

# wait for a remote ref to finish being computed
wait(r)

# get the computed value (automatically waits first)
v = fetch(r)

# faster version of fetch(remote_call(...))
v = remote_call_fetch(P, func, args...)

myid()  # get my process ID

# uninitialized reference to an object that will be stored locally
r = RemoteRef()
# or on processor P
r = RemoteRef(P)

# store a value to an uninitialized remote reference
put(r, value)

# create a partitioned global object
# This is one object shared by a set of processes, each of which stores a
# local piece. This is the fundamental abstraction DArray is built on.
g = GlobalObject(g->(...))
# The argument is a function that will run on each processor, and returns
# the local part for its processor. Its argument g is the GlobalObject
# being constructed.
# The GlobalObject itself acts like a pointer in a shared address space;
# it can be passed to any processor and still reference the same logical
# object.
# RemoteRefs also reference the same object from anywhere, but they
# refer to a single local object stored on one processor.
# All other objects passed to remote_call() are copied.
# A GlobalObject is basically a clique of RemoteRefs, if that helps.


# =============================================================================
# Tasks (AKA coroutines, or lightweight threads)
# =============================================================================

## using a Task as a generator

function generator()
    produce("start")
    for i=1:10
        produce(2i)
    end
    produce("stop")
end

for x = Task(generator)
    # this will loop over each value produced by the generator.
    # execution of the generator is suspended between its calls to produce()
    println(x)
end

## low-level Task interface

t = Task(f)   # create a task to run function f

# switch to task t, passing it the given arguments. If the task has not
# started yet, the arguments are passed to function f. Otherwise,
# the argsin are returned from Task t's last call to yieldto().
argsout,... = yieldto(t, argsin...)

# get the currently running Task
t = current_task()

# the current_task when t was created
t.parent
# when f returns value x, t performs yieldto(t.parent, x)

# see if a task has finished. once a task finishes, yielding to it continues
# to return the result of function f.
task_done(t)
