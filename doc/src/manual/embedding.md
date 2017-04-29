# Embedding Julia

As we have seen in [Calling C and Fortran Code](@ref), Julia has a simple and efficient way to
call functions written in C. But there are situations where the opposite is needed: calling Julia
function from C code. This can be used to integrate Julia code into a larger C/C++ project, without
the need to rewrite everything in C/C++. Julia has a C API to make this possible. As almost all
programming languages have some way to call C functions, the Julia C API can also be used to build
further language bridges (e.g. calling Julia from Python or C#).

## High-Level Embedding

We start with a simple C program that initializes Julia and calls some Julia code:

```
#include <julia.h>

int main(int argc, char *argv[])
{
    /* required: setup the Julia context */
    jl_init(NULL);

    /* run Julia commands */
    jl_eval_string("print(sqrt(2.0))");

    /* strongly recommended: notify Julia that the
         program is about to terminate. this allows
         Julia time to cleanup pending write requests
         and run all finalizers
    */
    jl_atexit_hook(0);
    return 0;
}
```

In order to build this program you have to put the path to the Julia header into the include path
and link against `libjulia`. For instance, when Julia is installed to `$JULIA_DIR`, one can compile
the above test program `test.c` with `gcc` using:

```
gcc -o test -fPIC -I$JULIA_DIR/include/julia -L$JULIA_DIR/lib test.c -ljulia $JULIA_DIR/lib/julia/libstdc++.so.6
```

Then if the environment variable `JULIA_HOME` is set to `$JULIA_DIR/bin`, the output `test` program
can be executed.

Alternatively, look at the `embedding.c` program in the Julia source tree in the `examples/` folder.
The file `ui/repl.c` program is another simple example of how to set `jl_options` options while
linking against `libjulia`.

The first thing that has to be done before calling any other Julia C function is to initialize
Julia. This is done by calling `jl_init`, which takes as argument a C string (`const char*`) to
the location where Julia is installed. When the argument is `NULL`, Julia tries to determine the
install location automatically.

The second statement in the test program evaluates a Julia statement using a call to `jl_eval_string`.

Before the program terminates, it is strongly recommended to call `jl_atexit_hook`.  The above
example program calls this before returning from `main`.

!!! note
    Currently, dynamically linking with the `libjulia` shared library requires passing the `RTLD_GLOBAL`
    option. In Python, this looks like:

    ```
    >>> julia=CDLL('./libjulia.dylib',RTLD_GLOBAL)
    >>> julia.jl_init.argtypes = [c_char_p]
    >>> julia.jl_init('.')
    250593296
    ```

!!! note
    If the julia program needs to access symbols from the main executable, it may be necessary to
    add `-Wl,--export-dynamic` linker flag at compile time on Linux in addition to the ones generated
    by `julia-config.jl` described below. This is not necessary when compiling a shared library.

### Using julia-config to automatically determine build parameters

The script *julia-config.jl* was created to aid in determining what build parameters are required
by a program that uses embedded Julia.  This script uses the build parameters and system configuration
of the particular Julia distribution it is invoked by to export the necessary compiler flags for
an embedding program to interact with that distribution.  This script is located in the Julia
shared data directory.

#### Example

Below is essentially the same as above with one small change; the argument to `jl_init` is now
**JULIA_INIT_DIR** which is defined by *julia-config.jl*.:

```
#include <julia.h>

int main(int argc, char *argv[])
{
   jl_init(JULIA_INIT_DIR);
   (void)jl_eval_string("println(sqrt(2.0))");
   jl_atexit_hook(0);
   return 0;
}
```

#### On the command line

A simple use of this script is from the command line.  Assuming that *julia-config.jl* is located
in */usr/local/julia/share/julia*, it can be invoked on the command line directly and takes any
combination of 3 flags:

```
/usr/local/julia/share/julia/julia-config.jl
Usage: julia-config [--cflags|--ldflags|--ldlibs]
```

If the above example source is saved in the file *embed_example.c*, then the following command
will compile it into a running program on Linux and Windows (MSYS2 environment), or if on OS/X,
then substitute `clang` for `gcc`.:

```
/usr/local/julia/share/julia/julia-config.jl --cflags --ldflags --ldlibs | xargs gcc embed_example.c
```

#### Use in Makefiles

But in general, embedding projects will be more complicated than the above, and so the following
allows general makefile support as well – assuming GNU make because of the use of the **shell**
macro expansions.  Additionally, though many times *julia-config.jl* may be found in the directory
*/usr/local*, this is not necessarily the case, but Julia can be used to locate *julia-config.jl*
too, and the makefile can be used to take advantage of that.  The above example is extended to
use a Makefile:

```
JL_SHARE = $(shell julia -e 'print(joinpath(JULIA_HOME,Base.DATAROOTDIR,"julia"))')
CFLAGS   += $(shell $(JL_SHARE)/julia-config.jl --cflags)
CXXFLAGS += $(shell $(JL_SHARE)/julia-config.jl --cflags)
LDFLAGS  += $(shell $(JL_SHARE)/julia-config.jl --ldflags)
LDLIBS   += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)

all: embed_example
```

Now the build command is simply **make**.

## Converting Types

Real applications will not just need to execute expressions, but also return their values to the
host program. `jl_eval_string` returns a `jl_value_t*`, which is a pointer to a heap-allocated
Julia object. Storing simple data types like `Float64` in this way is called `boxing`, and extracting
the stored primitive data is called `unboxing`. Our improved sample program that calculates the
square root of 2 in Julia and reads back the result in C looks as follows:

```
jl_value_t *ret = jl_eval_string("sqrt(2.0)");

if (jl_is_float64(ret)) {
    double ret_unboxed = jl_unbox_float64(ret);
    printf("sqrt(2.0) in C: %e \n", ret_unboxed);
}
```

In order to check whether `ret` is of a specific Julia type, we can use the `jl_is_...` functions.
By typing `typeof(sqrt(2.0))` into the Julia shell we can see that the return type is `Float64`
(`double` in C). To convert the boxed Julia value into a C double the `jl_unbox_float64` function
is used in the above code snippet.

Corresponding `jl_box_...` functions are used to convert the other way:

```julia
jl_value_t *a = jl_box_float64(3.0);
jl_value_t *b = jl_box_float32(3.0f);
jl_value_t *c = jl_box_int32(3);
```

As we will see next, boxing is required to call Julia functions with specific arguments.

## Calling Julia Functions

While `jl_eval_string` allows C to obtain the result of a Julia expression, it does not allow
passing arguments computed in C to Julia. For this you will need to invoke Julia functions directly,
using `jl_call`:

```julia
jl_function_t *func = jl_get_function(jl_base_module, "sqrt");
jl_value_t *argument = jl_box_float64(2.0);
jl_value_t *ret = jl_call1(func, argument);
```

In the first step, a handle to the Julia function `sqrt` is retrieved by calling `jl_get_function`.
The first argument passed to `jl_get_function` is a pointer to the `Base` module in which `sqrt`
is defined. Then, the double value is boxed using `jl_box_float64`. Finally, in the last step,
the function is called using `jl_call1`. `jl_call0`, `jl_call2`, and `jl_call3` functions also
exist, to conveniently handle different numbers of arguments. To pass more arguments, use `jl_call`:

```
jl_value_t *jl_call(jl_function_t *f, jl_value_t **args, int32_t nargs)
```

Its second argument `args` is an array of `jl_value_t*` arguments and `nargs` is the number of
arguments.

## Memory Management

As we have seen, Julia objects are represented in C as pointers. This raises the question of who
is responsible for freeing these objects.

Typically, Julia objects are freed by a garbage collector (GC), but the GC does not automatically
know that we are holding a reference to a Julia value from C. This means the GC can free objects
out from under you, rendering pointers invalid.

The GC can only run when Julia objects are allocated. Calls like `jl_box_float64` perform allocation,
and allocation might also happen at any point in running Julia code. However, it is generally
safe to use pointers in between `jl_...` calls. But in order to make sure that values can survive
`jl_...` calls, we have to tell Julia that we hold a reference to a Julia value. This can be done
using the `JL_GC_PUSH` macros:

```
jl_value_t *ret = jl_eval_string("sqrt(2.0)");
JL_GC_PUSH1(&ret);
// Do something with ret
JL_GC_POP();
```

The `JL_GC_POP` call releases the references established by the previous `JL_GC_PUSH`. Note that
`JL_GC_PUSH`  is working on the stack, so it must be exactly paired with a `JL_GC_POP` before
the stack frame is destroyed.

Several Julia values can be pushed at once using the `JL_GC_PUSH2` , `JL_GC_PUSH3` , and `JL_GC_PUSH4`
macros. To push an array of Julia values one can use the  `JL_GC_PUSHARGS` macro, which can be
used as follows:

```
jl_value_t **args;
JL_GC_PUSHARGS(args, 2); // args can now hold 2 `jl_value_t*` objects
args[0] = some_value;
args[1] = some_other_value;
// Do something with args (e.g. call jl_... functions)
JL_GC_POP();
```

The garbage collector also operates under the assumption that it is aware of every old-generation
object pointing to a young-generation one. Any time a pointer is updated breaking that assumption,
it must be signaled to the collector with the `jl_gc_wb` (write barrier) function like so:

```
jl_value_t *parent = some_old_value, *child = some_young_value;
((some_specific_type*)parent)->field = child;
jl_gc_wb(parent, child);
```

It is in general impossible to predict which values will be old at runtime, so the write barrier
must be inserted after all explicit stores. One notable exception is if the `parent` object was
just allocated and garbage collection was not run since then. Remember that most `jl_...` functions
can sometimes invoke garbage collection.

The write barrier is also necessary for arrays of pointers when updating their data directly.
For example:

```
jl_array_t *some_array = ...; // e.g. a Vector{Any}
void **data = (void**)jl_array_data(some_array);
jl_value_t *some_value = ...;
data[0] = some_value;
jl_gc_wb(some_array, some_value);
```

### Manipulating the Garbage Collector

There are some functions to control the GC. In normal use cases, these should not be necessary.

| Function             | Description                                  |
|:-------------------- |:-------------------------------------------- |
| `jl_gc_collect()`    | Force a GC run                               |
| `jl_gc_enable(0)`    | Disable the GC, return previous state as int |
| `jl_gc_enable(1)`    | Enable the GC,  return previous state as int |
| `jl_gc_is_enabled()` | Return current state as int                  |

## Working with Arrays

Julia and C can share array data without copying. The next example will show how this works.

Julia arrays are represented in C by the datatype `jl_array_t*`. Basically, `jl_array_t` is a
struct that contains:

  * Information about the datatype
  * A pointer to the data block
  * Information about the sizes of the array

To keep things simple, we start with a 1D array. Creating an array containing Float64 elements
of length 10 is done by:

```julia
jl_value_t* array_type = jl_apply_array_type(jl_float64_type, 1);
jl_array_t* x          = jl_alloc_array_1d(array_type, 10);
```

Alternatively, if you have already allocated the array you can generate a thin wrapper around
its data:

```
double *existingArray = (double*)malloc(sizeof(double)*10);
jl_array_t *x = jl_ptr_to_array_1d(array_type, existingArray, 10, 0);
```

The last argument is a boolean indicating whether Julia should take ownership of the data. If
this argument is non-zero, the GC will call `free` on the data pointer when the array is no longer
referenced.

In order to access the data of x, we can use `jl_array_data`:

```
double *xData = (double*)jl_array_data(x);
```

Now we can fill the array:

```
for(size_t i=0; i<jl_array_len(x); i++)
    xData[i] = i;
```

Now let us call a Julia function that performs an in-place operation on `x`:

```
jl_function_t *func  = jl_get_function(jl_base_module, "reverse!");
jl_call1(func, (jl_value_t*)x);
```

By printing the array, one can verify that the elements of `x` are now reversed.

### Accessing Returned Arrays

If a Julia function returns an array, the return value of `jl_eval_string` and `jl_call` can be
cast to a `jl_array_t*`:

```
jl_function_t *func  = jl_get_function(jl_base_module, "reverse");
jl_array_t *y = (jl_array_t*)jl_call1(func, (jl_value_t*)x);
```

Now the content of `y` can be accessed as before using `jl_array_data`. As always, be sure to
keep a reference to the array while it is in use.

### Multidimensional Arrays

Julia's multidimensional arrays are stored in memory in column-major order. Here is some code
that creates a 2D array and accesses its properties:

```
// Create 2D array of float64 type
jl_value_t *array_type = jl_apply_array_type(jl_float64_type, 2);
jl_array_t *x  = jl_alloc_array_2d(array_type, 10, 5);

// Get array pointer
double *p = (double*)jl_array_data(x);
// Get number of dimensions
int ndims = jl_array_ndims(x);
// Get the size of the i-th dim
size_t size0 = jl_array_dim(x,0);
size_t size1 = jl_array_dim(x,1);

// Fill array with data
for(size_t i=0; i<size1; i++)
    for(size_t j=0; j<size0; j++)
        p[j + size0*i] = i + j;
```

Notice that while Julia arrays use 1-based indexing, the C API uses 0-based indexing (for example
in calling `jl_array_dim`) in order to read as idiomatic C code.

## Exceptions

Julia code can throw exceptions. For example, consider:

```julia
jl_eval_string("this_function_does_not_exist()");
```

This call will appear to do nothing. However, it is possible to check whether an exception was
thrown:

```julia
if (jl_exception_occurred())
    printf("%s \n", jl_typeof_str(jl_exception_occurred()));
```

If you are using the Julia C API from a language that supports exceptions (e.g. Python, C#, C++),
it makes sense to wrap each call into `libjulia` with a function that checks whether an exception
was thrown, and then rethrows the exception in the host language.

### Throwing Julia Exceptions

When writing Julia callable functions, it might be necessary to validate arguments and throw exceptions
to indicate errors. A typical type check looks like:

```
if (!jl_is_float64(val)) {
    jl_type_error(function_name, (jl_value_t*)jl_float64_type, val);
}
```

General exceptions can be raised using the functions:

```
void jl_error(const char *str);
void jl_errorf(const char *fmt, ...);
```

`jl_error` takes a C string, and `jl_errorf` is called like `printf`:

```julia
jl_errorf("argument x = %d is too large", x);
```

where in this example `x` is assumed to be an integer.
