# Working with LLVM

This is not a replacement for the LLVM documentation, but a collection of tips for working on
LLVM for Julia.

## Overview of Julia to LLVM Interface

Julia statically links in LLVM by default. Build with `USE_LLVM_SHLIB=1` to link dynamically.

The code for lowering Julia AST to LLVM IR or interpreting it directly is in directory `src/`.

| File                | Description                                                |
|:------------------- |:---------------------------------------------------------- |
| `builtins.c`        | Builtin functions                                          |
| `ccall.cpp`         | Lowering `ccall`                                           |
| `cgutils.cpp`       | Lowering utilities, notably for array and tuple accesses   |
| `codegen.cpp`       | Top-level of code generation, pass list, lowering builtins |
| `debuginfo.cpp`     | Tracks debug information for JIT code                      |
| `disasm.cpp`        | Handles native object file and JIT code diassembly         |
| `gf.c`              | Generic functions                                          |
| `intrinsics.cpp`    | Lowering intrinsics                                        |
| `llvm-simdloop.cpp` | Custom LLVM pass for `@simd`                               |
| `sys.c`             | I/O and operating system utility functions                 |

Some of the `.cpp` files form a group that compile to a single object.

The difference between an intrinsic and a builtin is that a builtin is a first class function
that can be used like any other Julia function.  An intrinsic can operate only on unboxed data,
and therefore its arguments must be statically typed.

### Alias Analysis

Julia currently uses LLVM's [Type Based Alias Analysis](http://llvm.org/docs/LangRef.html#tbaa-metadata).
To find the comments that document the inclusion relationships, look for `static MDNode*` in
`src/codegen.cpp`.

The `-O` option enables LLVM's [Basic Alias Analysis](http://llvm.org/docs/AliasAnalysis.html#the-basicaa-pass).

## Building Julia with a different version of LLVM

The default version of LLVM is specified in `deps/Versions.make`. You can override it by creating
a file called `Make.user` in the top-level directory and adding a line to it such as:

```
LLVM_VER = 3.5.0
```

Besides the LLVM release numerals, you can also use `LLVM_VER = svn` to bulid against the latest
development version of LLVM.

## Passing options to LLVM

You can pass options to LLVM using *debug* builds of Julia.  To create a debug build, run `make debug`.
 The resulting executable is `usr/bin/julia-debug`. You can pass LLVM options to this executable
via the environment variable `JULIA_LLVM_ARGS`. Here are example settings using `bash` syntax:

  * `export JULIA_LLVM_ARGS = -print-after-all` dumps IR after each pass.
  * `export JULIA_LLVM_ARGS = -debug-only=loop-vectorize` dumps LLVM `DEBUG(...)` diagnostics for
    loop vectorizer *if* you built Julia with `LLVM_ASSERTIONS=1`. Otherwise you will get warnings
    about "Unknown command line argument". Counter-intuitively, building Julia with `LLVM_DEBUG=1`
    is *not* enough to dump `DEBUG` diagnostics from a pass.

## Debugging LLVM transformations in isolation

On occasion, it can be useful to debug LLVM's transformations in isolation from
the rest of the julia system, e.g. because reproducing the issue inside julia
would take too long, or because one wants to take advantage of LLVM's tooling
(e.g. bugpoint). To get unoptimized IR for the entire system iamge, pass the
`--output-unopt-bc unopt.bc` option to the system image build process, which will
output the unoptimized IR to an `unopt.bc` file. This file can then be passed to
LLVM tools as usual. `libjulia` can function as an LLVM pass plugin and can be
loaded into LLVM tools, to make julia-specific passes available in this
environment. In addition, it exposes the `-julia` meta-pass, which runs the
entire julia pass-pipeline over the IR. As an example, to generate a system
image, one could do:
```
opt -load libjulia.so -julia -o opt.bc unopt.bc
llc -o sys.o opt.bc
cc -shared -o sys.so sys.o
```
This system image can then be loaded by julia as usual.

It is also possible to dump an LLVM IR module for just one julia function,
using:
```
f, T = +, Tuple{Int,Int} # Substitute your function of interest here
optimize = false
open("plus.ll","w") do f
    println(f, Base._dump_function(f, T, false, false, false, true, :att, optimize))
end
```
These files can be processed the same way as the unoptimized sysimg IR shown
above.

## Improving LLVM optimizations for Julia

Improving LLVM code generation usually involves either changing Julia lowering to be more friendly
to LLVM's passes, or improving a pass.

If you are planning to improve a pass, be sure to read the [LLVM developer policy](http://llvm.org/docs/DeveloperPolicy.html).
The best strategy is to create a code example in a form where you can use LLVM's `opt` tool to
study it and the pass of interest in isolation.

1. Create an example Julia code of interest.
2. Use `JULIA_LLVM_ARGS = -print-after-all` to dump the IR.
3. Pick out the IR at the point just before the pass of interest runs.
4. Strip the debug metadata and fix up the TBAA metadata by hand.

The last step is labor intensive.  Suggestions on a better way would be appreciated.

## GC root placement

GC root placement is done by an LLVM late in the pass pipeline. Doing GC root
placement this late enables LLVM to make more aggressive optimizations around
code that requires GC roots, as well as allowing us to reduce the number of
required GC roots and GC root store operations (since LLVM doesn't understand
our GC, it wouldn't otherwise know what it is and is not allowed to do with
values stored to the GC frame, so it'll conservatively do very little). As an
example, consider an error path
```
    if some_condition()
        #= Use some variables maybe =#
        error("An error occurred")
    end
```
During constant folding, LLVM may discover that the condition is always false,
and can remove the basic block. However, if GC root lowering is done early,
the GC root slots used in the deleted block, as well as any values kept alive
in those slots only because they were used in the error path, would be kept
alive by LLVM. By doing GC root lowering late, we give LLVM the license to do
any of its usual optimizations (constant folding, dead code elimination, etc.),
without having to worry (too much) about which values may or may not be gc
tracked.

However, in order to be able to do late GC root placement, we need to be able to
identify a) which pointers are gc tracked and b) all uses of such pointers. The
goal of the GC placement pass is thus simple:

Minimize the number of needed gc roots/stores to them subject to the constraint
that at every safepoint, any live gc-tracked pointer (i.e. for which there is
a path after this point that contains a use of this pointer) is in some gc slot.

### Representation

The primary difficulty is thus choosing an IR representation that allows us to
identify gc-tracked pointers and their uses, even after the program has been
run through the optimizer. Our design makes use of three LLVM features to achieve
this:
- Custom address spaces
- Operand Bundles
- non-integral pointers

Custom address spaces allow us to tag every point with an integer that needs
to be preserved through optimizations. The compiler may not insert casts between
address spaces that did not exist in the original program and it must never
change the address space of a pointer on a load/store/etc operation. This allows
us to annotate which pointers are gc-tracked in an optimizer-resistant way. Note
that metadata would not be able to achieve the same purpose. Metadata is supposed
to always be discardable without altering the semantics of the program. However,
failing to identify a gc-tracked pointer alters the resulting program behavior
dramatically - it'll probably crash or return wrong results. We currently use
three different addressspaces (their numbers are defined in src/codegen_shared.cpp):

- GC Tracked Pointers (currently 10): These are pointers to boxed values that may be put
  into a GC frame. It is loosely equivalent to a `jl_value_t*` pointer on the C
  side. N.B. It is illegal to ever have a pointer in this address space that may
  not be stored to a GC slot.
- Derived Pointers (currently 11): These are pointers that are derived from some GC
  tracked pointer. Uses of these pointers generate uses of the original pointer.
  However, they need not themselves be known to the GC. The GC root placement
  pass MUST always find the GC tracked pointer from which this pointer is
  derived and use that as the pointer to root.
- Callee Rooted Pointers (currently 12): This is a utility address space to express the
  notion of a callee rooted value. All values of this address space MUST be
  storable to a GC root (though it is possible to relax this condition in the
  future), but unlike the other pointers need not be rooted if passed to a
  call (they do still need to be rooted if they are live across another safepoint
  between the definition and the call).

### Invariants.
The GC root placement pass makes use of several invariants, which need
to be observed by the frontend and are preserved by the optimizer.

First, only the following addressspace casts are allowed
- 0->{Tracked,Derived,CalleeRooted}: It is allowable to decay an untracked pointer to any of the
  other. However, do note that the optimizer has broad license to not root
  such a value. It is never safe to have a value in addressspace 0 in any part
  of the program if it is (or is derived from) a value that requires a GC root.
- Tracked->Derived: This is the standard decay route for interior values. The placement
  pass will look for these to identify the base pointer for any use.
- Tracked->CalleeRooted: Addrspace CalleeRooted serves merely as a hint that a GC root is not
  required. However, do note that the Derived->CalleeRooted decay is prohibited, since
  pointers should generally be storable to a GC slot, even in this address space.

Now let us consider what constitutes a use:
- Loads whose loaded values is in one of the address spaces
- Stores of a value in one of the address spaces to a location
- Stores to a pointer in one of the address spaces
- Calls for which a value in one of the address spaces is an operand
- Calls in jlcall ABI, for which the argument array contains a value
- Return instructions.

We explicitly allow load/stores and simple calls in address spaces Tracked/Derived. Elements of jlcall
argument arrays must always be in address space Tracked (it is required by the ABI that
they are valid `jl_value_t*` pointers). The same is true for return instructions
(though note that struct return arguments are allowed to have any of the address
spaces). The only allowable use of an address space CalleRooted pointer is to pass it to
a call (which must have an appropriately typed operand).

Further, we disallow getelementptr in addrspace Tracked. This is because unless
the operation is a noop, the resulting pointer will not be validly storable
to a GC slot and may thus not be in this address space. If such a pointer
is required, it should be decayed to addrspace Derived first.

Lastly, we disallow inttoptr/ptrtoint instructions in these address spaces.
Having these instructions would mean that some i64 values are really gc tracked.
This is problematic, because it breaks that stated requirement that we're able
to identify gc-relevant pointers. This invariant is accomplished using the LLVM
"non-integral pointers" feature, which is new in LLVM 5.0. It prohibits the
optimizer from making optimizations that would introduce these operations. Note
we can still insert static constants at JIT time by using inttoptr in address
space 0 and then decaying to the appropriate address space afterwards.

### Supporting ccall
One important aspect missing from the discussion so far is the handling of
`ccall`. `ccall` has the peculiar feature that the location and scope of a use
do not coincide. As an example consider:
```
A = randn(1024)
ccall(:foo, Void, (Ptr{Float64},), A)
```
In lowering, the compiler will insert a conversion from the array to the
pointer which drops the reference to the array value. However, we of course
need to make sure that the array does stay alive while we're doing the ccall.
To understand how this is done, first recall the lowering of the above code:
```
    return $(Expr(:foreigncall, :(:foo), Void, svec(Ptr{Float64}), :($(Expr(:foreigncall, :(:jl_array_ptr), Ptr{Float64}, svec(Any), :(A), 0))), :(A)))
```
The last `:(A)`, is an extra argument list inserted during lowering that informs
the code generator which julia level values need to be kept alive for the
duration of this ccall. We then take this information and represent it in an
"operand bundle" at the IR level. An operand bundle is essentially a fake use
that is attached to the call site. At the IR level, this looks like so:
```
    call void inttoptr (i64 ... to void (double*)*)(double* %5) [ "jl_roots"(%jl_value_t addrspace(10)* %A) ]
```
The GC root placement pass will treat the jl_roots operand bundle as if it were
a regular operand. However, as a final step, after the gc roots are inserted,
it will drop the operand bundle to avoid confusing instruction selection.

### Supporting pointer_from_objref
`pointer_from_objref` is special because it requires the user to take explicit
control of GC rooting. By our above invariants, this function is illegal,
because it performs an addressspace cast from 10 to 0. However, it can be useful,
in certain situations, so we provide a special intrinsic:
```
declared %jl_value_t *julia.pointer_from_objref(%jl_value_t addrspace(10)*)
```
which is lowered to the corresponding address space cast after gc root lowering.
Do note however that by using this intrinsic, the caller assumes all responsibility
for making sure that the value in question is rooted. Further this intrinsic is
not considered a use, so the GC root placement pass will not provide a GC root
for the function. As a result, the external rooting must be arranged while the
value is still tracked by the system. I.e. it is not valid to attempt use the
result of this operation to establish a global root - the optimizer may have
already dropped the value.
