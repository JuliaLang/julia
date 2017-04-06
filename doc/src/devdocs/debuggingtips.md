# gdb debugging tips

## Displaying Julia variables

Within `gdb`, any `jl_value_t*` object `obj` can be displayed using

```
(gdb) call jl_(obj)
```

The object will be displayed in the `julia` session, not in the gdb session. This is a useful
way to discover the types and values of objects being manipulated by Julia's C code.

Similarly, if you're debugging some of Julia's internals (e.g., `inference.jl`), you can print
`obj` using

```julia
ccall(:jl_, Void, (Any,), obj)
```

This is a good way to circumvent problems that arise from the order in which julia's output streams
are initialized.

Julia's flisp interpreter uses `value_t` objects; these can be displayed with `call fl_print(fl_ctx, ios_stdout, obj)`.

## Useful Julia variables for Inspecting

While the addresses of many variables, like singletons, can be be useful to print for many failures,
there are a number of additional variables (see julia.h for a complete list) that are even more
useful.

  * (when in `jl_apply_generic`) `f->linfo` and `jl_uncompress_ast(f->linfo, f->linfo->ast)` :: for
    figuring out a bit about the call-stack
  * `jl_lineno` and `jl_filename` :: for figuring out what line in a test to go start debugging from
    (or figure out how far into a file has been parsed)
  * `$1` :: not really a variable, but still a useful shorthand for referring to the result of the
    last gdb command (such as `print`)
  * `jl_options` :: sometimes useful, since it lists all of the command line options that were successfully
    parsed
  * `jl_uv_stderr` :: because who doesn't like to be able to interact with stdio

## Useful Julia functions for Inspecting those variables

  * `jl_gdblookup($rip)` :: For looking up the current function and line. (use `$eip` on i686 platforms)
  * `jlbacktrace()` :: For dumping the current Julia backtrace stack to stderr. Only usable after
    `record_backtrace()` has been called.
  * `jl_dump_llvm_value(Value*)` :: For invoking `Value->dump()` in gdb, where it doesn't work natively.
    For example, `f->linfo->functionObject`, `f->linfo->specFunctionObject`, and `to_function(f->linfo)`.
  * `Type->dump()` :: only works in lldb. Note: add something like `;1` to prevent lldb from printing
    its prompt over the output
  * `jl_eval_string("expr")` :: for invoking side-effects to modify the current state or to lookup
    symbols
  * `jl_typeof(jl_value_t*)` :: for extracting the type tag of a Julia value (in gdb, call `macro define jl_typeof jl_typeof`
    first, or pick something short like `ty` for the first arg to define a shorthand)

## Inserting breakpoints for inspection from gdb

In your `gdb` session, set a breakpoint in `jl_breakpoint` like so:

```
(gdb) break jl_breakpoint
```

Then within your Julia code, insert a call to `jl_breakpoint` by adding

```julia
ccall(:jl_breakpoint, Void, (Any,), obj)
```

where `obj` can be any variable or tuple you want to be accessible in the breakpoint.

It's particularly helpful to back up to the `jl_apply` frame, from which you can display the arguments
to a function using, e.g.,

```
(gdb) call jl_(args[0])
```

Another useful frame is `to_function(jl_method_instance_t *li, bool cstyle)`. The `jl_method_instance_t*`
argument is a struct with a reference to the final AST sent into the compiler. However, the AST
at this point will usually be compressed; to view the AST, call `jl_uncompress_ast` and then pass
the result to `jl_`:

```
#2  0x00007ffff7928bf7 in to_function (li=0x2812060, cstyle=false) at codegen.cpp:584
584          abort();
(gdb) p jl_(jl_uncompress_ast(li, li->ast))
```

## Inserting breakpoints upon certain conditions

### Loading a particular file

Let's say the file is `sysimg.jl`:

```
(gdb) break jl_load if strcmp(fname, "sysimg.jl")==0
```

### Calling a particular method

```
(gdb) break jl_apply_generic if strcmp((char*)(jl_symbol_name)(jl_gf_mtable(F)->name), "method_to_break")==0
```

Since this function is used for every call, you will make everything 1000x slower if you do this.

## Dealing with signals

Julia requires a few signal to function property. The profiler uses `SIGUSR2` for sampling and
the garbage collector uses `SIGSEGV` for threads synchronization. If you are debugging some code
that uses the profiler or multiple threads, you may want to let the debugger ignore these signals
since they can be triggered very often during normal operations. The command to do this in GDB
is (replace `SIGSEGV` with `SIGUSRS` or other signals you want to ignore):

```
(gdb) handle SIGSEGV noprint nostop pass
```

The corresponding LLDB command is (after the process is started):

```
(lldb) pro hand -p true -s false -n false SIGSEGV
```

If you are debugging a segfault with threaded code, you can set a breakpoint on `jl_critical_error`
(`sigdie_handler` should also work on Linux and BSD) in order to only catch the actual segfault
rather than the GC synchronization points.

## Debugging during Julia's build process (bootstrap)

Errors that occur during `make` need special handling. Julia is built in two stages, constructing
`sys0` and `sys.ji`. To see what commands are running at the time of failure, use `make VERBOSE=1`.

At the time of this writing, you can debug build errors during the `sys0` phase from the `base`
directory using:

```
julia/base$ gdb --args ../usr/bin/julia-debug -C native --build ../usr/lib/julia/sys0 sysimg.jl
```

You might need to delete all the files in `usr/lib/julia/` to get this to work.

You can debug the `sys.ji` phase using:

```
julia/base$ gdb --args ../usr/bin/julia-debug -C native --build ../usr/lib/julia/sys -J ../usr/lib/julia/sys0.ji sysimg.jl
```

By default, any errors will cause Julia to exit, even under gdb. To catch an error "in the act",
set a breakpoint in `jl_error` (there are several other useful spots, for specific kinds of failures,
including: `jl_too_few_args`, `jl_too_many_args`, and `jl_throw`).

Once an error is caught, a useful technique is to walk up the stack and examine the function by
inspecting the related call to `jl_apply`. To take a real-world example:

```
Breakpoint 1, jl_throw (e=0x7ffdf42de400) at task.c:802
802 {
(gdb) p jl_(e)
ErrorException("auto_unbox: unable to determine argument type")
$2 = void
(gdb) bt 10
#0  jl_throw (e=0x7ffdf42de400) at task.c:802
#1  0x00007ffff65412fe in jl_error (str=0x7ffde56be000 <_j_str267> "auto_unbox:
   unable to determine argument type")
   at builtins.c:39
#2  0x00007ffde56bd01a in julia_convert_16886 ()
#3  0x00007ffff6541154 in jl_apply (f=0x7ffdf367f630, args=0x7fffffffc2b0, nargs=2) at julia.h:1281
...
```

The most recent `jl_apply` is at frame #3, so we can go back there and look at the AST for the
function `julia_convert_16886`. This is the uniqued name for some method of `convert`. `f` in
this frame is a `jl_function_t*`, so we can look at the type signature, if any, from the `specTypes`
field:

```
(gdb) f 3
#3  0x00007ffff6541154 in jl_apply (f=0x7ffdf367f630, args=0x7fffffffc2b0, nargs=2) at julia.h:1281
1281            return f->fptr((jl_value_t*)f, args, nargs);
(gdb) p f->linfo->specTypes
$4 = (jl_tupletype_t *) 0x7ffdf39b1030
(gdb) p jl_( f->linfo->specTypes )
Tuple{Type{Float32}, Float64}           # <-- type signature for julia_convert_16886
```

Then, we can look at the AST for this function:

```
(gdb) p jl_( jl_uncompress_ast(f->linfo, f->linfo->ast) )
Expr(:lambda, Array{Any, 1}[:#s29, :x], Array{Any, 1}[Array{Any, 1}[], Array{Any, 1}[Array{Any, 1}[:#s29, :Any, 0], Array{Any, 1}[:x, :Any, 0]], Array{Any, 1}[], 0], Expr(:body,
Expr(:line, 90, :float.jl)::Any,
Expr(:return, Expr(:call, :box, :Float32, Expr(:call, :fptrunc, :Float32, :x)::Any)::Any)::Any)::Any)::Any
```

Finally, and perhaps most usefully, we can force the function to be recompiled in order to step
through the codegen process. To do this, clear the cached `functionObject` from the `jl_lamdbda_info_t*`:

```
(gdb) p f->linfo->functionObject
$8 = (void *) 0x1289d070
(gdb) set f->linfo->functionObject = NULL
```

Then, set a breakpoint somewhere useful (e.g. `emit_function`, `emit_expr`, `emit_call`, etc.),
and run codegen:

```
(gdb) p jl_compile(f)
... # your breakpoint here
```

## Debugging precompilation errors

Module precompilation spawns a separate Julia process to precompile each module. Setting a breakpoint
or catching failures in a precompile worker requires attaching a debugger to the worker. The easiest
approach is to set the debugger watch for new process launches matching a given name. For example:

```
(gdb) attach -w -n julia-debug
```

or:

```
(lldb) process attach -w -n julia-debug
```

Then run a script/command to start precompilation. As described earlier, use conditional breakpoints
in the parent process to catch specific file-loading events and narrow the debugging window. (some
operating systems may require alternative approaches, such as following each `fork` from the parent
process)

## Mozilla's Record and Replay Framework (rr)

Julia now works out of the box with [rr,](http://rr-project.org/) the lightweight recording and
deterministic debugging framework from Mozilla. This allows you to replay the trace of an execution
deterministically.  The replayed execution's address spaces, register contents, syscall data etc
are exactly the same in every run.

A recent version of `rr` (3.1.0 or higher) is required.
