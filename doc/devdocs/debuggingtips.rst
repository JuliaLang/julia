******************
gdb debugging tips
******************

Displaying Julia variables
--------------------------

Within ``gdb``, any ``jl_value_t*`` object ``obj`` can be displayed using
::

   (gdb) call jl_(obj)

The object will be displayed in the julia session, not in the gdb session.
This is a useful way to discover the types and values of objects being
manipulated by Julia's C code.

Similarly, if you're debugging some of julia's internals (e.g.,
``inference.jl``), you can print ``obj`` using
::

   ccall(:jl_, Void, (Any,), obj)

This is a good way to circumvent problems that arise from the order in which julia's output streams are initialized.

Inserting breakpoints for inspection from gdb
---------------------------------------------

In your ``gdb`` session, set a breakpoint in ``jl_breakpoint`` like so::

   (gdb) break jl_breakpoint

Then within your Julia code, insert a call to ``jl_breakpoint`` by adding
::

   ccall(:jl_breakpoint, Void, ())

or alternatively
::

   ccall(:jl_breakpoint, Void, (Any,), obj)

if you want to inspect ``obj`` from within ``jl_breakpoint``.

It's particularly helpful to back up to the ``jl_apply`` frame, from which you can display the arguments to a function using, e.g.,
::

   (gdb) call jl_(args[0])

Another useful frame is ``to_function(jl_lambda_info_t *li, bool cstyle)``. The ``jl_lambda_info_t*`` argument is a struct with a reference to the final AST sent into the compiler. However, the AST at this point will usually be compressed; to view the AST, call ``jl_uncompress_ast`` and then pass the result to ``jl_``::

   #2  0x00007ffff7928bf7 in to_function (li=0x2812060, cstyle=false) at codegen.cpp:584
   584	        abort();
   (gdb) p jl_(jl_uncompress_ast(li,li.ast))

Inserting breakpoints upon certain conditions
---------------------------------------------

Loading a particular file
~~~~~~~~~~~~~~~~~~~~~~~~~

Let's say the file is ``sysimg.jl``::

   (gdb) break jl_load if strcmp(fname, "sysimg.jl")==0

Calling a particular method
~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   (gdb) break jl_apply_generic if strcmp(F->name->name, "method_to_break")==0

Since this function is used for every call, you will make everything 1000x slower if you do this.

Mozilla's Record and Replay Framework (rr)
---------------------------------------------

Julia now works out of the box with `rr, <http://rr-project.org/>`_ the lightweight recording and
deterministic debugging framework from Mozilla. This allows you to replay the trace of an execution
deterministically.  The replayed execution's address spaces, register contents, syscall data etc
are exactly the same in every run.

A recent build of ``rr`` (from after 4 Feb 2015) is required.
