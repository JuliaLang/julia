profile.jl --- A simple profiler for Julia
==========================================

.. .. module:: profile.jl
   :synopsis: Allows you to determine running times for each line of code.

.. function:: @profile

   Profiling is controlled via the ``@profile`` macro. Your first step is to determine which code you want to profile and encapsulate it inside a ``@profile begin ... end`` block, like this::

     @profile begin
     function f1(x::Int)
       z = 0
       for j = 1:x
         z += j^2
       end
       return z
     end

     function f1(x::Float64)
       return x+2
     end

     function f1{T}(x::T)
       return x+5
     end

     f2(x) = 2*x
     end     # @profile begin

   Now load the file and execute the code you want to profile, e.g.::

     f1(215)
     for i = 1:100
       f1(3.5)
     end
     for i = 1:150
       f1(uint8(7))
     end
     for i = 1:125
       f2(11)
     end

   To view the execution times, type ``@profile report``.

   Here are the various options you have for controlling profiling:

   * ``@profile report``: display cumulative profiling results
   * ``@profile clear``: clear all timings accumulated thus far (start from zero)
   *  ``@profile off``: turn profiling off (there is no need to remove ``@profile begin ... end`` blocks)
   *  ``@profile on``: turn profiling back on


----
Tips
----

You should always discard the results of your first run: it may include the overhead needed to JIT-compile some of the subfunctions.

The primary source of variability is the garbage collector---if it runs between two "instrumentation" lines, its execution time gets added to the time that your own line of code contributes. This can make a fast-running line seem puzzlingly slow. One good way to reduce the variance is to run ``gc()`` before profiling. However, if your code tends to accumulate a bunch of temporaries that need to be cleaned up in the middle of the run, then calling ``gc()`` at the beginning can cause the collector to run at the same point in the code each time, a misleading but consistent result. A different approach is to use multiple runs (without an explicit ``gc()``) and hope that the collector runs at different points in each run. The cost of a given line is probably best reflected in those runs with shortest time for that line.

-----------
Limitations
-----------

Profiling adds a performance overhead which can be significant. You can prevent a subsection of your code from being profiled by encapsulating it inside a ``begin ... end`` block; in this case, the block as a whole is profiled, but the individual lines inside the block are not separately timed.
    
The profiler tries to compensate for its overhead in the reported times. This naturally leads to some degree of uncertainty about the execution time of individual lines. More significantly, currently the profiler does not compensate for its own instrumentation in profiled *subfunctions*. Consequently, it's recommended that you avoid profiling nested code as a big chunk---you probably want to pick out individual functions or groups of functions to profile separately.
