:mod:`profile.jl` --- A simple profiler for Julia
====================================================

.. module:: profile.jl
   :synopsis: Allows you to determine running times for each line of code.

.. function:: profile

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


-----------
Limitations
-----------

Profiling adds a significant performance overhead. You can prevent a subsection of your code from being profiled by encapsulating it inside a ``begin ... end`` block; in this case, the block as a whole is profiled, but the individual lines inside the block are not separately timed.
    
The profiler tries to compensate for its overhead in the reported times. This naturally leads to some degree of uncertainty about the execution time of individual lines. More significantly, currently the profiler does not compensate for its own instrumentation in profiled subfunctions. Consequently, it's not terribly useful in heavily-nested code to profile it as a big chunk---you probably want to pick out individual functions or groups of functions to profile separately.
