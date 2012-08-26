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

   To view the execution times, type ``@profile report``.  Each row of the output shows the number of times the line was executed, the cumulative time spent on that line, the estimated "compensated" cumulative time (compensating for the overhead of profiling, see below), and the line number and filename.

   Here are the various options you have for controlling profiling:

   * ``@profile report``: display cumulative profiling results
   * ``@profile clear``: clear all timings accumulated thus far (start from zero)
   *  ``@profile off``: turn profiling off (there is no need to remove ``@profile begin ... end`` blocks)
   *  ``@profile on``: turn profiling back on

   Be aware that profiling adds a significant performance overhead. You can prevent a subsection of your code from being profiled by encapsulating it inside a ``begin ... end`` block; in this case, the block as a whole is profiled, but the individual lines inside the block are not separately timed.
