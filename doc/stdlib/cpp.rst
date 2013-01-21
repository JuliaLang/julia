cpp.jl --- Calling C++ from Julia
=================================

.. .. module:: cpp.jl
   :synopsis: Provides partial support for calling C++ library functions from Julia.

Provides partial support for calling C++ library functions from Julia.

.. function:: @cpp(ccall_expression)

   Suppose you have a C++ shared library, ``libdemo``, which contains a function ``timestwo``::

     int timestwo(int x) {
       return 2*x;
     }

     double timestwo(double x) {
       return 2*x;
     }

   You can use these functions by placing the ``@cpp`` macro prior to a ccall, for example::

     mylib = dlopen("libdemo")
     x = 3.5
     x2 = @cpp ccall(dlsym(mylib, :timestwo), Float64, (Float64,), x)
     y = 3
     y2 = @cpp ccall(dlsym(mylib, :timestwo), Int, (Int,), y)
     
   The macro performs C++ ABI name-mangling (using the types of the parameters) to determine the correct library symbol.

   Like ``ccall``, this performs library calls without overhead. However, currently it has a number of limitations:

   * It does not support pure-header libraries
   * The restrictions of ``ccall`` apply here; for example, there is no support for ``struct``. Consequently it is not possible to use C++ objects.
   * Currently there is no C++ namespace support
   * Currently there is no support for templated functions
   * Currently only g++ is supported

   The latter three may not be difficult to `fix <http://www.agner.org/optimize/calling_conventions.pdf>`_.
