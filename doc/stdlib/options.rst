:mod:`options.jl` --- Optional arguments to functions
=====================================================

.. module:: options.jl
   :synopsis: Allows a flexible approach to providing default values for parameters in functions

.. function:: options

   Use the ``@options`` macro to set the value of optional parameters for a function that has been written to use them (see :func:`defaults` to learn how to write such functions).  The syntax is::

     opts = @options a=5 b=7

   For a function that uses optional parameters ``a`` and ``b``, this will override the default settings for these parameters. You would likely call that function in the following way::

     myfunc(requiredarg1, requiredarg2, ..., opts)

   Most functions written to use optional arguments will probably check to make sure that you are not supplying parameters that are not used by the function or its sub-functions. Typically, supplying unused parameters will result in an error. You can control the behavior this way::

     # throw an error if a or b is not used (the default)
     opts = @options CheckError a=5 b=2
     # issue a warning if a or b is not used
     opts = @options CheckWarn a=5 b=2a+1
     # don't check whether a and b are used
     opts = @options CheckNone a=5 b=2a+1

.. function:: set_options

    The ``@set_options`` macro lets you add new parameters to an existing options structure.  For example::

      @set_options opts d=99

    would add ``d`` to the set of parameters in ``opts``, or re-set its value if it was already supplied.

.. function:: defaults

    The ``@defaults`` macro is for writing functions that take optional parameters.  The typical syntax of such functions is::

      function myfunc(requiredarg1, requiredarg2, ..., opts::Options)
          @defaults opts a=11 b=2a+1 c=a*b d=100
          # The function body. Use a, b, c, and d just as you would
	  # any other variable. For example,
	  k = a + b
	  # You can pass opts down to subfunctions, which might supply
	  # additional defaults for other variables aa, bb, etc.
	  y = subfun(k, opts)
	  # Terminate your function with check_used, then return values
	  @check_used opts
	  return y
      end

    Note the function calls :func:`check_used` at the end.

    It is possible to have more than one Options parameter to a function, for example::

      function twinopts(x,plotopts::Options,calcopts::Options)
          @defaults plotopts linewidth=1
          @defaults calcopts n_iter=100
          # Do stuff
          @check_used plotopts
          @check_used calcopts
      end
 
.. function:: check_used

    The ``@check_used`` macro tests whether user-supplied parameters were ever accessed by the ``@defaults`` macro. The test is performed at the end of the function body, so that subfunction handling parameters not used by the parent function may be "credited" for their usage. Each sub-function should also call ``@check_used``, for example::

      function complexfun(x,opts::Options)
          @defaults opts parent=3 both=7
          println(parent)
          println(both)
          subfun1(x,opts)
          subfun2(x,opts)
          @check_used opts
      end
      
      function subfun1(x,opts::Options)
          @defaults opts sub1="sub1 default" both=0
          println(sub1)
          println(both)
          @check_used opts
      end
      
      function subfun2(x,opts::Options)
          @defaults opts sub2="sub2 default" both=22
          println(sub2)
          println(both)
          @check_used opts
      end

