*******************************************
Reporting and analyzing crashes (segfaults)
*******************************************

So you managed to break Julia.  Congratulations!  Collected here are some general procedures you can undergo for common symptoms encountered when something goes awry.  Including the information from these debugging steps can greatly help the maintainers when tracking down a segfault or trying to figure out why your script is running slower than expected.

If you've been directed to this page, find the symptom that best matches what you're experiencing and follow the instructions to generate the debugging information requested.  Table of symptoms:

* `Segfaults during bootstrap (sysimg.jl)`_

* `Segfaults when running a script`_

* `Errors during julia startup`_

.. _version info:

Version/Environment info
------------------------

No matter the error, we will always need to know what version of julia you are running. When julia first starts up, a header is printed out with a version number and date.  If your version is ``0.2.0`` or higher, please include the output of ``versioninfo()`` in any report you create::

 julia> versioninfo()
 Julia Version 0.3.3-pre+25
 Commit 417b50a* (2014-11-03 11:32 UTC)
 Platform Info:
   System: Linux (x86_64-linux-gnu)
   CPU: Intel(R) Core(TM) i7 CPU       L 640  @ 2.13GHz
   WORD_SIZE: 64
   BLAS: libopenblas (USE64BITINT DYNAMIC_ARCH NO_AFFINITY Nehalem)
   LAPACK: libopenblas
   LIBM: libopenlibm
   LLVM: libLLVM-3.3


.. _Segfaults during bootstrap (sysimg.jl):

Segfaults during bootstrap (sysimg.jl)
--------------------------------------

Segfaults toward the end of the ``make`` process of building julia are a common symptom of something going wrong while julia is preparsing the corpus of code in the ``base/`` folder.  Many factors can contribute toward this process dying unexpectedly, however it is as often as not due to an error in the C-code portion of julia, and as such must typically be debugged with a debug build inside of ``gdb``.  Explicitly:

Create a debug build of julia::

  $ cd <julia_root>
  $ make debug

Note that this process will likely fail with the same error as a normal ``make`` incantation, however this will create a debug executable that will offer ``gdb`` the debugging symbols needed to get accurate backtraces.  Next, manually run the bootstrap process inside of ``gdb``::

  $ cd base/
  $ gdb -x ../contrib/debug_bootstrap.gdb

This will start ``gdb``, attempt to run the bootstrap process using the debug build of julia, and print out a backtrace if (when) it segfaults.  You may need to hit ``<enter>`` a few times to get the full backtrace.  Create a gist_ with the backtrace, the `version info`_, and any other pertinent information you can think of and open a new issue_ on Github with a link to the gist.


.. _Segfaults when running a script:

Segfaults when running a script
-------------------------------

The procedure is very similar to `Segfaults during bootstrap (sysimg.jl)`_.  Create a debug build of Julia, and run your script inside of a debugged julia process::

  $ cd <julia_root>
  $ make debug
  $ gdb --args usr/bin/julia-debug-readline <path_to_your_script>

Note that ``gdb`` will sit there, waiting for instructions.  Type ``r`` to run the process, and ``bt`` to generate a backtrace once it segfaults::

  (gdb) r
  Starting program: /home/sabae/src/julia/usr/bin/julia-debug-readline ./test.jl
  ...
  (gdb) bt

Create a gist_ with the backtrace, the `version info`_, and any other pertinent information you can think of and open a new issue_ on Github with a link to the gist.


.. _Errors during julia startup:

Errors during julia startup
---------------------------

Occasionally errors occur during julia's startup process (especially when using binary distributions, as opposed to compiling from source) such as the following::

  $ julia
  exec: error -5

These errors typically indicate something is not getting loaded properly very early on in the bootup phase, and our best bet in determining what's going wrong is to use external tools to audit the disk activity of the ``julia`` process:

* On Linux, use ``strace``::

    $ strace julia

* On OSX, use ``dtruss``::

    $ dtruss -f julia

Create a gist_ with the ``strace``/ ``dtruss`` ouput, the `version info`_, and any other pertinent information and open a new issue_ on Github with a link to the gist.


Glossary
--------

A few terms have been used as shorthand in this guide:

* ``<julia_root>`` refers to the root directory of the julia source tree; e.g. it should contain folders such as ``base``, ``deps``, ``src``, ``test``, etc.....

.. _gist: http://gist.github.com
.. _issue: https://github.com/JuliaLang/julia/issues?state=open
