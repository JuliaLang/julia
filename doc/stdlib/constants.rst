
.. currentmodule:: Base

Constants
---------

.. data:: OS_NAME

   A symbol representing the name of the operating system. Possible values
   are ``:Linux``, ``:Darwin`` (OS X), or ``:Windows``.

.. data:: ARGS

   An array of the command line arguments passed to Julia, as strings.

.. data:: C_NULL

   The C null pointer constant, sometimes used when calling external code.

.. data:: CPU_CORES

   The number of CPU cores in the system.

.. data:: WORD_SIZE

   Standard word size on the current machine, in bits.

.. data:: VERSION

   An object describing which version of Julia is in use.

.. data:: LOAD_PATH

   An array of paths (as strings) where the ``require`` function looks for code.

.. data:: JULIA_HOME

   A string containing the full path to the directory containing the ``julia`` executable.

.. data:: ANY

   Equivalent to ``Any`` for dispatch purposes, but signals the compiler to skip code generation specialization for that field

See also:

:data:`STDIN`
:data:`STDOUT`
:data:`STDERR`
:data:`ENV`
:data:`ENDIAN_BOM`
:data:`MS_ASYNC`
:data:`MS_INVALIDATE`
:data:`MS_SYNC`
:data:`DL_LOAD_PATH`
:data:`RTLD_DEEPBIND`
:data:`RTLD_LOCAL`
:data:`RTLD_NOLOAD`
:data:`RTLD_LAZY`
:data:`RTLD_NOW`
:data:`RTLD_GLOBAL`
:data:`RTLD_NODELETE`
:data:`RTLD_FIRST`
