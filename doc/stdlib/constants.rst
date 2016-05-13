.. currentmodule:: Base

Constants
=========

.. variable:: nothing

   .. Docstring generated from Julia source

   The singleton instance of type ``Void``\ , used by convention when there is no value to return (as in a C ``void`` function). Can be converted to an empty ``Nullable`` value.

.. variable:: OS_NAME

   .. Docstring generated from Julia source

   A symbol representing the name of the operating system. Possible values are ``:Linux``\ , ``:Darwin`` (OS X), or ``:Windows``\ .

.. variable:: PROGRAM_FILE

   .. Docstring generated from Julia source

   A string containing the script name passed to Julia from the command line. Note that the script name remains unchanged from within included files. Alternatively see :data:`@__FILE__`\ .

.. variable:: ARGS

   .. Docstring generated from Julia source

   An array of the command line arguments passed to Julia, as strings.

.. variable:: C_NULL

   .. Docstring generated from Julia source

   The C null pointer constant, sometimes used when calling external code.

.. variable:: CPU_CORES

   .. Docstring generated from Julia source

   The number of CPU cores in the system.

.. variable:: WORD_SIZE

   .. Docstring generated from Julia source

   Standard word size on the current machine, in bits.

.. variable:: VERSION

   .. Docstring generated from Julia source

   A ``VersionNumber`` object describing which version of Julia is in use. For details see :ref:`man-version-number-literals`\ .

.. variable:: LOAD_PATH

   .. Docstring generated from Julia source

   An array of paths (as strings) where the ``require`` function looks for code.

.. variable:: JULIA_HOME

   .. Docstring generated from Julia source

   A string containing the full path to the directory containing the ``julia`` executable.

.. variable:: ANY

   .. Docstring generated from Julia source

   Equivalent to ``Any`` for dispatch purposes, but signals the compiler to skip code generation specialization for that field.

See also:

:data:`STDIN`
:data:`STDOUT`
:data:`STDERR`
:data:`ENV`
:data:`ENDIAN_BOM`
:data:`Libc.MS_ASYNC`
:data:`Libc.MS_INVALIDATE`
:data:`Libc.MS_SYNC`
:data:`Libdl.DL_LOAD_PATH`
:data:`Libdl.RTLD_DEEPBIND`
:data:`Libdl.RTLD_LOCAL`
:data:`Libdl.RTLD_NOLOAD`
:data:`Libdl.RTLD_LAZY`
:data:`Libdl.RTLD_NOW`
:data:`Libdl.RTLD_GLOBAL`
:data:`Libdl.RTLD_NODELETE`
:data:`Libdl.RTLD_FIRST`
