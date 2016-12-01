
.. currentmodule:: Base

Constants
=========

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
