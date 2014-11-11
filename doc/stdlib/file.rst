
.. currentmodule:: Base

Filesystem
----------

.. function:: isblockdev(path) -> Bool

   Returns ``true`` if ``path`` is a block device, ``false`` otherwise.

.. function:: ischardev(path) -> Bool

   Returns ``true`` if ``path`` is a character device, ``false`` otherwise.

.. function:: isdir(path) -> Bool

   Returns ``true`` if ``path`` is a directory, ``false`` otherwise.

.. function:: isexecutable(path) -> Bool

   Returns ``true`` if the current user has permission to execute ``path``,
   ``false`` otherwise.

.. function:: isfifo(path) -> Bool

   Returns ``true`` if ``path`` is a FIFO, ``false`` otherwise.

.. function:: isfile(path) -> Bool

   Returns ``true`` if ``path`` is a regular file, ``false`` otherwise.

.. function:: islink(path) -> Bool

   Returns ``true`` if ``path`` is a symbolic link, ``false`` otherwise.

.. function:: ispath(path) -> Bool

   Returns ``true`` if ``path`` is a valid filesystem path, ``false`` otherwise.

.. function:: isreadable(path) -> Bool

   Returns ``true`` if the current user has permission to read ``path``,
   ``false`` otherwise.

.. function:: issetgid(path) -> Bool

   Returns ``true`` if ``path`` has the setgid flag set, ``false`` otherwise.

.. function:: issetuid(path) -> Bool

   Returns ``true`` if ``path`` has the setuid flag set, ``false`` otherwise.

.. function:: issocket(path) -> Bool

   Returns ``true`` if ``path`` is a socket, ``false`` otherwise.

.. function:: issticky(path) -> Bool

   Returns ``true`` if ``path`` has the sticky bit set, ``false`` otherwise.

.. function:: iswritable(path) -> Bool

   Returns ``true`` if the current user has permission to write to ``path``,
   ``false`` otherwise.

.. function:: homedir() -> AbstractString

   Return the current user's home directory.

.. function:: dirname(path::AbstractString) -> AbstractString

   Get the directory part of a path.

.. function:: basename(path::AbstractString) -> AbstractString

   Get the file name part of a path.

.. function:: @__FILE__() -> AbstractString

   ``@__FILE__`` expands to a string with the absolute path and file name of the script being run.
   Returns ``nothing`` if run from a REPL or an empty string if evaluated by ``julia -e <expr>``.

.. function:: isabspath(path::AbstractString) -> Bool

   Determines whether a path is absolute (begins at the root directory).

.. function:: isdirpath(path::AbstractString) -> Bool

   Determines whether a path refers to a directory (for example, ends with a path separator).

.. function:: joinpath(parts...) -> AbstractString

   Join path components into a full path. If some argument is an absolute
   path, then prior components are dropped.

.. function:: abspath(path::AbstractString) -> AbstractString

   Convert a path to an absolute path by adding the current directory if
   necessary.

.. function:: normpath(path::AbstractString) -> AbstractString

   Normalize a path, removing "." and ".." entries.

.. function:: realpath(path::AbstractString) -> AbstractString

   Canonicalize a path by expanding symbolic links and removing "." and ".." entries.

.. function:: expanduser(path::AbstractString) -> AbstractString

   On Unix systems, replace a tilde character at the start of a path with the
   current user's home directory.

.. function:: splitdir(path::AbstractString) -> (AbstractString,AbstractString)

   Split a path into a tuple of the directory name and file name.

.. function:: splitdrive(path::AbstractString) -> (AbstractString,AbstractString)

   On Windows, split a path into the drive letter part and the path part. On Unix
   systems, the first component is always the empty string.

.. function:: splitext(path::AbstractString) -> (AbstractString,AbstractString)

   If the last component of a path contains a dot, split the path into everything
   before the dot and everything including and after the dot. Otherwise, return
   a tuple of the argument unmodified and the empty string.

.. function:: tempname()

   Generate a unique temporary file path.

.. function:: tempdir()

   Obtain the path of a temporary directory (possibly shared with other processes).

.. function:: mktemp()

   Returns ``(path, io)``, where ``path`` is the path of a new temporary file
   and ``io`` is an open file object for this path.

.. function:: mktempdir()

   Create a temporary directory and return its path.
