
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

.. function:: isfileorlink(path) -> Bool

   Returns ``true`` if ``path`` is a regular file or a symbolic link, ``false`` otherwise.

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

.. function:: homedir() -> String

   Return the current user's home directory.

.. function:: dirname(path::String) -> String

   Get the directory part of a path.

.. function:: basename(path::String) -> String

   Get the file name part of a path.

.. function:: isabspath(path::String) -> Bool

   Determines whether a path is absolute (begins at the root directory).

.. function:: isdirpath(path::String) -> Bool

   Determines whether a path refers to a directory (for example, ends with a path separator).

.. function:: joinpath(parts...) -> String

   Join path components into a full path. If some argument is an absolute
   path, then prior components are dropped.

.. function:: abspath(path::String) -> String

   Convert a path to an absolute path by adding the current directory if
   necessary.

.. function:: normpath(path::String) -> String

   Normalize a path, removing "." and ".." entries.

.. function:: realpath(path::String) -> String

   Canonicalize a path by expanding symbolic links and removing "." and ".." entries.

.. function:: expanduser(path::String) -> String

   On Unix systems, replace a tilde character at the start of a path with the
   current user's home directory.

.. function:: splitdir(path::String) -> (String,String)

   Split a path into a tuple of the directory name and file name.

.. function:: splitdrive(path::String) -> (String,String)

   On Windows, split a path into the drive letter part and the path part. On Unix
   systems, the first component is always the empty string.

.. function:: splitext(path::String) -> (String,String)

   If the last component of a path contains a dot, split the path into everything
   before the dot and everything including and after the dot. Otherwise, return
   a tuple of the argument unmodified and the empty string.

.. function:: tempname()

   Generate a unique temporary filename.

.. function:: tempdir()

   Obtain the path of a temporary directory.

.. function:: mktemp()

   Returns ``(path, io)``, where ``path`` is the path of a new temporary file
   and ``io`` is an open file object for this path.

.. function:: mktempdir()

   Create a temporary directory and return its path.
