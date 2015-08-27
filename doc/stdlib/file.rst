.. currentmodule:: Base

************
 Filesystem
************

.. function:: pwd() -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       pwd() -> AbstractString

   Get the current working directory.

.. function:: cd(dir::AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cd(dir::AbstractString)

   Set the current working directory.

   .. code-block:: julia

       cd(f, [dir])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning.

.. function:: cd(f, [dir])

   .. Docstring generated from Julia source

   .. code-block:: julia

       cd(dir::AbstractString)

   Set the current working directory.

   .. code-block:: julia

       cd(f, [dir])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning.

.. function:: readdir([dir]) -> Vector{ByteString}

   .. Docstring generated from Julia source

   .. code-block:: julia

       readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory ``dir`` (or the current working directory if not given).

.. function:: mkdir(path, [mode])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mkdir(path, [mode])

   Make a new directory with name ``path`` and permissions ``mode``\ . ``mode`` defaults to 0o777, modified by the current file creation mask.

.. function:: mkpath(path, [mode])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mkpath(path, [mode])

   Create all directories in the given ``path``\ , with permissions ``mode``\ . ``mode`` defaults to 0o777, modified by the current file creation mask.

.. function:: symlink(target, link)

   .. Docstring generated from Julia source

   .. code-block:: julia

       symlink(target, link)

   Creates a symbolic link to ``target`` with the name ``link``\ .

   **note**

   This function raises an error under operating systems that do not support soft symbolic links, such as Windows XP.

.. function:: readlink(path) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       readlink(path) -> AbstractString

   Returns the value of a symbolic link ``path``\ .

.. function:: chmod(path, mode)

   .. Docstring generated from Julia source

   .. code-block:: julia

       chmod(path, mode)

   Change the permissions mode of ``path`` to ``mode``\ . Only integer ``mode``\ s (e.g. 0o777) are currently supported.

.. function:: stat(file)

   .. Docstring generated from Julia source

   ::

              stat(file)

   Returns a structure whose fields contain information about the file. The fields of the structure are:

   ========= ======================================================================
    size      The size (in bytes) of the file
    device    ID of the device that contains the file
    inode     The inode number of the file
    mode      The protection mode of the file
    nlink     The number of hard links to the file
    uid       The user id of the owner of the file
    gid       The group id of the file owner
    rdev      If this file refers to a device, the ID of the device it refers to
    blksize   The file-system preferred block size for the file
    blocks    The number of such blocks allocated
    mtime     Unix timestamp of when the file was last modified
    ctime     Unix timestamp of when the file was created
   ========= ======================================================================

.. function:: lstat(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lstat(file)

   Like stat, but for symbolic links gets the info for the link itself rather than the file it refers to. This function must be called on a file path rather than a file object or a file descriptor.

.. function:: ctime(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ctime(file)

   Equivalent to stat(file).ctime

.. function:: mtime(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mtime(file)

   Equivalent to stat(file).mtime

.. function:: filemode(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       filemode(file)

   Equivalent to stat(file).mode

.. function:: filesize(path...)

   .. Docstring generated from Julia source

   .. code-block:: julia

       filesize(path...)

   Equivalent to stat(file).size

.. function:: uperm(file)

   .. Docstring generated from Julia source

   ::

              uperm(file)

   Gets the permissions of the owner of the file as a bitfield of

   ==== =====================
    01   Execute Permission
    02   Write Permission
    04   Read Permission
   ==== =====================

   For allowed arguments, see ``stat``.

.. function:: gperm(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       gperm(file)

   Like uperm but gets the permissions of the group owning the file

.. function:: operm(file)

   .. Docstring generated from Julia source

   .. code-block:: julia

       operm(file)

   Like uperm but gets the permissions for people who neither own the file nor are a member of the group owning the file

.. function:: cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false, follow_symlinks::Bool=false)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false, follow_symlinks::Bool=false)

   Copy the file, link, or directory from *src* to *dest*. ``remove_destination=true`` will first remove an existing ``dst``\ .

   If ``follow_symlinks=false``\ , and ``src`` is a symbolic link, ``dst`` will be created as a symbolic link. If ``follow_symlinks=true`` and ``src`` is a symbolic link, ``dst`` will be a copy of the file or directory ``src`` refers to.

.. function:: download(url,[localfile])

   .. Docstring generated from Julia source

   .. code-block:: julia

       download(url,[localfile])

   Download a file from the given url, optionally renaming it to the given local file name. Note that this function relies on the availability of external tools such as ``curl``\ , ``wget`` or ``fetch`` to download the file and is provided for convenience. For production use or situations in which more options are needed, please use a package that provides the desired functionality instead.

.. function:: mv(src::AbstractString,dst::AbstractString; remove_destination::Bool=false)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mv(src::AbstractString,dst::AbstractString; remove_destination::Bool=false)

   Move the file, link, or directory from *src* to *dest*. ``remove_destination=true`` will first remove an existing ``dst``\ .

.. function:: rm(path::AbstractString; recursive=false)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rm(path::AbstractString; recursive=false)

   Delete the file, link, or empty directory at the given path. If ``recursive=true`` is passed and the path is a directory, then all contents are removed recursively.

.. function:: touch(path::AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       touch(path::AbstractString)

   Update the last-modified timestamp on a file to the current time.

.. function:: tempname()

   .. Docstring generated from Julia source

   .. code-block:: julia

       tempname()

   Generate a unique temporary file path.

.. function:: tempdir()

   .. Docstring generated from Julia source

   .. code-block:: julia

       tempdir()

   Obtain the path of a temporary directory (possibly shared with other processes).

.. function:: mktemp([parent=tempdir()])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mktemp([parent=tempdir()])

   Returns ``(path, io)``\ , where ``path`` is the path of a new temporary file in ``parent`` and ``io`` is an open file object for this path.

   .. code-block:: julia

       mktemp(f::function, [parent=tempdir()])

   Apply the function ``f`` to the result of ``mktemp(parent)`` and remove the temporary file upon completion.

.. function:: mktemp(f::function, [parent=tempdir()])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mktemp([parent=tempdir()])

   Returns ``(path, io)``\ , where ``path`` is the path of a new temporary file in ``parent`` and ``io`` is an open file object for this path.

   .. code-block:: julia

       mktemp(f::function, [parent=tempdir()])

   Apply the function ``f`` to the result of ``mktemp(parent)`` and remove the temporary file upon completion.

.. function:: mktempdir([parent=tempdir()])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mktempdir([parent=tempdir()])

   Create a temporary directory in the ``parent`` directory and return its path.

   .. code-block:: julia

       mktempdir(f::function, [parent=tempdir()])

   Apply the function ``f`` to the result of ``mktempdir(parent)`` and remove the temporary directory upon completion.

.. function:: mktempdir(f::function, [parent=tempdir()])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mktempdir([parent=tempdir()])

   Create a temporary directory in the ``parent`` directory and return its path.

   .. code-block:: julia

       mktempdir(f::function, [parent=tempdir()])

   Apply the function ``f`` to the result of ``mktempdir(parent)`` and remove the temporary directory upon completion.

.. function:: isblockdev(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isblockdev(path) -> Bool

   Returns ``true`` if ``path`` is a block device, ``false`` otherwise.

.. function:: ischardev(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       ischardev(path) -> Bool

   Returns ``true`` if ``path`` is a character device, ``false`` otherwise.

.. function:: isdir(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isdir(path) -> Bool

   Returns ``true`` if ``path`` is a directory, ``false`` otherwise.

.. function:: isexecutable(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isexecutable(path) -> Bool

   Returns ``true`` if the current user has permission to execute ``path``\ , ``false`` otherwise.

.. function:: isfifo(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isfifo(path) -> Bool

   Returns ``true`` if ``path`` is a FIFO, ``false`` otherwise.

.. function:: isfile(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isfile(path) -> Bool

   Returns ``true`` if ``path`` is a regular file, ``false`` otherwise.

.. function:: islink(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       islink(path) -> Bool

   Returns ``true`` if ``path`` is a symbolic link, ``false`` otherwise.

.. function:: ismount(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       ismount(path) -> Bool

   Returns ``true`` if ``path`` is a mount point, ``false`` otherwise.

.. function:: ispath(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       ispath(path) -> Bool

   Returns ``true`` if ``path`` is a valid filesystem path, ``false`` otherwise.

.. function:: isreadable(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isreadable(path) -> Bool

   Returns ``true`` if the current user has permission to read ``path``\ , ``false`` otherwise.

.. function:: issetgid(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       issetgid(path) -> Bool

   Returns ``true`` if ``path`` has the setgid flag set, ``false`` otherwise.

.. function:: issetuid(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       issetuid(path) -> Bool

   Returns ``true`` if ``path`` has the setuid flag set, ``false`` otherwise.

.. function:: issocket(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       issocket(path) -> Bool

   Returns ``true`` if ``path`` is a socket, ``false`` otherwise.

.. function:: issticky(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       issticky(path) -> Bool

   Returns ``true`` if ``path`` has the sticky bit set, ``false`` otherwise.

.. function:: iswritable(path) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       iswritable(path) -> Bool

   Returns ``true`` if the current user has permission to write to ``path``\ , ``false`` otherwise.

.. function:: homedir() -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       homedir() -> AbstractString

   Return the current user's home directory.

.. function:: dirname(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       dirname(path::AbstractString) -> AbstractString

   Get the directory part of a path.

.. function:: basename(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       basename(path::AbstractString) -> AbstractString

   Get the file name part of a path.

.. function:: @__FILE__() -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       @__FILE__() -> AbstractString

   ``@__FILE__`` expands to a string with the absolute path and file name of the script being run. Returns ``nothing`` if run from a REPL or an empty string if evaluated by ``julia -e <expr>``\ .

.. function:: isabspath(path::AbstractString) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isabspath(path::AbstractString) -> Bool

   Determines whether a path is absolute (begins at the root directory).

.. function:: isdirpath(path::AbstractString) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isdirpath(path::AbstractString) -> Bool

   Determines whether a path refers to a directory (for example, ends with a path separator).

.. function:: joinpath(parts...) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       joinpath(parts...) -> AbstractString

   Join path components into a full path. If some argument is an absolute path, then prior components are dropped.

.. function:: abspath(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       abspath(path::AbstractString) -> AbstractString

   Convert a path to an absolute path by adding the current directory if necessary.

.. function:: normpath(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       normpath(path::AbstractString) -> AbstractString

   Normalize a path, removing "." and ".." entries.

.. function:: realpath(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       realpath(path::AbstractString) -> AbstractString

   Canonicalize a path by expanding symbolic links and removing "." and ".." entries.

.. function:: relpath(path::AbstractString, startpath::AbstractString = ".") -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       relpath(path::AbstractString, startpath::AbstractString = ".") -> AbstractString

   Return a relative filepath to path either from the current directory or from an optional start directory. This is a path computation: the filesystem is not accessed to confirm the existence or nature of path or startpath.

.. function:: expanduser(path::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   .. code-block:: julia

       expanduser(path::AbstractString) -> AbstractString

   On Unix systems, replace a tilde character at the start of a path with the current user's home directory.

.. function:: splitdir(path::AbstractString) -> (AbstractString,AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       splitdir(path::AbstractString) -> (AbstractString,AbstractString)

   Split a path into a tuple of the directory name and file name.

.. function:: splitdrive(path::AbstractString) -> (AbstractString,AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       splitdrive(path::AbstractString) -> (AbstractString,AbstractString)

   On Windows, split a path into the drive letter part and the path part. On Unix systems, the first component is always the empty string.

.. function:: splitext(path::AbstractString) -> (AbstractString,AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       splitext(path::AbstractString) -> (AbstractString,AbstractString)

   If the last component of a path contains a dot, split the path into everything before the dot and everything including and after the dot. Otherwise, return a tuple of the argument unmodified and the empty string.

