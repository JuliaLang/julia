
.. currentmodule:: Base

************
 Filesystem
************

.. function:: pwd() -> AbstractString

   Get the current working directory.


.. function:: cd(f[, dir])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning.


.. function:: cd(f[, dir])

   Temporarily changes the current working directory (HOME if not specified) and applies function f before returning.


.. function:: readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory *dir* (or the current working directory if not given).


.. function:: mkdir(path[, mode])

   Make a new directory with name ``path`` and permissions ``mode``. mask.


.. function:: mkpath(path[, mode])

   Create all directories in the given ``path``, with permissions creation mask.


.. function:: symlink(target, link)

   Creates a symbolic link to ``target`` with the name ``link``. Note: This function raises an error under operating systems that


.. function:: readlink(path) -> AbstractString

   Returns the value of a symbolic link ``path``.


.. function:: chmod(path, mode)

   Change the permissions mode of ``path`` to ``mode``. Only integer


.. function:: stat(file)

   Returns a structure whose fields contain information about the file. The fields of the structure are:


.. function:: lstat(file)

   Like stat, but for symbolic links gets the info for the link itself rather than the file it refers to. This function must be called on a file path rather than a file object or a file descriptor.


.. function:: ctime(file)

   Equivalent to stat(file).ctime


.. function:: mtime(file)

   Equivalent to stat(file).mtime


.. function:: filemode(file)

   Equivalent to stat(file).mode


.. function:: filesize(path...)

   Equivalent to stat(file).size


.. function:: uperm(file)

   Gets the permissions of the owner of the file as a bitfield of For allowed arguments, see ``stat``.


.. function:: gperm(file)

   Like uperm but gets the permissions of the group owning the file


.. function:: operm(file)

   Like uperm but gets the permissions for people who neither own the file nor are a member of the group owning the file


.. function:: cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false, follow_symlinks::Bool=false)

   Copy the file, link, or directory from *src* to *dest*. If *follow_symlinks=false*, and src is a symbolic link, dst will be created as a symbolic link. If *follow_symlinks=true* and src is a symbolic link, dst will be a copy of the file or directory *src* refers to.


.. function:: download(url[, localfile])

   Download a file from the given url, optionally renaming it to the given local file name. Note that this function relies on the availability of external tools such as ``curl``, ``wget`` or production use or situations in which more options are need, please use a package that provides the desired functionality instead.


.. function:: mv(src::AbstractString, dst::AbstractString; remove_destination::Bool=false)

   Move the file, link, or directory from *src* to *dest*. ``remove_destination=true`` will first remove an existing *dst*.


.. function:: rm(path::AbstractString; recursive=false)

   Delete the file, link, or empty directory at the given path. If contents are removed recursively.


.. function:: touch(path::AbstractString)

   Update the last-modified timestamp on a file to the current time.


.. function:: tempname()

   Generate a unique temporary file path.


.. function:: tempdir()

   Obtain the path of a temporary directory (possibly shared with other processes).


.. function:: mktemp([parent=tempdir()])

   Returns ``(path, io)``, where ``path`` is the path of a new temporary file in ``parent`` and ``io`` is an open file object for this path.


.. function:: mktempdir([parent=tempdir()])

   Create a temporary directory in the ``parent`` directory and return its path.


.. function:: isblockdev(path) -> Bool

   Returns ``true`` if ``path`` is a block device, ``false`` otherwise.


.. function:: ischardev(path) -> Bool

   Returns ``true`` if ``path`` is a character device, ``false`` otherwise.


.. function:: isdir(path) -> Bool

   Returns ``true`` if ``path`` is a directory, ``false`` otherwise.


.. function:: isexecutable(path) -> Bool

   Returns ``true`` if the current user has permission to execute


.. function:: isfifo(path) -> Bool

   Returns ``true`` if ``path`` is a FIFO, ``false`` otherwise.


.. function:: isfile(path) -> Bool

   Returns ``true`` if ``path`` is a regular file, ``false`` otherwise.


.. function:: islink(path) -> Bool

   Returns ``true`` if ``path`` is a symbolic link, ``false`` otherwise.


.. function:: ismount(path) -> Bool

   Returns ``true`` if ``path`` is a mount point, ``false`` otherwise.


.. function:: ispath(path) -> Bool

   Returns ``true`` if ``path`` is a valid filesystem path, ``false`` otherwise.


.. function:: isreadable(path) -> Bool

   Returns ``true`` if the current user has permission to read


.. function:: issetgid(path) -> Bool

   Returns ``true`` if ``path`` has the setgid flag set, ``false`` otherwise.


.. function:: issetuid(path) -> Bool

   Returns ``true`` if ``path`` has the setuid flag set, ``false`` otherwise.


.. function:: issocket(path) -> Bool

   Returns ``true`` if ``path`` is a socket, ``false`` otherwise.


.. function:: issticky(path) -> Bool

   Returns ``true`` if ``path`` has the sticky bit set, ``false`` otherwise.


.. function:: iswritable(path) -> Bool

   Returns ``true`` if the current user has permission to write to


.. function:: homedir() -> AbstractString

   Return the current user's home directory.


.. function:: dirname(path::AbstractString) -> AbstractString

   Get the directory part of a path.


.. function:: basename(path::AbstractString) -> AbstractString

   Get the file name part of a path.


.. function:: @__FILE__() -> AbstractString

   name of the script being run. Returns ``nothing`` if run from a REPL or an empty string if evaluated by ``julia -e <expr>``.


.. function:: isabspath(path::AbstractString) -> Bool

   Determines whether a path is absolute (begins at the root directory).


.. function:: isdirpath(path::AbstractString) -> Bool

   Determines whether a path refers to a directory (for example, ends with a path separator).


.. function:: joinpath(parts...) -> AbstractString

   Join path components into a full path. If some argument is an absolute path, then prior components are dropped.


.. function:: abspath(path::AbstractString) -> AbstractString

   Convert a path to an absolute path by adding the current directory if necessary.


.. function:: normpath(path::AbstractString) -> AbstractString

   Normalize a path, removing ``.`` and ``..`` entries.


.. function:: realpath(path::AbstractString) -> AbstractString

   Canonicalize a path by expanding symbolic links and removing ``.`` and ``..`` entries.


.. function:: relpath(path::AbstractString, startpath::AbstractString = ".") -> AbstractString

   Return a relative filepath to path either from the current directory or from an optional start directory. This is a path computation: the filesystem is not accessed to confirm the existence or nature of path or startpath.


.. function:: expanduser(path::AbstractString) -> AbstractString

   On Unix systems, replace a tilde character at the start of a path with the current user's home directory.


.. function:: splitdir(path::AbstractString) -> (AbstractString, AbstractString)

   Split a path into a tuple of the directory name and file name.


.. function:: splitdrive(path::AbstractString) -> (AbstractString, AbstractString)

   On Windows, split a path into the drive letter part and the path part. On Unix systems, the first component is always the empty string.


.. function:: splitext(path::AbstractString) -> (AbstractString, AbstractString)

   If the last component of a path contains a dot, split the path into everything before the dot and everything including and after the dot. Otherwise, return a tuple of the argument unmodified and the empty string.


