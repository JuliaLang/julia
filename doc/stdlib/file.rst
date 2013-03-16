
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

.. function:: iswriteable(path) -> Bool

   Returns ``true`` if the current user has permission to write to ``path``,
   ``false`` otherwise.

