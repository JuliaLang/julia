.. currentmodule:: Base


Modules
-------

.. module:: Collections

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Meta

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Operators

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Order

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Pkg

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Pkg2

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Profile

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Sys

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Test

   UNDOCUMENTED (may not appear in helpdb.jl)


Types
-----


Ccall types
-----------


Exceptions
----------


Global constants and variables
------------------------------


Mathematical constants
----------------------


Operators
---------



scalar math
-----------

.. function:: invdigamma(...)

   UNDOCUMENTED

.. function:: trigamma(...)

   UNDOCUMENTED


specfun
-------

.. function:: besselh(...)

   UNDOCUMENTED

.. function:: polygamma(...)

   UNDOCUMENTED


arrays
------

.. function:: bsxfun(...)

   UNDOCUMENTED

.. function:: cartesianmap(...)

   UNDOCUMENTED

.. function:: checkbounds(...)

   UNDOCUMENTED

.. function:: findnext(...)

   UNDOCUMENTED

.. function:: findnz(...)

   UNDOCUMENTED

.. function:: gradient(...)

   UNDOCUMENTED

.. function:: index_shape(...)

   UNDOCUMENTED

.. function:: promote_shape(...)

   UNDOCUMENTED

.. function:: setindex_shape_check(X::AbstractArray{T,N},I...)

   UNDOCUMENTED

.. function:: slice(...)

   UNDOCUMENTED


linear algebra
--------------

.. function:: solve(...)

   UNDOCUMENTED

.. function:: symmetrize!(...)

   UNDOCUMENTED


sparse
------


bitarrays
---------


dequeues
--------


collections
-----------

strings and text output
-----------------------

.. function:: first_utf8_byte(c::Char)

   UNDOCUMENTED

.. function:: is_utf8_start(byte::Uint8)

   UNDOCUMENTED

.. function:: lcfirst(...)

   UNDOCUMENTED

.. function:: print_joined(...)

   UNDOCUMENTED

.. function:: print_shortest(...)

   UNDOCUMENTED

.. function:: print_with_color(...)

   UNDOCUMENTED

.. function:: repl_show(...)

   UNDOCUMENTED

.. function:: ucfirst(...)

   UNDOCUMENTED

.. function:: xdump(...)

   UNDOCUMENTED


random numbers
--------------

bigfloat & precision
--------------------

statistics
----------

.. function:: hist2d(...)

   UNDOCUMENTED

.. function:: median!(v::AbstractArray{T<:Real,1})

   UNDOCUMENTED

.. function:: quantile!(v::AbstractArray{T,1},q::AbstractArray{T,1})

   UNDOCUMENTED


signal processing
-----------------


numerical integration
---------------------


iteration
---------


object identity and equality
----------------------------


tasks and conditions
--------------------


time
----


errors
------


types
-----


syntax
------


help and reflection
-------------------

.. function:: functionlocs(...)

   UNDOCUMENTED

.. function:: less(...)

   UNDOCUMENTED


loading source files
--------------------


RTS internals
-------------

misc
----

.. function:: tty_cols()

   UNDOCUMENTED

.. function:: tty_rows()

   UNDOCUMENTED


IP address stuff
----------------


I/O and events
--------------

.. function:: mmap_grow(len::Integer,prot::Integer,flags::Integer,fd::Integer,offset::Int64)

   UNDOCUMENTED

.. function:: mmap_stream_settings(s::IO)

   UNDOCUMENTED


multiprocessing
---------------

.. function:: isready(rr::RemoteRef)

   UNDOCUMENTED


distributed arrays
------------------


paths and file names
--------------------

.. function:: expanduser(path::String)

   UNDOCUMENTED

.. function:: isdirpath(path::String)

   UNDOCUMENTED

.. function:: normpath(...)

   UNDOCUMENTED

.. function:: realpath(path::String)

   UNDOCUMENTED

.. function:: splitdir(...)

   UNDOCUMENTED

.. function:: splitdrive(path::String)

   UNDOCUMENTED

.. function:: splitext(path::String)

   UNDOCUMENTED


filesystem operations
---------------------


external processes ## TODO: whittle down these exports.
-------------------------------------------------------

.. function:: process_signaled(s::Process)

   UNDOCUMENTED


C interface
-----------

.. function:: c_malloc(size::Integer)

   UNDOCUMENTED

.. function:: dlopen_e(...)

   UNDOCUMENTED

.. function:: pointer_from_objref(x)

   UNDOCUMENTED

.. function:: unsafe_copy!(...)

   UNDOCUMENTED

.. function:: unsafe_pointer_to_objref(p::Ptr{T})

   UNDOCUMENTED


Macros
------

