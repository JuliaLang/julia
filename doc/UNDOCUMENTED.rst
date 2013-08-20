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

.. function:: get_precision(num::FloatingPoint)

   Get the precision of a floating point number as, as defined by the effective number of bits in the mantissa.

.. function:: get_bigfloat_precision()

   Get the precision currently used for BigFloat arithmetic.

.. function:: set_bigfloat_precision(x::Int64)

   Set the precision to be used to BigFloat arithmetic.

.. function:: with_bigfloat_precision(f::Function,precision::Integer)

   Change the bigfloat arithmetic precision for the duration of f. Is logically equivalent to
   old = get_bigfloat_precision()
   set_bigfloat_precision(precision)
   f()
   set_bigfloat_precision(old)

.. function:: get_bigfloat_rounding()

   Get the current BigFloat rounding mode. Valid modes are const RoundToNearest, RoundToZero, RoundUp, RoundDown, RoundAwayZero

.. function:: set_bigfloat_rounding(mode)

   Set the BigFloat rounding mode. See get_bigfloat_rounding for available modes

.. function:: with_bigfloat_rounding(f::Function,mode)

   Change the bigfloat arithmetic precision for the duration of f. See get_bigfloat_rounding for available rounding modes. Also
   see with_bigfloat_precision.

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

.. function:: countlines(...)

   UNDOCUMENTED

.. function:: eatwspace(s::IOStream)

   UNDOCUMENTED

.. function:: eatwspace_comment(s::IOStream,cmt::Char)

   UNDOCUMENTED

.. function:: mmap_grow(len::Integer,prot::Integer,flags::Integer,fd::Integer,offset::Int64)

   UNDOCUMENTED

.. function:: mmap_stream_settings(s::IO)

   UNDOCUMENTED

.. function:: PipeBuffer(...)

   UNDOCUMENTED

.. function:: readavailable(this::AsyncStream)

   UNDOCUMENTED

.. function:: readchomp(x)

   UNDOCUMENTED

.. function:: readdir(...)

   UNDOCUMENTED

.. function:: truncate(...)

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

.. function:: cp(src::String,dst::String)

   UNDOCUMENTED

.. function:: ctime(path...)

   UNDOCUMENTED

.. function:: download(...)

   UNDOCUMENTED

.. function:: filemode(path...)

   UNDOCUMENTED

.. function:: filesize(path...)

   UNDOCUMENTED

.. function:: gperm(...)

   UNDOCUMENTED

.. function:: ls(args...)

   UNDOCUMENTED

.. function:: lstat(...)

   UNDOCUMENTED

.. function:: mtime(path...)

   UNDOCUMENTED

.. function:: mv(src::String,dst::String)

   UNDOCUMENTED

.. function:: operm(...)

   UNDOCUMENTED

.. function:: rm(path::String)

   UNDOCUMENTED

.. function:: stat(...)

   UNDOCUMENTED

.. function:: touch(path::String)

   UNDOCUMENTED

.. function:: uperm(...)

   UNDOCUMENTED


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

