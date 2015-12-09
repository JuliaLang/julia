# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Docs: keywords

include("helpdb/BLAS.jl")
include("helpdb/Libdl.jl")
include("helpdb/Libc.jl")
include("helpdb/Collections.jl")
include("helpdb/Profile.jl")
include("helpdb/Cartesian.jl")
include("helpdb/Base.jl")
include("helpdb/Dates.jl")
include("helpdb/Pkg.jl")

doc"""
    randjump(r::MersenneTwister, jumps, [jumppoly]) -> Vector{MersenneTwister}

Create an array of the size `jumps` of initialized `MersenneTwister` RNG objects where the first RNG object given as a parameter and following `MersenneTwister` RNGs in the array initialized such that a state of the RNG object in the array would be moved forward (without generating numbers) from a previous RNG object array element on a particular number of steps encoded by the jump polynomial `jumppoly`.

Default jump polynomial moves forward `MersenneTwister` RNG state by 10^20 steps.
"""
randjump

doc"""
```rst
..  \:(start, [step], stop)

Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1, and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the function ``colon``.
The colon is also used in indexing to select whole dimensions.
```
"""
colon(start, step, stop)

doc"""
```rst
..  $(x, y)

Bitwise exclusive or
```
"""
Base.(:$)(x, y)

doc"""
    getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr,UInt16)

Get the IP address and the port that the given TCP socket is connected to (or bound to, in the case of TCPServer).
"""
getsockname

doc"""
    Base.remoteref_id(r::AbstractRemoteRef) -> (whence, id)

A low-level API which returns the unique identifying tuple for a remote reference. A reference id is a tuple of two
elements - pid where the reference was created from and a one-up number from that node.
"""
Base.remoteref_id

doc"""
    Base.channel_from_id(refid) -> c

A low-level API which returns the backing AbstractChannel for an id returned by `remoteref_id`. The call is valid only on the node where the backing channel exists.
"""
Base.channel_from_id

doc"""
    Base.worker_id_from_socket(s::IO) -> pid

A low-level API which given a `IO` connection, returns the pid of the worker it is connected to. This is useful when writing custom `serialize` methods for a type, which
optimizes the data written out depending on the receiving process id.
"""
Base.worker_id_from_socket



