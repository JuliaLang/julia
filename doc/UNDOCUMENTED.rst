.. currentmodule:: Base


Modules
-------

.. module:: Collections

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: DSP

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Errno

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: FFTW

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: GMP

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: LibRandom

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: LinAlg

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Math

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Meta

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: MPFR

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Operators

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Order

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: PCRE

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Pkg

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Pkg2

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Profile

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: QuadGK

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Random

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: Sort

   UNDOCUMENTED (may not appear in helpdb.jl)

.. module:: SparseMatrix

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

.. function:: !==(x,y)

   UNDOCUMENTED

.. function:: A_ldiv_Bc(a,b)

   UNDOCUMENTED

.. function:: A_ldiv_Bt(a,b)

   UNDOCUMENTED

.. function:: A_mul_B(...)

   UNDOCUMENTED

.. function:: A_mul_Bc(...)

   UNDOCUMENTED

.. function:: A_mul_Bt(...)

   UNDOCUMENTED

.. function:: A_rdiv_Bc(...)

   UNDOCUMENTED

.. function:: A_rdiv_Bt(a,b)

   UNDOCUMENTED

.. function:: Ac_ldiv_B(...)

   UNDOCUMENTED

.. function:: Ac_ldiv_Bc(...)

   UNDOCUMENTED

.. function:: Ac_mul_B(...)

   UNDOCUMENTED

.. function:: Ac_mul_Bc(...)

   UNDOCUMENTED

.. function:: Ac_rdiv_B(a,b)

   UNDOCUMENTED

.. function:: Ac_rdiv_Bc(a,b)

   UNDOCUMENTED

.. function:: At_ldiv_B(...)

   UNDOCUMENTED

.. function:: At_ldiv_Bt(...)

   UNDOCUMENTED

.. function:: At_mul_B(...)

   UNDOCUMENTED

.. function:: At_mul_Bt(...)

   UNDOCUMENTED

.. function:: At_rdiv_B(a,b)

   UNDOCUMENTED

.. function:: At_rdiv_Bt(a,b)

   UNDOCUMENTED


scalar math
-----------

.. function:: big(...)

   UNDOCUMENTED

.. function:: divrem(...)

   UNDOCUMENTED

.. function:: invdigamma(...)

   UNDOCUMENTED

.. function:: rationalize(...)

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

.. function:: pascal(n)

   UNDOCUMENTED

.. function:: promote_shape(...)

   UNDOCUMENTED

.. function:: searchsortedfirst(...)

   UNDOCUMENTED

.. function:: searchsortedlast(...)

   UNDOCUMENTED

.. function:: setindex_shape_check(X::AbstractArray{T,N},I...)

   UNDOCUMENTED

.. function:: slice(...)

   UNDOCUMENTED


linear algebra
--------------

.. function:: bkfact!(...)

   UNDOCUMENTED

.. function:: bkfact(...)

   UNDOCUMENTED

.. function:: factorize!(...)

   UNDOCUMENTED

.. function:: factorize(A::AbstractArray{T,2})

   UNDOCUMENTED

.. function:: isposdef!(...)

   UNDOCUMENTED

.. function:: ldltd!(A::SymTridiagonal{T<:Union(Float64,Complex{Float32},Complex{Float64},Float32)})

   UNDOCUMENTED

.. function:: ldltd(A::SymTridiagonal{T<:Union(Float64,Complex{Float32},Complex{Float64},Float32)})

   UNDOCUMENTED

.. function:: logdet(...)

   UNDOCUMENTED

.. function:: randsym(n)

   UNDOCUMENTED

.. function:: rref(...)

   UNDOCUMENTED

.. function:: schurfact!(...)

   UNDOCUMENTED

.. function:: solve(...)

   UNDOCUMENTED

.. function:: symmetrize!(...)

   UNDOCUMENTED

.. function:: tril!(...)

   UNDOCUMENTED

.. function:: triu!(...)

   UNDOCUMENTED


sparse
------

.. function:: spdiagm(v::Union(AbstractArray{T,2},AbstractArray{T,1}))

   UNDOCUMENTED


bitarrays
---------

.. function:: bitpack(A::AbstractArray{T,N})

   UNDOCUMENTED

.. function:: bitunpack(B::BitArray{N})

   UNDOCUMENTED

.. function:: flipbits!(B::BitArray{N})

   UNDOCUMENTED

.. function:: rol(B::BitArray{1},i::Integer)

   UNDOCUMENTED

.. function:: ror(B::BitArray{1},i::Integer)

   UNDOCUMENTED


dequeues
--------

.. function:: prepend!(a::Array{T,1},items::Array{T,1})

   UNDOCUMENTED


collections
-----------

.. function:: issubset(...)

   UNDOCUMENTED


strings and text output
-----------------------

.. function:: digits(...)

   UNDOCUMENTED

.. function:: eachmatch(...)

   UNDOCUMENTED

.. function:: escape_string(s::String)

   UNDOCUMENTED

.. function:: first_utf8_byte(c::Char)

   UNDOCUMENTED

.. function:: float32_isvalid(...)

   UNDOCUMENTED

.. function:: float64_isvalid(...)

   UNDOCUMENTED

.. function:: info(msg::String...)

   UNDOCUMENTED

.. function:: is_utf8_start(byte::Uint8)

   UNDOCUMENTED

.. function:: lcfirst(...)

   UNDOCUMENTED

.. function:: match(...)

   UNDOCUMENTED

.. function:: matchall(...)

   UNDOCUMENTED

.. function:: ndigits0z(...)

   UNDOCUMENTED

.. function:: print_escaped(io,s::String,esc::String)

   UNDOCUMENTED

.. function:: print_joined(...)

   UNDOCUMENTED

.. function:: print_matrix(...)

   UNDOCUMENTED

.. function:: print_quoted(io,s::String)

   UNDOCUMENTED

.. function:: print_quoted_literal(io,s::String)

   UNDOCUMENTED

.. function:: print_shortest(...)

   UNDOCUMENTED

.. function:: print_unescaped(io,s::String)

   UNDOCUMENTED

.. function:: print_unescaped_chars(io,s::String,esc::String)

   UNDOCUMENTED

.. function:: print_with_color(...)

   UNDOCUMENTED

.. function:: repeat(...)

   UNDOCUMENTED

.. function:: repl_show(...)

   UNDOCUMENTED

.. function:: rsearch(...)

   UNDOCUMENTED

.. function:: showcompact(...)

   UNDOCUMENTED

.. function:: sprint(...)

   UNDOCUMENTED

.. function:: summary(...)

   UNDOCUMENTED

.. function:: ucfirst(...)

   UNDOCUMENTED

.. function:: unescape_chars(s::String,esc::String)

   UNDOCUMENTED

.. function:: unescape_string(s::String)

   UNDOCUMENTED

.. function:: warn(msg::String...)

   UNDOCUMENTED

.. function:: xdump(...)

   UNDOCUMENTED


random numbers
--------------

.. function:: randn!(A::Array{Float64,N})

   UNDOCUMENTED


bigfloat & precision
--------------------

.. function:: get_precision(...)

   UNDOCUMENTED

.. function:: get_bigfloat_precision()

   UNDOCUMENTED

.. function:: set_bigfloat_precision(x::Int64)

   UNDOCUMENTED

.. function:: with_bigfloat_precision(f::Function,precision::Integer)

   UNDOCUMENTED

.. function:: get_bigfloat_rounding()

   UNDOCUMENTED

.. function:: set_bigfloat_rounding(x::Int64)

   UNDOCUMENTED

.. function:: with_bigfloat_rounding(f::Function,rounding::Integer)

   UNDOCUMENTED


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
-----------------------


iteration
---------


object identity and equality
----------------------------

.. function:: deepcopy_internal(...)

   UNDOCUMENTED


tasks and conditions
--------------------


time
----


errors
------

.. function:: systemerror(p,b::Bool)

   UNDOCUMENTED


types
-----


syntax
------

.. function:: esc(e::ANY)

   UNDOCUMENTED

.. function:: expand(x)

   UNDOCUMENTED

.. function:: gensym(...)

   UNDOCUMENTED

.. function:: macroexpand(x)

   UNDOCUMENTED

.. function:: parse(...)

   UNDOCUMENTED


help and reflection
-------------------

.. function:: disassemble(...)

   UNDOCUMENTED

.. function:: finfer(f::Union(Function,DataType),types)

   UNDOCUMENTED

.. function:: functionlocs(...)

   UNDOCUMENTED

.. function:: less(...)

   UNDOCUMENTED

.. function:: versioninfo(...)

   UNDOCUMENTED

.. function:: whicht(f,types)

   UNDOCUMENTED


loading source files
--------------------


RTS internals
-------------

.. function:: gc_bytes()

   UNDOCUMENTED

.. function:: precompile(f,args::(Any...,))

   UNDOCUMENTED


misc
----

.. function:: tty_cols()

   UNDOCUMENTED

.. function:: tty_rows()

   UNDOCUMENTED


IP address stuff
----------------

.. function:: parse_ipv4(str)

   UNDOCUMENTED

.. function:: parse_ipv6(str)

   UNDOCUMENTED


I/O and events
--------------

.. function:: accept(...)

   UNDOCUMENTED

.. function:: bind(...)

   UNDOCUMENTED

.. function:: countlines(...)

   UNDOCUMENTED

.. function:: eatwspace(s::IOStream)

   UNDOCUMENTED

.. function:: eatwspace_comment(s::IOStream,cmt::Char)

   UNDOCUMENTED

.. function:: fd(...)

   UNDOCUMENTED

.. function:: isopen(...)

   UNDOCUMENTED

.. function:: isreadonly(...)

   UNDOCUMENTED

.. function:: mmap_grow(len::Integer,prot::Integer,flags::Integer,fd::Integer,offset::Int64)

   UNDOCUMENTED

.. function:: mmap_stream_settings(s::IO)

   UNDOCUMENTED

.. function:: nb_available(...)

   UNDOCUMENTED

.. function:: open_any_tcp_port(...)

   UNDOCUMENTED

.. function:: PipeBuffer(...)

   UNDOCUMENTED

.. function:: poll_fd(s,seconds::Real)

   UNDOCUMENTED

.. function:: poll_file(s,interval_seconds::Real,seconds::Real)

   UNDOCUMENTED

.. function:: readavailable(this::AsyncStream)

   UNDOCUMENTED

.. function:: readchomp(x)

   UNDOCUMENTED

.. function:: readdir(...)

   UNDOCUMENTED

.. function:: redirect_stderr(...)

   UNDOCUMENTED

.. function:: redirect_stdin(...)

   UNDOCUMENTED

.. function:: redirect_stdout(...)

   UNDOCUMENTED

.. function:: start_reading(...)

   UNDOCUMENTED

.. function:: start_timer(...)

   UNDOCUMENTED

.. function:: start_watching(...)

   UNDOCUMENTED

.. function:: stop_reading(stream::AsyncStream)

   UNDOCUMENTED

.. function:: stop_timer(timer::TimeoutAsyncWork)

   UNDOCUMENTED

.. function:: truncate(...)

   UNDOCUMENTED

.. function:: uv_error(...)

   UNDOCUMENTED


multiprocessing
---------------

.. function:: interrupt(...)

   UNDOCUMENTED

.. function:: isready(rr::RemoteRef)

   UNDOCUMENTED


distributed arrays
------------------

.. function:: localpart(d::DArray{T,N,A})

   UNDOCUMENTED


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

.. function:: pipeline_error(...)

   UNDOCUMENTED

.. function:: process_options(args::Array{Any,1})

   UNDOCUMENTED

.. function:: process_signaled(s::Process)

   UNDOCUMENTED

.. function:: process_status(s::Process)

   UNDOCUMENTED

.. function:: process_term_signal(s::Process)

   UNDOCUMENTED

.. function:: spawn_nostdin(...)

   UNDOCUMENTED


C interface
-----------

.. function:: c_malloc(size::Integer)

   UNDOCUMENTED

.. function:: disable_sigint(f::Function)

   UNDOCUMENTED

.. function:: dlopen_e(...)

   UNDOCUMENTED

.. function:: pointer_from_objref(x)

   UNDOCUMENTED

.. function:: reenable_sigint(f::Function)

   UNDOCUMENTED

.. function:: unsafe_copy!(...)

   UNDOCUMENTED

.. function:: unsafe_pointer_to_objref(p::Ptr{T})

   UNDOCUMENTED


Macros
------

.. function:: assert(...)

   UNDOCUMENTED

.. function:: which(f,args...)

   UNDOCUMENTED

.. function:: gensym(...)

   UNDOCUMENTED

.. function:: eval(...)

   UNDOCUMENTED

.. function:: show(...)

   UNDOCUMENTED

