# This file is a part of Julia. License is MIT: https://julialang.org/license

module Base

Core._import(Base, Core, :_eval_import, :_eval_import, true)
Core._import(Base, Core, :_eval_using, :_eval_using, true)

using .Core.Intrinsics, .Core.IR

macro max_methods_1(f::Symbol)
    :(typeof(function $f end).name.max_methods = 0x1)
end
macro max_methods_2(f::Symbol)
    :(typeof(function $f end).name.max_methods = 0x2)
end

# Minimize world-splitting for functions to which users or package authors
# will add more methods to.
#
# `@max_methods_2` is for functions which take type arguments and need
# a method for the bottom type. `@max_methods_1` does not suffice in that case.
#
# To re-generate the below list of functions:
#
# * Use the following script to generate the initial list.
#
# * Then, manually delete lines corresponding to functions to which
#   users or package authors are not allowed to add methods to.
#
# * Then, manually adjust some lines to `@max_methods_2` where necessary.
#
# Script to generate the initial list:
#
# ```julia
# function public_generic_functions(io::IO, mod::Module)
#     function f(n::Symbol)
#         g = getproperty(mod, n)
#         (
#             (g isa Function) &&
#             (!(g isa Core.IntrinsicFunction)) &&  # exclude intrinsic functions
#             (!(g isa Core.Builtin)) &&            # exclude built-in functions
#             Base.issingletontype(typeof(g)) &&    # only include functions of singleton type
#             (string(n) == string(g)) &&           # exclude alias bindings
#             (string(n)[1] != '@') &&              # exclude macros
#             (parentmodule(g) == mod)              # only include functions that belong to this module
#         )
#     end
#     ns = names(mod)
#     it1 = Iterators.filter(f, ns)
#     it2 = Iterators.map(Base.Fix1(getproperty, mod), it1)
#     g1 = Base.Fix1(print, io)
#     g2 = Base.Fix1(g1, "@max_methods_1 ")
#     g3 = Base.Fix2(g2, '\n')
#     foreach(g3, it2)
# end
# public_generic_functions(stdout, Base)
# ```
@max_methods_1 !
@max_methods_1 !=
@max_methods_1 &
@max_methods_1 *
@max_methods_1 +
@max_methods_1 -
@max_methods_1 /
@max_methods_1 //
@max_methods_1 <
@max_methods_1 <<
@max_methods_1 <=
@max_methods_1 ==
@max_methods_1 >
@max_methods_1 >=
@max_methods_1 >>
@max_methods_1 >>>
@max_methods_1 PipeBuffer
@max_methods_1 \
@max_methods_1 ^
@max_methods_1 __has_internal_change
@max_methods_1 __precompile__
@max_methods_1 abs
@max_methods_1 abs2
@max_methods_1 accumulate
@max_methods_1 accumulate!
@max_methods_1 acos
@max_methods_1 acosh
@max_methods_1 acquire
@max_methods_1 active_manifest
@max_methods_1 active_project
@max_methods_1 addenv
@max_methods_1 adjoint
@max_methods_1 all
@max_methods_1 all!
@max_methods_1 allequal
@max_methods_1 allunique
@max_methods_1 angle
@max_methods_1 any
@max_methods_1 any!
@max_methods_1 append!
@max_methods_1 argmax
@max_methods_1 argmin
@max_methods_1 ascii
@max_methods_1 asin
@max_methods_1 asinh
@max_methods_1 asyncmap
@max_methods_1 asyncmap!
@max_methods_1 atan
@max_methods_1 atanh
@max_methods_1 atexit
@max_methods_1 atreplinit
@max_methods_1 axes
@max_methods_1 backtrace
@max_methods_1 big
@max_methods_1 bind
@max_methods_1 binomial
@max_methods_1 bitreverse
@max_methods_1 bitrotate
@max_methods_1 bitstring
@max_methods_1 bswap
@max_methods_1 bytes2hex
@max_methods_1 bytesavailable
@max_methods_1 cat
@max_methods_1 catch_backtrace
@max_methods_1 cconvert
@max_methods_1 ceil
@max_methods_1 checkbounds
@max_methods_1 checked_length
@max_methods_1 checkindex
@max_methods_1 chomp
@max_methods_1 chop
@max_methods_1 chopprefix
@max_methods_1 chopsuffix
@max_methods_1 circcopy!
@max_methods_1 circshift
@max_methods_1 circshift!
@max_methods_1 cis
@max_methods_1 cispi
@max_methods_1 clamp
@max_methods_1 clamp!
@max_methods_1 cld
@max_methods_1 close
@max_methods_1 closewrite
@max_methods_1 cmp
@max_methods_1 coalesce
@max_methods_1 code_lowered
@max_methods_1 code_typed
@max_methods_1 codepoint
@max_methods_1 codeunit
@max_methods_1 codeunits
@max_methods_1 collect
@max_methods_1 complex
@max_methods_1 conj
@max_methods_1 conj!
@max_methods_1 contains
@max_methods_1 convert
@max_methods_1 copy
@max_methods_1 copy!
@max_methods_1 copyline
@max_methods_1 copysign
@max_methods_1 copyto!
@max_methods_1 copyuntil
@max_methods_1 cos
@max_methods_1 cosh
@max_methods_1 count
@max_methods_1 count!
@max_methods_1 count_ones
@max_methods_1 count_zeros
@max_methods_1 countlines
@max_methods_1 ctruncate
@max_methods_1 cumprod
@max_methods_1 cumprod!
@max_methods_1 cumsum
@max_methods_1 cumsum!
@max_methods_1 current_exceptions
@max_methods_1 current_task
@max_methods_1 deepcopy
@max_methods_1 delete!
@max_methods_1 deleteat!
@max_methods_1 denominator
@max_methods_1 depwarn
@max_methods_1 detach
@max_methods_1 diff
@max_methods_1 digits
@max_methods_1 digits!
@max_methods_1 disable_sigint
@max_methods_1 displaysize
@max_methods_1 div
@max_methods_1 divrem
@max_methods_1 download
@max_methods_1 dropdims
@max_methods_1 dump
@max_methods_1 eachcol
@max_methods_1 eachindex
@max_methods_1 eachline
@max_methods_1 eachmatch
@max_methods_1 eachrow
@max_methods_1 eachrsplit
@max_methods_1 eachslice
@max_methods_1 eachsplit
@max_methods_1 elsize
@max_methods_1 eltype
@max_methods_1 empty
@max_methods_1 empty!
@max_methods_1 endswith
@max_methods_1 eof
@max_methods_1 eps
@max_methods_1 error
@max_methods_1 errormonitor
@max_methods_1 esc
@max_methods_1 escape_microsoft_c_args
@max_methods_1 escape_raw_string
@max_methods_1 escape_string
@max_methods_1 evalfile
@max_methods_1 exit
@max_methods_1 exit_on_sigint
@max_methods_1 exp
@max_methods_1 exp10
@max_methods_1 exp2
@max_methods_1 expm1
@max_methods_1 extrema
@max_methods_1 extrema!
@max_methods_1 factorial
@max_methods_1 falses
@max_methods_1 fd
@max_methods_1 fdio
@max_methods_1 fetch
@max_methods_1 fieldcount
@max_methods_1 fieldindex
@max_methods_1 fieldname
@max_methods_1 fieldnames
@max_methods_1 fieldoffset
@max_methods_1 fieldtypes
@max_methods_1 filesize
@max_methods_1 fill
@max_methods_1 fill!
@max_methods_1 filter
@max_methods_1 filter!
@max_methods_1 finalize
@max_methods_1 finalizer
@max_methods_1 findall
@max_methods_1 findfirst
@max_methods_1 findlast
@max_methods_1 findmax
@max_methods_1 findmax!
@max_methods_1 findmin
@max_methods_1 findmin!
@max_methods_1 findnext
@max_methods_1 findprev
@max_methods_1 first
@max_methods_1 firstindex
@max_methods_1 fld
@max_methods_1 fld1
@max_methods_1 fldmod
@max_methods_1 fldmod1
@max_methods_1 flipsign
@max_methods_1 float
@max_methods_1 floatmax
@max_methods_1 floatmin
@max_methods_1 floor
@max_methods_1 flush
@max_methods_1 fma
@max_methods_1 foldl
@max_methods_1 foldr
@max_methods_1 foreach
@max_methods_1 front
@max_methods_1 fullname
@max_methods_1 functionloc
@max_methods_1 gcd
@max_methods_1 gcdx
@max_methods_1 gensym
@max_methods_1 get
@max_methods_1 get!
@max_methods_1 get_extension
@max_methods_1 getindex
@max_methods_1 getkey
@max_methods_1 getproperty
@max_methods_1 has_offset_axes
@max_methods_1 hasfield
@max_methods_1 hash
@max_methods_1 haskey
@max_methods_1 hasmethod
@max_methods_1 hasproperty
@max_methods_1 hcat
@max_methods_1 hex2bytes
@max_methods_1 hex2bytes!
@max_methods_1 htol
@max_methods_1 hton
@max_methods_1 hvcat
@max_methods_1 hvncat
@max_methods_1 identify_package
@max_methods_1 identity
@max_methods_1 ifelse
@max_methods_1 ignorestatus
@max_methods_1 imag
@max_methods_1 in
@max_methods_1 in!
@max_methods_1 include_dependency
@max_methods_1 include_string
@max_methods_1 indexin
@max_methods_1 insert!
@max_methods_1 insertdims
@max_methods_1 instances
@max_methods_1 intersect
@max_methods_1 intersect!
@max_methods_1 inv
@max_methods_1 invmod
@max_methods_1 invperm
@max_methods_1 invpermute!
@max_methods_1 isa_ast_node
@max_methods_1 isabstracttype
@max_methods_1 isambiguous
@max_methods_1 isapprox
@max_methods_1 isascii
@max_methods_1 isassigned
@max_methods_1 isbits
@max_methods_1 isbitstype
@max_methods_1 isconcretetype
@max_methods_1 isconst
@max_methods_1 isdisjoint
@max_methods_1 isdispatchtuple
@max_methods_1 isdone
@max_methods_1 isempty
@max_methods_1 isequal
@max_methods_1 iseven
@max_methods_1 isexecutable
@max_methods_1 isexported
@max_methods_1 isexpr
@max_methods_1 isfinite
@max_methods_1 isfull
@max_methods_1 isimmutable
@max_methods_1 isinf
@max_methods_1 isinteger
@max_methods_1 isinteractive
@max_methods_1 isless
@max_methods_1 islocked
@max_methods_1 ismalformed
@max_methods_1 ismarked
@max_methods_1 ismissing
@max_methods_1 ismutable
@max_methods_1 ismutabletype
@max_methods_1 isnan
@max_methods_1 isnegative
@max_methods_1 isnothing
@max_methods_1 isodd
@max_methods_1 isone
@max_methods_1 isopen
@max_methods_1 isoverlong
@max_methods_1 isperm
@max_methods_1 ispositive
@max_methods_1 ispow2
@max_methods_1 isprimitivetype
@max_methods_1 ispublic
@max_methods_1 isqrt
@max_methods_1 isreadable
@max_methods_1 isreadonly
@max_methods_1 isready
@max_methods_1 isreal
@max_methods_1 issetequal
@max_methods_1 issingletontype
@max_methods_1 issorted
@max_methods_1 isstructtype
@max_methods_1 issubnormal
@max_methods_1 issubset
@max_methods_1 istaskdone
@max_methods_1 istaskfailed
@max_methods_1 istaskstarted
@max_methods_1 isunordered
@max_methods_1 isvalid
@max_methods_1 iswritable
@max_methods_1 iszero
@max_methods_1 iterate
@max_methods_1 jit_total_bytes
@max_methods_1 join
@max_methods_1 keepat!
@max_methods_1 keys
@max_methods_1 keytype
@max_methods_1 kill
@max_methods_1 kron
@max_methods_1 kron!
@max_methods_1 last
@max_methods_1 lastindex
@max_methods_1 lcm
@max_methods_1 leading_ones
@max_methods_1 leading_zeros
@max_methods_1 length
@max_methods_1 link_pipe!
@max_methods_1 load_path
@max_methods_1 locate_package
@max_methods_1 lock
@max_methods_1 log
@max_methods_1 log10
@max_methods_1 log1p
@max_methods_1 log2
@max_methods_1 logrange
@max_methods_1 lpad
@max_methods_1 lstrip
@max_methods_1 ltoh
@max_methods_1 ltruncate
@max_methods_1 macroexpand
@max_methods_1 macroexpand!
@max_methods_1 map
@max_methods_1 map!
@max_methods_1 mapfoldl
@max_methods_1 mapfoldr
@max_methods_1 mapreduce
@max_methods_1 mapslices
@max_methods_1 mark
@max_methods_1 match
@max_methods_1 max
@max_methods_1 maximum
@max_methods_1 maximum!
@max_methods_1 maxintfloat
@max_methods_1 memoryindex
@max_methods_1 merge
@max_methods_1 merge!
@max_methods_1 mergewith
@max_methods_1 mergewith!
@max_methods_1 methods
@max_methods_1 min
@max_methods_1 minimum
@max_methods_1 minimum!
@max_methods_1 minmax
@max_methods_1 mod
@max_methods_1 mod1
@max_methods_1 modifyproperty!
@max_methods_1 moduleroot
@max_methods_1 mul_hi
@max_methods_1 muladd
@max_methods_1 nameof
@max_methods_1 names
@max_methods_1 nand
@max_methods_1 ncodeunits
@max_methods_1 ndigits
@max_methods_1 ndims
@max_methods_1 nextfloat
@max_methods_1 nextind
@max_methods_1 nextpow
@max_methods_1 nextprod
@max_methods_1 nonmissingtype
@max_methods_1 nor
@max_methods_1 notify
@max_methods_1 notnothing
@max_methods_1 ntoh
@max_methods_1 ntuple
@max_methods_1 numerator
@max_methods_1 objectid
@max_methods_1 occursin
@max_methods_1 oftype
@max_methods_1 one
@max_methods_1 ones
@max_methods_1 oneunit
@max_methods_1 open
@max_methods_1 operator_associativity
@max_methods_1 operator_precedence
@max_methods_1 pairs
@max_methods_1 parent
@max_methods_1 parentindices
@max_methods_1 parentmodule
@max_methods_1 parse
@max_methods_1 pathof
@max_methods_1 peek
@max_methods_1 permute!
@max_methods_1 permutedims
@max_methods_1 permutedims!
@max_methods_1 pipeline
@max_methods_1 pkgdir
@max_methods_1 pkgversion
@max_methods_1 pointer
@max_methods_1 pointer_from_objref
@max_methods_1 pop!
@max_methods_1 popat!
@max_methods_1 popfirst!
@max_methods_1 position
@max_methods_1 powermod
@max_methods_1 precision
@max_methods_1 precompile
@max_methods_1 prepend!
@max_methods_1 prevfloat
@max_methods_1 prevind
@max_methods_1 prevpow
@max_methods_1 print
@max_methods_1 println
@max_methods_1 printstyled
@max_methods_1 process_exited
@max_methods_1 process_running
@max_methods_1 prod
@max_methods_1 prod!
@max_methods_1 promote
@max_methods_1 promote_rule
@max_methods_1 promote_shape
@max_methods_1 promote_type
@max_methods_1 propertynames
@max_methods_1 push!
@max_methods_1 pushfirst!
@max_methods_1 put!
@max_methods_1 quoted
@max_methods_1 rand
@max_methods_1 randn
@max_methods_1 range
@max_methods_1 rationalize
@max_methods_1 read
@max_methods_1 read!
@max_methods_1 readavailable
@max_methods_1 readbytes!
@max_methods_1 readchomp
@max_methods_1 readeach
@max_methods_1 readline
@max_methods_1 readlines
@max_methods_1 readuntil
@max_methods_1 real
@max_methods_1 redirect_stdio
@max_methods_1 reduce
@max_methods_1 reenable_sigint
@max_methods_1 reim
@max_methods_1 reinterpret
@max_methods_1 release
@max_methods_1 rem
@max_methods_1 remove_linenums!
@max_methods_1 repeat
@max_methods_1 replace
@max_methods_1 replace!
@max_methods_1 replaceproperty!
@max_methods_1 repr
@max_methods_1 require_one_based_indexing
@max_methods_1 reset
@max_methods_1 reseteof
@max_methods_1 reshape
@max_methods_1 resize!
@max_methods_1 rest
@max_methods_1 rethrow
@max_methods_1 retry
@max_methods_1 reverse
@max_methods_1 reverse!
@max_methods_1 reverseind
@max_methods_1 rot180
@max_methods_1 rotl90
@max_methods_1 rotr90
@max_methods_1 round
@max_methods_1 rpad
@max_methods_1 rsplit
@max_methods_1 rstrip
@max_methods_1 rtruncate
@max_methods_1 run
@max_methods_1 runtests
@max_methods_1 schedule
@max_methods_1 seek
@max_methods_1 seekend
@max_methods_1 seekstart
@max_methods_1 selectdim
@max_methods_1 setcpuaffinity
@max_methods_1 setdiff
@max_methods_1 setdiff!
@max_methods_1 setenv
@max_methods_1 setgid
@max_methods_1 setindex!
@max_methods_1 setproperty!
@max_methods_1 setpropertyonce!
@max_methods_1 setuid
@max_methods_1 shell_escape
@max_methods_1 shell_escape_csh
@max_methods_1 shell_escape_posixly
@max_methods_1 shell_escape_wincmd
@max_methods_1 shell_split
@max_methods_1 show
@max_methods_1 show_invalid
@max_methods_1 showarg
@max_methods_1 showerror
@max_methods_1 sign
@max_methods_1 signbit
@max_methods_1 signed
@max_methods_1 similar
@max_methods_1 sin
@max_methods_1 sinh
@max_methods_1 size
@max_methods_1 sizehint!
@max_methods_1 sizeof
@max_methods_1 skip
@max_methods_1 skipchars
@max_methods_1 skipmissing
@max_methods_1 sleep
@max_methods_1 something
@max_methods_1 sort
@max_methods_1 sort!
@max_methods_1 sortperm
@max_methods_1 sortslices
@max_methods_1 splat
@max_methods_1 splice!
@max_methods_1 split
@max_methods_1 split_rest
@max_methods_1 sprint
@max_methods_1 sqrt
@max_methods_1 stack
@max_methods_1 startswith
@max_methods_1 stat
@max_methods_1 step
@max_methods_1 stride
@max_methods_1 strides
@max_methods_1 string
@max_methods_1 strip
@max_methods_1 success
@max_methods_1 sum
@max_methods_1 sum!
@max_methods_1 summary
@max_methods_1 summarysize
@max_methods_1 supertype
@max_methods_1 swapproperty!
@max_methods_1 symdiff
@max_methods_1 symdiff!
@max_methods_1 systemerror
@max_methods_1 tail
@max_methods_1 take!
@max_methods_1 takestring!
@max_methods_1 tan
@max_methods_1 tanh
@max_methods_1 task_local_storage
@max_methods_1 thisind
@max_methods_1 time_ns
@max_methods_1 timedwait
@max_methods_1 to_index
@max_methods_1 to_indices
@max_methods_1 trailing_ones
@max_methods_1 trailing_zeros
@max_methods_1 transcode
@max_methods_1 transpose
@max_methods_1 trues
@max_methods_1 trunc
@max_methods_1 truncate
@max_methods_1 trylock
@max_methods_1 tryparse
@max_methods_1 typeintersect
@max_methods_1 typejoin
@max_methods_1 typemax
@max_methods_1 typemin
@max_methods_1 uabs
@max_methods_1 unescape_string
@max_methods_1 union
@max_methods_1 union!
@max_methods_1 unique
@max_methods_1 unique!
@max_methods_1 unlock
@max_methods_1 unmark
@max_methods_1 unsafe_convert
@max_methods_1 unsafe_copyto!
@max_methods_1 unsafe_load
@max_methods_1 unsafe_modify!
@max_methods_1 unsafe_pointer_to_objref
@max_methods_1 unsafe_read
@max_methods_1 unsafe_replace!
@max_methods_1 unsafe_store!
@max_methods_1 unsafe_string
@max_methods_1 unsafe_swap!
@max_methods_1 unsafe_trunc
@max_methods_1 unsafe_wrap
@max_methods_1 unsafe_write
@max_methods_1 unsigned
@max_methods_1 valtype
@max_methods_1 values
@max_methods_1 vcat
@max_methods_1 vec
@max_methods_1 vect
@max_methods_1 view
@max_methods_1 wait
@max_methods_1 waitall
@max_methods_1 waitany
@max_methods_1 which
@max_methods_1 widemul
@max_methods_1 widen
@max_methods_1 windowserror
@max_methods_1 withenv
@max_methods_1 write
@max_methods_1 xor
@max_methods_1 yield
@max_methods_1 yieldto
@max_methods_1 zero
@max_methods_1 zeros
@max_methods_1 |
@max_methods_1 |>
@max_methods_1 ~
@max_methods_1 ∉
@max_methods_1 ∋
@max_methods_1 ∌
@max_methods_1 ∘
@max_methods_1 ≉
@max_methods_1 ⊇
@max_methods_1 ⊈
@max_methods_1 ⊉
@max_methods_1 ⊊
@max_methods_1 ⊋

# to start, we're going to use a very simple definition of `include`
# that doesn't require any function (except what we can get from the `Core` top-module)
# start this big so that we don't have to resize before we have defined how to grow an array
const _included_files = Array{Tuple{Module,String},1}(Core.undef, 400)
setfield!(_included_files, :size, (1,))
function include(mod::Module, path::String)
    len = getfield(_included_files.size, 1)
    memlen = _included_files.ref.mem.length
    lenp1 = Core.add_int(len, 1)
    if len === memlen # by the time this is true we hopefully will have defined _growend!
        _growend!(_included_files, UInt(1))
    else
        setfield!(_included_files, :size, (lenp1,))
    end
    Core.memoryrefset!(Core.memoryref(_included_files.ref, lenp1), (mod, ccall(:jl_prepend_cwd, Any, (Any,), path)), :not_atomic, true)
    Core.println(path)
    ccall(:jl_uv_flush, Nothing, (Ptr{Nothing},), Core.io_pointer(Core.stdout))
    Core.include(mod, path)
end
include(path::String) = include(Base, path)

struct IncludeInto <: Function
    m::Module
end
(this::IncludeInto)(fname::AbstractString) = include(this.m, fname)

# from now on, this is now a top-module for resolving syntax
const is_primary_base_module = ccall(:jl_module_parent, Ref{Module}, (Any,), Base) === Core.Main
ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Base, is_primary_base_module)

# The @inline/@noinline macros that can be applied to a function declaration are not available
# until after array.jl, and so we will mark them within a function body instead.
macro inline()   Expr(:meta, :inline)   end
macro noinline() Expr(:meta, :noinline) end

macro _boundscheck() Expr(:boundscheck) end

# Try to help prevent users from shooting them-selves in the foot
# with ambiguities by defining a few common and critical operations
# (and these don't need the extra convert code)
getproperty(x::Module, f::Symbol) = (@inline; getglobal(x, f))
getproperty(x::Type, f::Symbol) = (@inline; getfield(x, f))
setproperty!(x::Type, f::Symbol, v) = error("setfield! fields of Types should not be changed")
setproperty!(x::Array, f::Symbol, v) = error("setfield! fields of Array should not be changed")
getproperty(x::Tuple, f::Int) = (@inline; getfield(x, f))
setproperty!(x::Tuple, f::Int, v) = setfield!(x, f, v) # to get a decent error

getproperty(x, f::Symbol) = (@inline; getfield(x, f))
function setproperty!(x, f::Symbol, v)
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return setfield!(x, f, val)
end

typeof(function getproperty end).name.constprop_heuristic = Core.FORCE_CONST_PROP
typeof(function setproperty! end).name.constprop_heuristic = Core.FORCE_CONST_PROP

dotgetproperty(x, f) = getproperty(x, f)

getproperty(x::Module, f::Symbol, order::Symbol) = (@inline; getglobal(x, f, order))
function setproperty!(x::Module, f::Symbol, v, order::Symbol=:monotonic)
    @inline
    ty = Core.get_binding_type(x, f)
    val = v isa ty ? v : convert(ty, v)
    return setglobal!(x, f, val, order)
end
getproperty(x::Type, f::Symbol, order::Symbol) = (@inline; getfield(x, f, order))
setproperty!(x::Type, f::Symbol, v, order::Symbol) = error("setfield! fields of Types should not be changed")
getproperty(x::Tuple, f::Int, order::Symbol) = (@inline; getfield(x, f, order))
setproperty!(x::Tuple, f::Int, v, order::Symbol) = setfield!(x, f, v, order) # to get a decent error

getproperty(x, f::Symbol, order::Symbol) = (@inline; getfield(x, f, order))
function setproperty!(x, f::Symbol, v, order::Symbol)
    @inline
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return setfield!(x, f, val, order)
end

function swapproperty!(x, f::Symbol, v, order::Symbol=:not_atomic)
    @inline
    ty = fieldtype(typeof(x), f)
    val = v isa ty ? v : convert(ty, v)
    return Core.swapfield!(x, f, val, order)
end
function modifyproperty!(x, f::Symbol, op, v, order::Symbol=:not_atomic)
    @inline
    return Core.modifyfield!(x, f, op, v, order)
end
function replaceproperty!(x, f::Symbol, expected, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = fieldtype(typeof(x), f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.replacefield!(x, f, expected, val, success_order, fail_order)
end
function setpropertyonce!(x, f::Symbol, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = fieldtype(typeof(x), f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.setfieldonce!(x, f, val, success_order, fail_order)
end

function swapproperty!(x::Module, f::Symbol, v, order::Symbol=:not_atomic)
    @inline
    ty = Core.get_binding_type(x, f)
    val = v isa ty ? v : convert(ty, v)
    return Core.swapglobal!(x, f, val, order)
end
function modifyproperty!(x::Module, f::Symbol, op, v, order::Symbol=:not_atomic)
    @inline
    return Core.modifyglobal!(x, f, op, v, order)
end
function replaceproperty!(x::Module, f::Symbol, expected, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = Core.get_binding_type(x, f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.replaceglobal!(x, f, expected, val, success_order, fail_order)
end
function setpropertyonce!(x::Module, f::Symbol, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    @inline
    ty = Core.get_binding_type(x, f)
    val = desired isa ty ? desired : convert(ty, desired)
    return Core.setglobalonce!(x, f, val, success_order, fail_order)
end

convert(::Type{Any}, Core.@nospecialize x) = x
convert(::Type{T}, x::T) where {T} = x
include("coreio.jl")

import Core: @doc, @__doc__, WrappedException, @int128_str, @uint128_str, @big_str, @cmd

# Export list
include("exports.jl")

function set_syntax_version end
_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module
function _setup_module!(mod::Module, Core.@nospecialize syntax_ver)
    # using Base
    Core._using(mod, _topmod(mod), UInt8(0))
    Core.declare_const(mod, :include, IncludeInto(mod))
    Core.declare_const(mod, :eval, Core.EvalInto(mod))
    if syntax_ver === nothing
        return nothing
    end
    set_syntax_version(mod, syntax_ver)
    return nothing
end

# core docsystem
include("docs/core.jl")
Core.atdoc!(CoreDocs.docm)

eval(x) = Core.eval(Base, x)
eval(m::Module, x) = Core.eval(m, x)

include("public.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    # otherwise, they just just eventually get (noisily) overwritten later
    global show, print, println
    show(io::IO, x) = Core.show(io, x)
    print(io::IO, a...) = Core.print(io, a...)
    println(io::IO, x...) = Core.println(io, x...)
end

## Load essential files and libraries
include("essentials.jl")

# Because lowering inserts direct references, it is mandatory for this binding
# to exist before we start inferring code.
function string end
import Core: String

# For OS specific stuff
# We need to strcat things here, before strings are really defined
function strcat(x::String, y::String)
    out = ccall(:jl_alloc_string, Ref{String}, (Int,), Core.sizeof(x) + Core.sizeof(y))
    gc_x = @_gc_preserve_begin(x)
    gc_y = @_gc_preserve_begin(y)
    gc_out = @_gc_preserve_begin(out)
    out_ptr = unsafe_convert(Ptr{UInt8}, out)
    unsafe_copyto!(out_ptr, unsafe_convert(Ptr{UInt8}, x), Core.sizeof(x))
    unsafe_copyto!(out_ptr + Core.sizeof(x), unsafe_convert(Ptr{UInt8}, y), Core.sizeof(y))
    @_gc_preserve_end(gc_x)
    @_gc_preserve_end(gc_y)
    @_gc_preserve_end(gc_out)
    return out
end


"""
    time_ns()::UInt64

Get the time in nanoseconds relative to some machine-specific arbitrary time in the past.
The primary use is for measuring elapsed times during program execution. The return value is guaranteed to
be monotonic (mod 2⁶⁴) while the system is running, and is unaffected by clock drift or changes to local calendar time,
but it may change arbitrarily across system reboots or suspensions.

(Although the returned time is always in nanoseconds, the timing resolution is platform-dependent.)
"""
time_ns() = ccall(:jl_hrtime, UInt64, ())

# A warning to be interpolated in the docstring of every dangerous mutating function in Base, see PR #50824
const _DOCS_ALIASING_WARNING = """
!!! warning
    Behavior can be unexpected when any mutated argument shares memory with any other argument.
"""

include("ctypes.jl")
include("gcutils.jl")
include("generator.jl")
include("runtime_internals.jl")
include("options.jl")

# define invoke(f, T, args...; kwargs...), without kwargs wrapping
# to forward to invoke
function Core.kwcall(kwargs::NamedTuple, ::typeof(invoke), f, T, args...)
    @inline
    # prepend kwargs and f to the invoked from the user
    T = rewrap_unionall(Tuple{Core.Typeof(kwargs), Core.Typeof(f), (unwrap_unionall(T)::DataType).parameters...}, T)
    return invoke(Core.kwcall, T, kwargs, f, args...)
end
# invoke does not have its own call cache, but kwcall for invoke does
setfield!(typeof(invoke).name, :max_args, Int32(3), :monotonic) # invoke, f, T, args...

# define applicable(f, T, args...; kwargs...), without kwargs wrapping
# to forward to applicable
function Core.kwcall(kwargs::NamedTuple, ::typeof(applicable), @nospecialize(args...))
    @inline
    return applicable(Core.kwcall, kwargs, args...)
end
function Core._hasmethod(@nospecialize(f), @nospecialize(t)) # this function has a special tfunc (TODO: make this a Builtin instead like applicable)
    tt = rewrap_unionall(Tuple{Core.Typeof(f), (unwrap_unionall(t)::DataType).parameters...}, t)
    return Core._hasmethod(tt)
end

"""
    invokelatest(f, args...; kwargs...)

Calls `f(args...; kwargs...)`, but guarantees that the most recent method of `f`
will be executed.   This is useful in specialized circumstances,
e.g. long-running event loops or callback functions that may
call obsolete versions of a function `f`.
(The drawback is that `invokelatest` is somewhat slower than calling
`f` directly, and the type of the result cannot be inferred by the compiler.)

!!! compat "Julia 1.9"
    Prior to Julia 1.9, this function was not exported, and was called as `Base.invokelatest`.
"""
const invokelatest = Core.invokelatest

# define invokelatest(f, args...; kwargs...), without kwargs wrapping
# to forward to invokelatest
function Core.kwcall(kwargs::NamedTuple, ::typeof(invokelatest), f, args...)
    @inline
    return Core.invokelatest(Core.kwcall, kwargs, f, args...)
end
setfield!(typeof(invokelatest).name, :max_args, Int32(2), :monotonic) # invokelatest, f, args...

"""
    invoke_in_world(world, f, args...; kwargs...)

Call `f(args...; kwargs...)` in a fixed world age, `world`.

This is useful for infrastructure running in the user's Julia session which is
not part of the user's program. For example, things related to the REPL, editor
support libraries, etc. In these cases it can be useful to prevent unwanted
method invalidation and recompilation latency, and to prevent the user from
breaking supporting infrastructure by mistake.

The global world age can be queried using [`Base.get_world_counter()`](@ref)
and stored for later use within the lifetime of the current Julia session, or
when serializing and reloading the system image.

Technically, `invoke_in_world` will prevent any function called by `f` from
being extended by the user during their Julia session. That is, generic
function method tables seen by `f` (and any functions it calls) will be frozen
as they existed at the given `world` age. In a sense, this is like the opposite
of [`invokelatest`](@ref).

!!! note
    It is not valid to store world ages obtained in precompilation for later use.
    This is because precompilation generates a "parallel universe" where the
    world age refers to system state unrelated to the main Julia session.
"""
const invoke_in_world = Core.invoke_in_world

function Core.kwcall(kwargs::NamedTuple, ::typeof(invoke_in_world), world::UInt, f, args...)
    @inline
    return Core.invoke_in_world(world, Core.kwcall, kwargs, f, args...)
end
setfield!(typeof(invoke_in_world).name, :max_args, Int32(3), :monotonic) # invoke_in_world, world, f, args...

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("expr.jl")
include("pair.jl")
include("traits.jl")
include("range.jl")
include("error.jl")

# core numeric operations & types
==(x, y) = x === y
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
include("refvalue.jl")
include("cmem.jl")

function nextfloat end
function prevfloat end
include("rounding.jl")
include("float.jl")

# Lazy strings
include("strings/lazy.jl")

function cld end
function fld end
include("checked.jl")
using .Checked

# array structures
include("indices.jl")
include("genericmemory.jl")
include("array.jl")
include("abstractarray.jl")
include("baseext.jl")

include("c.jl")
include("abstractset.jl")
include("bitarray.jl")
include("bitset.jl")
include("abstractdict.jl")
include("iddict.jl")
include("idset.jl")
include("ntuple.jl")
include("iterators.jl")
using .Iterators: zip, enumerate, only
using .Iterators: Flatten, Filter, product  # for generators
using .Iterators: Stateful    # compat (was formerly used in reinterpretarray.jl)
include("namedtuple.jl")

include("anyall.jl")

include("ordering.jl")
using .Order

include("coreir.jl")
include("module.jl")

BUILDROOT::String = ""
DATAROOT::String = ""
const DL_LOAD_PATH = String[]

baremodule BuildSettings end

function process_sysimg_args!()
    let i = 2 # skip file name
        while i <= length(Core.ARGS)
            if Core.ARGS[i] == "--buildsettings"
                include(BuildSettings, ARGS[i+1])
            elseif Core.ARGS[i] == "--buildroot"
                global BUILDROOT = Core.ARGS[i+1]
            elseif Core.ARGS[i] == "--dataroot"
                global DATAROOT = Core.ARGS[i+1]
            else
                error(strcat("invalid sysimage argument: ", Core.ARGS[i]))
            end
            i += 2
        end
    end
end
process_sysimg_args!()

function isready end

include(strcat(DATAROOT, "julia/Compiler/src/Compiler.jl"))
using .Compiler.ReinferUtils: ReinferUtils, invalidate_code_for_globalref!

const _return_type = Compiler.return_type

# Enable compiler
Compiler.bootstrap!()

include("flfrontend.jl")
Core._setparser!(fl_parse)
Core._setlowerer!(fl_lower)

# Further definition of Base will happen in Base.jl if loaded.

# Ensure this file is also tracked
@assert !isassigned(_included_files, 1)
_included_files[1] = (@__MODULE__, ccall(:jl_prepend_cwd, Any, (Any,), "Base_compiler.jl"))

end # module Base
using .Base
