macro deprecate(old,new)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            Expr(:export,esc(old)),
            :(function $(esc(old))(args...)
                  depwarn(string($oldname," is deprecated, use ",$newname," instead."),
                          $oldname)
                  $(esc(new))(args...)
              end))
    elseif isa(old,Expr) && old.head == :call
        oldcall = sprint(io->show_unquoted(io,old))
        newcall = sprint(io->show_unquoted(io,new))
        oldname = Expr(:quote, old.args[1])
        Expr(:toplevel,
            Expr(:export,esc(old.args[1])),
            :($(esc(old)) = begin
                  depwarn(string($oldcall," is deprecated, use ",$newcall," instead."),
                          $oldname)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

function depwarn(msg, funcsym)
    bt = backtrace()
    caller = firstcaller(bt, funcsym)
    warn(msg, once=(caller!=C_NULL), key=caller, bt=bt)
end

function firstcaller(bt::Array{Ptr{None},1}, funcsym::Symbol)
    # Identify the calling line
    i = 1
    while i <= length(bt)
        lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Int32), bt[i], 0)
        i += 1
        if lkup === ()
            continue
        end
        fname, file, line = lkup
        if fname == funcsym
            break
        end
    end
    if i <= length(bt)
        return bt[i]
    end
    return C_NULL
end

# 0.1

const IOString = IOBuffer
export IOString
const PipeString = PipeBuffer
export PipeString

# 0.2

@deprecate  A_mul_B(A,B,C)      A_mul_B!(A,B,C)
@deprecate  A_mul_Bt(A,B,C)     A_mul_Bt!(A,B,C)
@deprecate  At_mul_B(A,B,C)     At_mul_B!(A,B,C)
@deprecate  At_mul_Bt(A,B,C)    At_mul_Bt!(A,B,C)
@deprecate  Ac_mul_B(A,B,C)     Ac_mul_B!(A,B,C)
@deprecate  A_mul_Bc(A,B,C)     A_mul_Bc!(A,B,C) 
@deprecate  Ac_mul_Bc(A,B,C)    Ac_mul_Bc!(A,B,C)
@deprecate  Ac_mul_Bt(A,B,C)    Ac_mul_Bt!(A,B,C)
@deprecate  strchr              search
@deprecate  iswriteable         iswritable
@deprecate  localize            localpart
@deprecate  logb                exponent
@deprecate  ilogb               exponent
@deprecate  ref_shape           index_shape
@deprecate  assign_shape_check  setindex_shape_check
@deprecate  quote_string        repr
@deprecate  safe_char(c)        (is_valid_char(char(c))||error())&&char(c)
@deprecate  check_ascii(x)      (is_valid_ascii(x)?x:error())
@deprecate  check_utf8(x)       (is_valid_utf8(x)?x:error())
@deprecate  each_line           eachline
@deprecate  each_match          eachmatch
@deprecate  function_loc        functionloc
@deprecate  compile_hint        precompile
@deprecate  begins_with         beginswith
@deprecate  ends_with           endswith
@deprecate  parse_float         parsefloat
@deprecate  parse_int           parseint
@deprecate  parse_bin(T,s)      parseint(T,s,2)
@deprecate  parse_bin(s)        parseint(s,2)
@deprecate  parse_oct(T,s)      parseint(T,s,8)
@deprecate  parse_oct(s)        parseint(s,8)
@deprecate  parse_hex(T,s)      parseint(T,s,16)
@deprecate  parse_hex(s)        parseint(s,16)
@deprecate  wait_accept         accept
@deprecate  findn_nzs           findnz
@deprecate  DivideByZeroError   DivideError
@deprecate  cartesian_map       cartesianmap
@deprecate  check_bounds        checkbounds
@deprecate  system_error        systemerror
@deprecate  seek_end            seekend
@deprecate  addprocs_ssh_tunnel(m) addprocs(m, tunnel=true)
@deprecate  addprocs_ssh        addprocs
@deprecate  addprocs_local      addprocs
@deprecate  remote_call         remotecall
@deprecate  remote_call_fetch   remotecall_fetch
@deprecate  remote_call_wait    remotecall_wait
@deprecate  has(s::Set, x)      contains(s, x)
@deprecate  has(s::IntSet, x)   contains(s, x)
@deprecate  has(d,k)            haskey(d,k)
@deprecate  diagmm              scale
@deprecate  diagmm!             scale!
@deprecate  unsafe_ref          unsafe_load
@deprecate  unsafe_assign       unsafe_store!
@deprecate  add_each!           union!
@deprecate  del_each!           setdiff!
@deprecate  real_valued         isreal
@deprecate  integer_valued      isinteger
@deprecate  isdenormal          issubnormal
@deprecate  get_precision       precision
@deprecate  expr(hd, a...)              Expr(hd, a...)
@deprecate  expr(hd, a::Array{Any,1})   Expr(hd, a...)
@deprecate  readdir(cmd::Cmd)           readdir(string(cmd)[2:end-1])
@deprecate  isbool(x)                   iseltype(x,Bool)
@deprecate  iscomplex(x)                iseltype(x,Complex)
@deprecate  lstrip(a::String, b::String) lstrip(a, collect(b))
@deprecate  rstrip(a::String, b::String) rstrip(a, collect(b))
@deprecate  delete!(a::Vector, x)     splice!(a, x)
@deprecate  delete!(a::BitVector, x)  splice!(a, x)
@deprecate  |(s::Set...)              union(s...)
@deprecate  (&)(s::Set...)            intersect(s...)
@deprecate  -(a::Set, b::Set)         setdiff(a,b)
@deprecate  ($)(s1::IntSet, s2::IntSet)  symdiff(s1,s2)
@deprecate  |(s::IntSet, s2::IntSet)     union(s, s2)
@deprecate  (&)(s::IntSet, s2::IntSet)   intersect(s, s2)
@deprecate  -(a::IntSet, b::IntSet)      setdiff(a,b)
@deprecate  ~(s::IntSet)                 complement(s)
@deprecate  openblas_set_num_threads      blas_set_num_threads
@deprecate  check_openblas                check_blas
@deprecate  msync(A::Array, flags::Int)    msync(A)
@deprecate  msync(A::BitArray, flags::Int) msync(A)
@deprecate  square(x::Number)          x*x
@deprecate  finfer                     code_typed
@deprecate  disassemble(f::Function,t::Tuple)           code_llvm(f,t)
@deprecate  disassemble(f::Function,t::Tuple,asm::Bool) (asm ? code_native(f,t) : code_llvm(f,t))
@deprecate  add(s::Set, x)                  push!(s,x)
@deprecate  add!(s::Set, x)                 push!(s,x)
@deprecate  add(s::IntSet, x)               push!(s,x)
@deprecate  add!(s::IntSet, x)              push!(s,x)
@deprecate  delete!(d::Dict, key, default)  pop!(d, key, default)
@deprecate  get(A::Array, B::Array, I, default) get!(A, B, I, default)
@deprecate repl_show(io, x)  writemime(io, MIME"text/plain"(), x)
@deprecate error_show  showerror
@deprecate eatwspace(io)  skipchars(io, isspace)
@deprecate eatwspace_comment(io, cmt)  skipchars(io, isspace, linecomment=cmt)
@deprecate open_any_tcp_port listenany
@deprecate  subtype             issubtype
@deprecate  bsxfun              broadcast
@deprecate max(x)              maximum(x)
@deprecate min(x)              minimum(x)
@deprecate max(f::Function,x)  maximum(f,x)
@deprecate min(f::Function,x)  minimum(f,x)
@deprecate max(x,_::(),d)      maximum(x,d)
@deprecate min(x,_::(),d)      minimum(x,d)
@deprecate assert(x,y)         (@assert x y)
# NOTE: when this deprecation is removed, also remove
#   copy!(dest::AbstractArray, doffs::Integer, src::Integer)
# in abstractarray.jl
@deprecate copy!(dest::AbstractArray, src, doffs::Integer)  copy!(dest, doffs, src)
@deprecate qrpfact!(A)         qrfact!(A, pivot=true)
@deprecate qrpfact(A)          qrfact(A, pivot=true)
@deprecate qrp(A, thin)        qr(A, thin=thin, pivot=true)
@deprecate qrp(A)              qr(A, pivot=true)
@deprecate cholpfact!(A)            cholfact!(A, :U, pivot=true)
@deprecate cholpfact!(A,tol=tol)        cholfact!(A, :U, pivot=true, tol=tol)
@deprecate cholpfact!(A,uplo,tol=tol)   cholfact!(A, uplo, pivot=true, tol=tol)
@deprecate cholpfact(A)             cholfact(A, :U, pivot=true)
@deprecate symmetrize!(A)      Base.LinAlg.copytri!(A, 'U')
@deprecate symmetrize!(A, uplo)      Base.LinAlg.copytri!(A, uplo)
@deprecate factorize!(A)       factorize(A)
@deprecate svdfact(A,thin)      svdfact(A,thin=thin)
@deprecate svdfact!(A,thin)     svdfact(A,thin=thin)
@deprecate svd(A,thin)          svd(A,thin=thin)

deprecated_ls() = run(`ls -l`)
deprecated_ls(args::Cmd) = run(`ls -l $args`)
deprecated_ls(args::String...) = run(`ls -l $args`)
function ls(args...)
    depwarn("ls() is deprecated, use readdir() instead. If you are at the repl prompt, consider `;ls`.", :ls)
    deprecated_ls(args...)
end
export ls

function start_timer(timer::Timer, timeout::Int, repeat::Int)
    depwarn("start_timer now expects arguments in units of seconds. you may need to update your code", :start_timer)
    invoke(start_timer, (Timer,Real,Real), timer, timeout, repeat)
end

# redirection operators
@deprecate |(a::AbstractCmd,b::AbstractCmd) (a|>b)
@deprecate >(a::Redirectable,b::AbstractCmd) (a|>b)
@deprecate >(a::String,b::AbstractCmd) (a|>b)
@deprecate >(a::AbstractCmd,b::Redirectable) (a|>b)
@deprecate >(a::AbstractCmd,b::String) (a|>b)
@deprecate <(a::AbstractCmd,b::String) (b|>a)
@deprecate |(x, f::Function) (x|>f)

@deprecate  SpawnNullStream() DevNull

@deprecate memio(args...)  IOBuffer()

@deprecate user_homedir homedir
@deprecate user_prefdir homedir
@deprecate user_documentsdir homedir

# note removed macros: str, B_str, I_str, E_str, L_str, L_mstr, I_mstr, E_mstr

const ref = getindex
export ref
const assign = setindex!
export assign

const TimeoutAsyncWork = Timer
export TimeoutAsyncWork

# will be removed from exports (moved into Base.Sys): OS_NAME, WORD_SIZE, CPU_CORES

typealias ComplexPair Complex
export ComplexPair

# superseded sorting API

@deprecate select(v::AbstractVector,k::Union(Int,Range1),o::Ordering) select(v,k,order=o)
@deprecate select(v::AbstractVector,k::Union(Int,Range1),f::Function) select(v,k,lt=f)
@deprecate select(f::Function,v::AbstractVector,k::Union(Int,Range1)) select(v,k,lt=f)

# @deprecate select!(v::AbstractVector,k::Union(Int,Range1),o::Ordering) select!(v,k,order=o)
@deprecate select!(v::AbstractVector,k::Union(Int,Range1),f::Function) select!(v,k,lt=f)
@deprecate select!(f::Function,v::AbstractVector,k::Union(Int,Range1)) select!(v,k,lt=f)

@deprecate sort(v::AbstractVector,o::Ordering) sort(v,order=o)
@deprecate sort(v::AbstractVector,a::Algorithm) sort(v,alg=a)
@deprecate sort(v::AbstractVector,a::Algorithm,o::Ordering) sort(v,alg=a,order=o)
@deprecate sort(v::AbstractVector,o::Ordering,a::Algorithm) sort(v,alg=a,order=o)
@deprecate sort(v::AbstractVector,f::Function) sort(v,lt=f)
@deprecate sort(f::Function,v::AbstractVector) sort(v,lt=f)
@deprecate sort(v::AbstractVector,a::Algorithm,f::Function) sort(v,alg=a,lt=f)
@deprecate sort(v::AbstractVector,f::Function,a::Algorithm) sort(v,alg=a,lt=f)
@deprecate sort(f::Function,v::AbstractVector,a::Algorithm) sort(v,alg=a,lt=f)

@deprecate sort!(v::AbstractVector,o::Ordering) sort!(v,order=o)
@deprecate sort!(v::AbstractVector,a::Algorithm) sort!(v,alg=a)
# @deprecate sort!(v::AbstractVector,a::Algorithm,o::Ordering) sort!(v,alg=a,order=o)
@deprecate sort!(v::AbstractVector,o::Ordering,a::Algorithm) sort!(v,alg=a,order=o)
@deprecate sort!(v::AbstractVector,f::Function) sort!(v,lt=f)
@deprecate sort!(f::Function,v::AbstractVector) sort!(v,lt=f)
@deprecate sort!(v::AbstractVector,a::Algorithm,f::Function) sort!(v,alg=a,lt=f)
@deprecate sort!(v::AbstractVector,f::Function,a::Algorithm) sort!(v,alg=a,lt=f)
@deprecate sort!(f::Function,v::AbstractVector,a::Algorithm) sort!(v,alg=a,lt=f)

@deprecate sortperm(v::AbstractVector,o::Ordering) sortperm(v,order=o)
@deprecate sortperm(v::AbstractVector,a::Algorithm) sortperm(v,alg=a)
@deprecate sortperm(v::AbstractVector,a::Algorithm,o::Ordering) sortperm(v,alg=a,order=o)
@deprecate sortperm(v::AbstractVector,o::Ordering,a::Algorithm) sortperm(v,alg=a,order=o)
@deprecate sortperm(v::AbstractVector,f::Function) sortperm(v,lt=f)
@deprecate sortperm(f::Function,v::AbstractVector) sortperm(v,lt=f)
@deprecate sortperm(v::AbstractVector,a::Algorithm,f::Function) sortperm(v,alg=a,lt=f)
@deprecate sortperm(v::AbstractVector,f::Function,a::Algorithm) sortperm(v,alg=a,lt=f)
@deprecate sortperm(f::Function,v::AbstractVector,a::Algorithm) sortperm(v,alg=a,lt=f)

@deprecate sort(v::AbstractVector,d::Integer,o::Ordering) sort(v,d,order=o)
@deprecate sort(v::AbstractVector,d::Integer,a::Algorithm) sort(v,d,alg=a)
@deprecate sort(v::AbstractVector,d::Integer,a::Algorithm,o::Ordering) sort(v,d,alg=a,order=o)
@deprecate sort(v::AbstractVector,d::Integer,o::Ordering,a::Algorithm) sort(v,d,alg=a,order=o)

@deprecate sort!(v::AbstractVector,d::Integer,o::Ordering) sort!(v,d,order=o)
@deprecate sort!(v::AbstractVector,d::Integer,a::Algorithm) sort!(v,d,alg=a)
@deprecate sort!(v::AbstractVector,d::Integer,a::Algorithm,o::Ordering) sort!(v,d,alg=a,order=o)
@deprecate sort!(v::AbstractVector,d::Integer,o::Ordering,a::Algorithm) sort!(v,d,alg=a,order=o)

@deprecate sortby(v::AbstractVector,f::Function) sort(v,by=f)
@deprecate sortby(f::Function,v::AbstractVector) sort(v,by=f)
@deprecate sortby(v::AbstractVector,a::Algorithm,f::Function) sort(v,alg=a,by=f)
@deprecate sortby(v::AbstractVector,f::Function,a::Algorithm) sort(v,alg=a,by=f)
@deprecate sortby(f::Function,v::AbstractVector,a::Algorithm) sort(v,alg=a,by=f)

@deprecate sortby!(v::AbstractVector,f::Function) sort!(v,by=f)
@deprecate sortby!(f::Function,v::AbstractVector) sort!(v,by=f)
@deprecate sortby!(v::AbstractVector,a::Algorithm,f::Function) sort!(v,alg=a,by=f)
@deprecate sortby!(v::AbstractVector,f::Function,a::Algorithm) sort!(v,alg=a,by=f)
@deprecate sortby!(f::Function,v::AbstractVector,a::Algorithm) sort!(v,alg=a,by=f)

@deprecate sortrows(v::AbstractMatrix,o::Ordering) sortrows(v,order=o)
@deprecate sortrows(v::AbstractMatrix,a::Algorithm) sortrows(v,alg=a)
@deprecate sortrows(v::AbstractMatrix,a::Algorithm,o::Ordering) sortrows(v,alg=a,order=o)
@deprecate sortrows(v::AbstractMatrix,o::Ordering,a::Algorithm) sortrows(v,alg=a,order=o)

@deprecate sortcols(v::AbstractMatrix,o::Ordering) sortcols(v,order=o)
@deprecate sortcols(v::AbstractMatrix,a::Algorithm) sortcols(v,alg=a)
@deprecate sortcols(v::AbstractMatrix,a::Algorithm,o::Ordering) sortcols(v,alg=a,order=o)
@deprecate sortcols(v::AbstractMatrix,o::Ordering,a::Algorithm) sortcols(v,alg=a,order=o)

@deprecate parse(str::String, pos::Int, greedy::Bool, raise::Bool) parse(str,pos,greedy=greedy,raise=raise)
@deprecate parse(str::String, pos::Int, greedy::Bool) parse(str,pos,greedy=greedy)

function amap(f::Function, A::AbstractArray, axis::Integer)
    depwarn("amap is deprecated, use mapslices(f, A, dims) instead", :amap)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]

    if axis_size == 0
        return f(A)
    end

    idx = ntuple(ndimsA, j -> j == axis ? 1 : 1:dimsA[j])
    r = f(sub(A, idx))
    R = Array(typeof(r), axis_size)
    R[1] = r

    for i = 2:axis_size
        idx = ntuple(ndimsA, j -> j == axis ? i : 1:dimsA[j])
        R[i] = f(sub(A, idx))
    end

    return R
end

# Conditional usage of packages and modules
function usingmodule(names::Symbol...)
    depwarn("usingmodule is deprecated, use using instead", :usingmodule)
    eval(current_module(), Expr(:toplevel, Expr(:using, names...)))
end
function usingmodule(names::String)
    depwarn("usingmodule is deprecated, use using instead", :usingmodule)
    usingmodule([symbol(name) for name in split(names,".")]...)
end
export usingmodule

# 0.2 discontinued functions

function addprocs_scyld(np::Integer)
    error("Base.addprocs_scyld is discontinued - add package ClusterManagers and then use ClusterManagers.addprocs_scyld instead.")
end
export addprocs_scyld

function addprocs_sge(np::Integer)
    error("Base.addprocs_sge is discontinued - add package ClusterManagers and then use ClusterManagers.addprocs_sge instead.")
end
export addprocs_sge

function integer_partitions(n,m)
    error("integer_partitions(n,m) has been renamed to partitions(n,m), and is now an iterator.  Please update your code.")
end
export integer_partitions

function mmread(file)
    error("mmread(file) is discontinued - add package MatrixMarket and use MatrixMarket.mmread instead.")
end

function mmread(file, infoonly)
    error("mmread(file, infoonly) is discontinued - add package MatrixMarket and use MatrixMarket.mmread instead.")
end
export mmread

# 0.3 deprecations
@deprecate dense  full

export Stat
const Stat = StatStruct

export CharString
const CharString = UTF32String

@deprecate set_rounding(r::RoundingMode) set_rounding(Float64,r)
@deprecate get_rounding() get_rounding(Float64)
@deprecate with_rounding(f::Function, r::RoundingMode) with_rounding(f::Function, Float64, r)

@deprecate set_bigfloat_rounding(r::RoundingMode) set_rounding(BigFloat,r)
@deprecate get_bigfloat_rounding() get_rounding(BigFloat)
@deprecate with_bigfloat_rounding(f::Function, r::RoundingMode) with_rounding(f::Function, BigFloat, r)
eval(Sys, :(@deprecate shlib_list dllist))
# Sys.shlib_ext is deprecated, renamed to Sys.dlext. Remove alias before release

@deprecate degrees2radians deg2rad
@deprecate radians2degrees rad2deg

@deprecate spzeros(m::Integer) spzeros(m, m)
@deprecate spzeros(Tv::Type, m::Integer) spzeros(Tv, m, m)

@deprecate myindexes localindexes

@deprecate setfield setfield!
@deprecate put      put!
@deprecate take     take!

@deprecate Set(a, b...) Set({a, b...})
# for a bit of backwards compatibility
IntSet(xs::Integer...) = (s=IntSet(); for a in xs; push!(s,a); end; s)
Set{T<:Number}(xs::T...) = Set{T}(xs)

@deprecate normfro(A) vecnorm(A)

# 0.3 discontinued functions

function nnz(X)
    depwarn("nnz has been renamed to countnz and is no longer computed in constant time for sparse matrices. Instead, use nfilled() for the number of elements in a sparse matrix.", :nnz)
    countnz(X)
end
export nnz

scale!{T<:Base.LinAlg.BlasReal}(X::Array{T}, s::Complex) = error("scale!: Cannot scale a real array by a complex value in-place.  Use scale(X::Array{Real}, s::Complex) instead.")
