macro deprecate(old,new)
    if isa(old,Symbol)
        oldname = Expr(:quote,old)
        newname = Expr(:quote,new)
        Expr(:toplevel,
            Expr(:export,esc(old)),
            :(function $(esc(old))(args...)
                  warn_once(string($oldname," is deprecated, use ",$newname," instead."); depth=1)
                  $(esc(new))(args...)
              end))
    elseif isa(old,Expr) && old.head == :call
        oldcall = sprint(io->show_unquoted(io,old))
        newcall = sprint(io->show_unquoted(io,new))
        Expr(:toplevel,
            Expr(:export,esc(old.args[1])),
            :($(esc(old)) = begin
                  warn_once(string($oldcall," is deprecated, use ",$newcall," instead."); depth=1)
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

# 0.1

@deprecate  push            push!
@deprecate  pop             pop!
@deprecate  grow            grow!
@deprecate  enqueue         unshift!
@deprecate  unshift         unshift!
@deprecate  shift           shift!
@deprecate  insert          insert!
@deprecate  del             delete!
@deprecate  del_all         empty!
@deprecate  add_each        add_each!
@deprecate  del_each        del_each!
@deprecate  toggle          symdiff!
@deprecate  toggle_each     symdiff!
@deprecate  xor!            symdiff!
@deprecate  load            require
@deprecate  idump           xdump
@deprecate  cwd             pwd
@deprecate  strlen          length
@deprecate  strchr          search
@deprecate  memchr          search
@deprecate  lc              lowercase
@deprecate  uc              uppercase
@deprecate  nCr             binomial
@deprecate  julia_pkgdir    Pkg.dir
@deprecate  tintersect      typeintersect
@deprecate  choose          first
@deprecate  system          run
@deprecate  order           sortperm
@deprecate  numel           length
@deprecate  islogical       isbool
@deprecate  csvread         readcsv
@deprecate  dlmread         readdlm
@deprecate  csvwrite        writecsv
@deprecate  dlmwrite        writedlm
@deprecate  chi2rnd         randchi2
@deprecate  betarnd         randbeta
@deprecate  exprnd          randexp
@deprecate  rot90           rotl90
@deprecate  chars           collect
@deprecate  elements        collect
@deprecate  pairs           collect
@deprecate  strcat          string
@deprecate  iswalnum        isalnum
@deprecate  iswalpha        isalpha
@deprecate  iswascii        isascii
@deprecate  iswblank        isblank
@deprecate  iswcntrl        iscntrl
@deprecate  iswdigit        isdigit
@deprecate  iswgraph        isgraph
@deprecate  iswlower        islower
@deprecate  iswprint        isprint
@deprecate  iswpunct        ispunct
@deprecate  iswspace        isspace
@deprecate  iswupper        isupper
@deprecate  iswxdigit       isxdigit
@deprecate  copy_to         copy!
@deprecate  countp          count
@deprecate  anyp            any
@deprecate  allp            all
@deprecate  resize          sizehint
@deprecate  permute         permutedims
@deprecate  ipermute        ipermutedims
@deprecate  is_hex_digit    isxdigit
@deprecate  read_from       readsfrom
@deprecate  write_to        writesto
@deprecate  download_file   download
@deprecate  histc           hist
@deprecate  map_to          map!
@deprecate  rotl            rol
@deprecate  rotr            ror
@deprecate  flipbits        (~)
@deprecate  cor_pearson     cor
@deprecate  cov_pearson     cov
@deprecate  areduce         reducedim
@deprecate  tmpnam          tempname
@deprecate  lud             lufact
@deprecate  chold           cholfact
@deprecate  cholpd          cholpfact
@deprecate  qrd             qrfact
@deprecate  qrpd            qrpfact
@deprecate  key             getkey

@deprecate  grow!(a,d)              resize!(a,length(a)+d)
@deprecate  keytype(a)              eltype(a)[1]
@deprecate  valtype(a)              eltype(a)[2]
@deprecate  randi(n,x...)           rand(1:n,x...)
@deprecate  randival(lo,hi,x...)    rand(lo:hi,x...)
@deprecate  squeeze(A)              squeeze(A,find([size(A)...].==1))
@deprecate  getenv(var)             ENV[var]
@deprecate  hasenv(var)             has(ENV,var)
@deprecate  setenv(var::ByteString,val::ByteString)         ENV[var] = val
@deprecate  unsetenv(var)           delete!(ENV,var)

function svd(a::StridedMatrix, vecs::Bool, thin::Bool)
    warn_once("The second argument ``vecs`` is no longer supported. Use svd(a, thin) instead."; depth=1)
    svd(a, thin)
end

function svdt(a::StridedMatrix, vecs::Bool, thin::Bool)
    warn_once("The second argument ``vecs`` is no longer supported. Use svdt(a, thin) instead."; depth=1)
    svdt(a, thin)
end

# discontinued functions

export randexp, randg, randbeta, randchi
for (fun,typ) in {(:randexp,:Exponential), (:randg,:Gamma), (:randbeta,:Beta), (:randchi,:Chisq)}
@eval $fun(x...) = error($fun," is no longer supported, use the Distributions package instead:

    using Distributions
    rand(",$(Expr(:quote,typ)),"())
")
end

const IOString = IOBuffer
export IOString
const PipeString = PipeBuffer
export PipeString

# @spawnlocal discontinued

# 0.2

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
@deprecate  float64_valued      isfloat64
@deprecate  isdenormal          issubnormal
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
@deprecate openblas_set_num_threads      blas_set_num_threads
@deprecate check_openblas                check_blas
@deprecate msync(A::Array, flags::Int)    msync(A)
@deprecate msync(A::BitArray, flags::Int) msync(A)
@deprecate square(x::Number)          x*x
@deprecate finfer                     code_typed
@deprecate disassemble(f::Function,t::Tuple)           code_llvm(f,t)
@deprecate disassemble(f::Function,t::Tuple,asm::Bool) (asm ? code_native(f,t) : code_llvm(f,t))
@deprecate  add(s::Set, x)                  push!(s,x)
@deprecate  add!(s::Set, x)                 push!(s,x)
@deprecate  delete!(d::Dict, key, default)  pop!(d, key, default)

deprecated_ls() = run(`ls -l`)
deprecated_ls(args::Cmd) = run(`ls -l $args`)
deprecated_ls(args::String...) = run(`ls -l $args`)
function ls(args...)
    warn_once("ls() is deprecated, use readdir() instead. If you are at the repl prompt, consider `;ls`.")
    deprecated_ls(args...)
end
function start_timer(timer::TimeoutAsyncWork, timeout::Int, repeat::Int)
    warn_once("start_timer now expects arguments in units of seconds. you may need to update your code")
    invoke(start_timer, (TimeoutAsyncWork,Real,Real), timer, timeout, repeat)
end

# redirection operators
@deprecate |(a::AbstractCmd,b::AbstractCmd) (a|>b)
@deprecate >(a::Redirectable,b::AbstractCmd) (a|>b)
@deprecate >(a::String,b::AbstractCmd) (a|>b)
@deprecate >(a::AbstractCmd,b::Redirectable) (a|>b)
@deprecate >(a::AbstractCmd,b::String) (a|>b)
@deprecate <(a::AbstractCmd,b::String) (b|>a)
@deprecate |(x, f::Function) (x|>f)

@deprecate memio(args...)  IOBuffer()

# note removed macros: str, B_str, I_str, E_str, L_str, L_mstr, I_mstr, E_mstr

const ref = getindex
export ref
const assign = setindex!
export assign

# will be removed from exports (moved into Base.Sys): OS_NAME, WORD_SIZE, CPU_CORES

typealias ComplexPair Complex
export ComplexPair

# superseded sorting API

@deprecate select(v::AbstractVector,k::Union(Int,Range1),o::Ordering) select(v,k,order=o)
@deprecate select(v::AbstractVector,k::Union(Int,Range1),f::Function) select(v,k,lt=f)
@deprecate select(f::Function,v::AbstractVector,k::Union(Int,Range1)) select(v,k,lt=f)

# @deprecate select!(v::AbstractVector,k::Union(Int,Range1),o::Ordering) select!(v,k,order=o)
@deprecate select!(v::AbstractVector,k::Union(Int,Range1),f::Function) select!(v,k,lt=f)
@deprecate select!(f::Function,v::AbstractVector,k::k::Union(Int,Range1)) select!(v,k,lt=f)

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

function amap(f::Function, A::AbstractArray, axis::Integer)
    warn_once("amap is deprecated, use mapslices(f, A, dims) instead")
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
    warn_once("usingmodule is deprecated, use using instead")
    eval(current_module(), Expr(:toplevel, Expr(:using, names...)))
end
function usingmodule(names::String)
    warn_once("usingmodule is deprecated, use using instead")
    usingmodule([symbol(name) for name in split(names,".")]...)
end
export usingmodule

# discontinued functions

function addprocs_scyld(np::Integer)
    error("Base.addprocs_scyld is discontinued - add package ClusterManagers and then use ClusterManagers.addprocs_scyld instead.")
end
export addprocs_scyld

function addprocs_sge(np::Integer)
    error("Base.addprocs_sge is discontinued - add package ClusterManagers and then use ClusterManagers.addprocs_sge instead.")
end
export addprocs_sge
