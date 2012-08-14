module Base

export
    # Modules
    Base,Grisu,Printf,PCRE,FFTW,FFT,
    # Types
    AbstractMatrix,AbstractVector,Array,Associative,CharString,Chars,Cmd,Cmds,
    Colon,Complex,Complex128,Complex64,ComplexPair,DArray,Dict,Dims,EachLine,
    EachSearch,Enumerate,EnvHash,Executable,FDSet,FileDes,FileOffset,Filter,
    GORef,GenericString,GlobalObject,IO,IOStream,IOTally,ImaginaryUnit,Indices,
    IntSet,LocalProcess,Location,Matrix,ObjectIdDict,Pipe,PipeEnd,PipeIn,
    PipeOut,Port,Ports,ProcessExited,ProcessGroup,ProcessNotRun,ProcessRunning,
    ProcessSignaled,ProcessStatus,ProcessStopped,Range,Range1,RangeIndex,Ranges,
    Rational,Regex,RegexMatch,RegexMatchIterator,Region,RemoteRef,RepString,
    RevString,Reverse,RopeString,Set,StridedArray,StridedMatrix,StridedVecOrMat,
    StridedVector,SubArray,SubDArray,SubOrDArray,SubString,TransformedString,
    VecOrMat,Vector,VersionNumber,WeakKeyDict,Zip,
    Stat, Factorization, Cholesky, LU, QR, QRP,
    # Exceptions
    ArgumentError,BackTrace,DisconnectException,ErrorException,KeyError,
    LoadError,MethodError,ParseError,SystemError,TypeError,
    # Global constants and variables
    ARGS,C_NULL,CPU_CORES,CURRENT_OS,ENDIAN_BOM,ENV,Inf,Inf32,LOAD_PATH,
    MS_ASYNC,MS_INVALIDATE,MS_SYNC,NaN,NaN32,OUTPUT_STREAM,RANDOM_SEED,STDERR,
    STDIN,STDOUT,VERSION,WORD_SIZE,Scheduler,e,im,pi,
    # Unix error codes
    E2BIG,EACCES,EADDRINUSE,EADDRNOTAVAIL,EADV,EAFNOSUPPORT,EAGAIN,EALREADY,
    EBADE,EBADF,EBADFD,EBADMSG,EBADR,EBADRQC,EBADSLT,EBFONT,EBUSY,ECANCELED,
    ECHILD,ECHRNG,ECOMM,ECONNABORTED,ECONNREFUSED,ECONNRESET,EDEADLK,
    EDESTADDRREQ,EDOM,EDOTDOT,EDQUOT,EEXIST,EFAULT,EFBIG,EHOSTDOWN,EHOSTUNREACH,
    EHWPOISON,EIDRM,EILSEQ,EINPROGRESS,EINTR,EINVAL,EIO,EISCONN,EISDIR,EISNAM,
    EKEYEXPIRED,EKEYREJECTED,EKEYREVOKED,EL2HLT,EL2NSYNC,EL3HLT,EL3RST,ELIBACC,
    ELIBBAD,ELIBEXEC,ELIBMAX,ELIBSCN,ELNRNG,ELOOP,EMEDIUMTYPE,EMFILE,EMLINK,
    EMSGSIZE,EMULTIHOP,ENAMETOOLONG,ENAVAIL,ENETDOWN,ENETRESET,ENETUNREACH,
    ENFILE,ENOANO,ENOBUFS,ENOCSI,ENODATA,ENODEV,ENOENT,ENOEXEC,ENOKEY,ENOLCK,
    ENOLINK,ENOMEDIUM,ENOMEM,ENOMSG,ENONET,ENOPKG,ENOPROTOOPT,ENOSPC,ENOSR,
    ENOSTR,ENOSYS,ENOTBLK,ENOTCONN,ENOTDIR,ENOTEMPTY,ENOTNAM,ENOTRECOVERABLE,
    ENOTSOCK,ENOTTY,ENOTUNIQ,ENXIO,EOPNOTSUPP,EOVERFLOW,EOWNERDEAD,EPERM,
    EPFNOSUPPORT,EPIPE,EPROTO,EPROTONOSUPPORT,EPROTOTYPE,ERANGE,EREMCHG,EREMOTE,
    EREMOTEIO,ERESTART,ERFKILL,EROFS,ESHUTDOWN,ESOCKTNOSUPPORT,ESPIPE,ESRCH,
    ESRMNT,ESTALE,ESTRPIPE,ETIME,ETIMEDOUT,ETOOMANYREFS,ETXTBSY,EUCLEAN,EUNATCH,
    EUSERS,EXDEV,EXFULL,
    # Operators
    !,!=,$,%,&,*,+,-,.!=,.*,./,.<,.<=,.==,.>,.>=,.\,.^,/,//,:,<,<:,<<,<=,==,
    >,>=,>>,>>>,\,^,|,~,
    A_ldiv_Bc,A_ldiv_Bt,A_mul_B,A_mul_Bc,A_mul_Bt,A_rdiv_Bc,A_rdiv_Bt,Ac_ldiv_B,
    Ac_ldiv_Bc,Ac_mul_B,Ac_mul_Bc,Ac_rdiv_B,Ac_rdiv_Bc,At_ldiv_B,At_ldiv_Bt,
    At_mul_B,At_mul_Bt,At_rdiv_B,At_rdiv_Bt,
    # scalar math
    abs,abs2,acos,acosd,acosh,acot,acotd,acoth,acsc,acscd,acsch,angle,
    asec,asecd,asech,asin,asind,asinh,atan,atan2,atand,atanh,bitmix,bool,
    binomial,bswap,cbrt,ceil,cis,clamp,cmp,combinations,complex,complex128,
    complex64,conj,copysign,cos,cosc,cosd,cosh,cot,cotd,coth,
    count_ones,count_zeros,csc,cscd,csch,degrees2radians,den,div,eps,
    erf,erfc,exp,exp2,expm1,factor,factorial,fld,flipsign,float,float32,
    float64,float64_valued,floor,fpart,frexp,gamma,gcd,gcdx,hex2num,hypot,
    iceil,ifloor,ilogb,imag,inf,int,int128,int16,int32,int64,int8,
    integer,integer_partitions,integer_valued,inv,invmod,ipart,iround,
    isbool,iscomplex,isdenormal,iseven,isfinite,isinf,isinteger,islogical,isnan,
    isodd,ispow2,isprime,isreal,itrunc,lcm,ldexp,leading_ones,leading_zeros,
    lfact,lgamma,log,log10,log1p,log2,logb,mod,mod1,modf,nCr,nPr,nan,
    nextfloat,nextpow2,num,num2hex,one,pow,power_by_squaring,powermod,
    prevfloat,radians2degrees,rational,real,real_valued,realmax,realmin,
    reim,reinterpret,rem,round,sec,secd,sech,sign,signbit,signed,significand,
    sin,sinc,sind,sinh,sqrt,square,tan,tand,tanh,trailing_ones,trailing_zeros,
    trunc,uint,uint128,uint16,uint32,uint64,uint8,unsigned,zero,nextprod,
    prevprod,isinteger,typemax,typemin,
    # arrays and linear algebra
    amap,and!,areduce,axpy,bsxfun,cartesian_map,cat,cell,cell_1d,cell_2d,
    chol,chol!,circshift,colon,cond,conj!,copy_to,cross,ctranspose,
    cumprod,cumsum,det,diag,diagm,diagmm,diagmm!,diff,dot,each_col,
    each_col!,each_row,each_row!,each_vec,each_vec!,eig,eye,falses,
    fill,fill!,filter,filter!,find,findmax,findmin,findn,findn_nzs,
    first,flipdim,fliplr,flipud,full,gen_cartesian_map,gradient,grow,
    hcat,hvcat,ind2sub,insert,invperm,ipermute,ishermitian,isperm,issorted,
    issym,issym_rnd,istril,istriu,kron,last,linreg,linspace,logspace,
    lu,lu!,matmul2x2,matmul3x3,max,maxintfloat,min,ndims,nnz,nonzeros,norm,not!,
    nthperm,nthperm!,ones,or!,order,partitions,pascal,permute,pop,prod,
    promote_shape,push,qr,randcycle,randperm,randsym,rank,repmat,
    reshape,reverse,reverse!,rot180,rot90,rotl90,rotr90,rref,search_sorted,
    select,select!,shift,shuffle,shuffle!,size,slice,slicedim,sort,sort!,
    sort_by,sort_by!,sortperm,sortr,sortr!,squeeze,step,stride,strides,
    sub,sub2ind,sum,svd,svdvals,trace,transpose,trideig,tril,triu,trues,
    unshift,vcat,vec,append!,xor!,zeros,findfirst,qrp,sdd,ref_shape,
    assign_shape_check, to_index, indices, append_any, make_loop_nest,
    trailingsize, check_bounds, search_sorted_last,search_sorted_first,
    # collections
    add,add_each,add_weak_key,add_weak_value,all,allp,any,anyp,
    assign,choose,complement,complement!,contains,contains_is,count,countp,
    del,del_all,del_each,dict,elements,eltype,enqueue,get,has,hash,
    inter,intersect,intersection,intersection!,intset,isempty,key,keys,length,
    map,map_to,map_to2,mapreduce,merge,merge!,numel,pairs,reduce,ref,rehash,
    scan,similar,toggle,toggle_each,union,union!,values,
    # strings and text output
    ascii,begins_with,byte_string_classify,char,chars,charwidth,
    check_ascii,check_utf8,chomp,chop,chr2ind,bytestring,each_match,
    each_search,ends_with,escape_string,first_utf8_byte,ind2chr,
    is_utf8_start,is_valid_ascii,is_valid_utf8,isvalid,
    iswalnum,iswalpha,iswascii,iswblank,iswcntrl,iswdigit,iswgraph,iswlower,
    iswprint,iswpunct,iswspace,iswupper,iswxdigit,join,lc,lcfirst,
    lowercase,lpad,lstrip,match,ismatch,memcat,memchr,nextind,prevind,
    replace,rpad,rstrip,safe_char,search,split,strcat,strchr,string,
    strip,strlen,strwidth,thisind,transform_to_utf8,uc,ucfirst,
    uppercase,utf8,randstring,
    bin,bits,dec,dump,float32_isvalid,float64_isvalid,fprintf,hex,idump,
    is_hex_digit,ndigits,ndigits0z,oct,parse_bin,parse_float,parse_hex,
    parse_int,parse_oct,print,print_escaped,print_joined,
    print_matrix,print_quoted,print_quoted_literal,print_shortest,
    print_unescaped,print_unescaped_chars,printf,println,quote_string,
    repeat,repl_show,show,showall,showcompact,sprint,sprintf,repr,summary,
    unescape_chars,unescape_string,base,
    # statistics and random numbers
    betarnd,chi2rnd,cor,cor_pearson,cor_spearman,cov,cov_pearson,cov_spearman,
    decile,exprnd,hist,histc,mad,mean,median,quantile,quartile,quintile,
    rand,rand!,randbeta,randbeta!,randbit,randbit!,randbool,
    randbool!,randchi2,randchi2!,randexp,randexp!,randg,randg!,randg2,
    randi,randi!,randival,randival!,randn,randn!,srand,std,tiedrank,var,
    weighted_mean,
    # signal processing
    bfft,bfftn,brfft,brfftn,conv,conv2,deconv,fft,fft2,fft3,
    fftn,fftshift,filt,ifft,ifft2,ifft3,ifftn,ifftshift,irfft,irfftn,
    rfft,rfftn,xcorr,

    # iteration
    start,done,next,enumerate,zip,
    # object identity and equality
    copy,isequal,isless,identity,object_id,sizeof,isimmutable,
    # tasks
    consume,current_task,istaskdone,produce,tls,
    # time
    sleep,strftime,strptime,tic,time,time_ns,toc,toq,
    # errors
    assert,error,system_error,
    # types
    convert,isleaftype,oftype,promote,promote_rule,promote_type,super,
    tintersect,
    # syntax
    expand,esc,expr,gensym,parse,parse_input_line,parseatom,symbol,
    # help and reflection
    ans,apropos,function_loc,edit,methods,help,less,names,which,whicht,whos,
    # loading source files
    evalfile,find_in_path,include_string,load,require,
    # RTS internals
    compile_hint,finalizer,finfer,gc,gc_disable,gc_enable,isbuiltin,isconst,
    isgeneric,
    # misc
    exit,quit,method_missing,ntuple,peakflops,times,tty_cols,tty_rows,

    # I/O and events
    add_fd_handler,close,countlines,csvread,csvwrite,del_fd_handler,
    deserialize,dlmread,dlmwrite,each_line,eatwspace,eatwspace_comment,
    eof,fd,fdio,flush,gethostname,getipaddr,htol,hton,ltoh,ntoh,memio,
    mmap,mmap_array,mmap_grow,mmap_stream_settings,msync,munmap,
    nb_available,open,position,read,readall,readchomp,readline,readlines,
    readuntil,seek,select_read,serialize,skip,stderr,stderr_stream,stdin,
    stdin_stream,stdout,stdout_stream,takebuf_array,takebuf_string,truncate,
    with_output_to_string,write,
    # multiprocessing
    addprocs_local,addprocs_sge,addprocs_ssh,at_each,broadcast,fetch,
    is_go_member,isready,make_scheduled,yield,enq_work,myid,nprocs,
    pfor,pmap,preduce,put,remote_call,remote_call_fetch,remote_call_wait,
    remote_do,rr2id,spawn,spawnat,spawnlocal,take,
    # distributed arrays
    changedist,darray,dcell,defaultdist,dfill,dist,distdim,distribute,dones,
    drand,drandn,dzeros,localize,localize_copy,locate,map_vectorized,maxdim,
    myindexes,owner,pieceindex,pieceindexes,procs,
    # paths and file names
    basename,cd,cwd,fullfile,is_file_readable,ls,tmpnam,basename,dirname,
    dirname_basename, file_path, path_expand, file_copy,
    file_remove, file_create, path_rename, dir_create, dir_remove,
    file_exists, tempdir, tempfile, download_file, real_path,
    abs_path, abs_path_split, filemode, fileparts, filesize, fullfile,
    isrooted, split_extension, 
    split_path, tilde_expand, mtime, ctime, stat, lstat, isfifo, ispath,
    ischardev, isdir, isblockdev, isfile, islink, issocket, issetuid,
    issetgid, issticky, isreadable, iswriteable, isexecutable, uperm,
    gperm, operm, 
    # external processes
    cmd_stdin_stream,cmd_stdout_stream,cmds,connect,dup2,exec,fork,getpid,
    ignorestatus,in,make_pipe,other,out,output,pipeline_error,
    process_exit_status,process_exited,process_options,process_running,
    process_signaled,process_status,process_stop_signal,process_stopped,
    process_term_signal,read_from,run,setsuccess,success,successful,system,
    wait,wait_nohang,write_to,
    # C interface
    c_free,dlopen,dlsym,errno,getenv,hasenv,pointer,pointer_to_array,
    ptr_arg_convert,setenv,strerror,unsetenv,
    # Macros
    @v_str, @unexpected, @assert, @r_str, @str, @S_str, @I_str, @E_str,
    @B_str, @b_str, @cmd, @time, @elapsed, @windows_only, @unix_only,
    @sync, @spawn, @spawnlocal, @spawnat, @everywhere, @parallel,
    @gensym, @eval, @task, @thunk, @L_str, @vectorize_1arg,
    @vectorize_2arg, @printf, @sprintf, @memoize

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(x) = print(stdout_stream, x)
    show(x) = show(stdout_stream, x)
    write(io, a::Array{Uint8,1}) =
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              io.ios, a, length(a))
    print(io, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void},Any,),
                                 io.ios, s)
    print(io, s::ASCIIString) = (write(io, s.data);nothing)
    print(io, x) = show(io, x)
    println(io, x) = (print(io, x); print(io, "\n"))
    show(io, x) = ccall(:jl_show_any, Void, (Any, Any,), io, x)
    show(io, s::ASCIIString) = (write(io, s.data);nothing)
    show(io, s::Symbol) = print(io, s)
    show(io, b::Bool) = print(io, b ? "true" : "false")
    show(io, n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), io, n)
    show(io, n::Integer)  = show(io, int64(n))
    print(io, a...) = for x=a; print(io, x); end
    function show(io, e::Expr)
        print(io, e.head)
        print(io, "(")
        for i=1:arraylen(e.args)
            show(io, arrayref(e.args,i))
            print(io, ", ")
        end
        print(io, ")\n")
    end
end

## Load essential files and libraries

include("base.jl")

# core operations & types
include("range.jl")
include("tuple.jl")
include("cell.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("promotion.jl")
include("operators.jl")
include("pointer.jl")

_jl_lib = ccall(:jl_load_dynamic_library,Ptr{Void},(Ptr{None},),C_NULL)
_jl_libfdm = dlopen("libfdm")

include("float.jl")
include("reduce.jl")
include("complex.jl")
include("rational.jl")

# core data structures (used by type inference)
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("intset.jl")
include("dict.jl")
include("set.jl")

# compiler
include("inference.jl")

# I/O, strings & printing
include("io.jl")
include("char.jl")
include("ascii.jl")
include("utf8.jl")
include("string.jl")
include("regex.jl")
include("show.jl")
include("grisu.jl")
include("printf.jl")

# concurrency and parallelism
include("iterator.jl")
include("task.jl")
include("process.jl")
include("serialize.jl")
include("multi.jl")

# system & environment
include("osutils.jl")
include("libc.jl")
include("env.jl")
include("errno_h.jl")
include("file.jl")
include("stat.jl")

# front end
include("client.jl")

# core math functions
include("intfuncs.jl")
include("floatfuncs.jl")
include("math.jl")
include("math_libm.jl")
include("sort.jl")
include("combinatorics.jl")
include("statistics.jl")

# random number generation
include("random.jl")

# distributed arrays and memory-mapped arrays
include("darray.jl")
include("mmap.jl")

# utilities - version, timing, help, edit
include("version.jl")
include("util.jl")
include("datafmt.jl")

## Load optional external libraries

include("build_h.jl")

# linear algebra
include("linalg.jl")
include("linalg_dense.jl")
include("linalg_blas.jl")
include("linalg_lapack.jl")
include("factorizations.jl")

# signal processing
include("signal_fftw.jl")
include("signal.jl")
import Base.FFT
import Base.FFT.*

# prime method cache with some things we know we'll need right after startup
compile_hint(cwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
compile_hint(select_read, (FDSet, Float64))
compile_hint(next, (Dict{Any,Any}, Int))
compile_hint(start, (Dict{Any,Any},))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(isempty, (Array{WorkItem,1},))
compile_hint(ref, (Dict{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(_start, ())
compile_hint(process_options, (Array{Any,1},))
compile_hint(run_repl, ())
compile_hint(anyp, (Function, Array{Any,1}))
compile_hint(Dict, (Int,))
compile_hint(Dict{Any,Any}, (Int,))
compile_hint(Set, ())
compile_hint(assign, (Dict{Any,Any}, Bool, Cmd))
compile_hint(rehash, (Dict{Any,Any}, Int))
compile_hint(run, (Cmd,))
compile_hint(spawn, (Cmd,))
compile_hint(assign, (Dict{Any,Any}, Bool, FileDes))
compile_hint(wait, (Int32,))
compile_hint(system_error, (ASCIIString, Bool))
compile_hint(SystemError, (ASCIIString,))
compile_hint(has, (EnvHash, ASCIIString))
compile_hint(parse_input_line, (ASCIIString,))
compile_hint(cmp, (Int32, Int32))
compile_hint(min, (Int32, Int32))
compile_hint(==, (ASCIIString, ASCIIString))
compile_hint(arg_gen, (ASCIIString,))
compile_hint(_jl_librandom_init, ())
compile_hint(srand, (ASCIIString, Int))
compile_hint(open, (ASCIIString, Bool, Bool, Bool, Bool))
compile_hint(srand, (Uint64,))
compile_hint(done, (IntSet, Int64))
compile_hint(next, (IntSet, Int64))
compile_hint(ht_keyindex, (Dict{Any,Any}, Int32))
compile_hint(perform_work, (WorkItem,))
compile_hint(notify_done, (WorkItem,))
compile_hint(work_result, (WorkItem,))
compile_hint(del_fd_handler, (Int32,))
compile_hint(enqueue, (Array{WorkItem,1}, WorkItem))
compile_hint(enq_work, (WorkItem,))
compile_hint(pop, (Array{WorkItem,1},))
compile_hint(string, (Int,))
compile_hint(parse_int, (Type{Int}, ASCIIString, Int))
compile_hint(repeat, (ASCIIString, Int))
compile_hint(KeyError, (Int,))
compile_hint(show, (Float64,))
compile_hint(match, (Regex, ASCIIString))
compile_hint(strlen, (ASCIIString,))
compile_hint(dims2string, (Tuple,))
compile_hint(alignment, (Float64,))
compile_hint(repl_callback, (Expr, Int))
compile_hint(istaskdone, (Task,))
compile_hint(make_stdout_stream, ())
compile_hint(make_stdin_stream, ())
compile_hint(make_stderr_stream, ())
compile_hint(int, (Uint64,))
compile_hint(copy, (Bool,))
compile_hint(bool, (Bool,))
compile_hint(bool, (RemoteRef,))
compile_hint(wait, (RemoteRef,))
compile_hint(hash, (RemoteRef,))
compile_hint(take, (RemoteRef,))
compile_hint(bitmix, (Int, Int))
compile_hint(bitmix, (Uint, Int))
compile_hint(bitmix, (Uint64, Int64))
compile_hint(hash, (Int,))
compile_hint(isequal, (Symbol, Symbol))
compile_hint(isequal, (Bool, Bool))
compile_hint(WaitFor, (Symbol, RemoteRef))
compile_hint(_jl_answer_color, ())
compile_hint(get, (EnvHash, ASCIIString, ASCIIString))
compile_hint(notify_empty, (WorkItem,))
compile_hint(rr2id, (RemoteRef,))
compile_hint(isequal, (RemoteRef, WeakRef))
compile_hint(isequal, (RemoteRef, RemoteRef))
compile_hint(_ieval, (Symbol,))
compile_hint(static_convert, (Any, Any))
compile_hint(assign, (Array{Any,1}, WeakRef, Int))
compile_hint(hash, (Tuple,))
compile_hint(assign, (Dict{Any,Any}, WorkItem, (Int,Int)))
compile_hint(isequal, ((Int,Int),(Int,Int)))
compile_hint(RemoteRef, (Int, Int, Int))
compile_hint(inlining_pass, (LambdaStaticData, Array{Any,1}))
compile_hint(_jl_eval_user_input, (Expr, Bool))
compile_hint(print, (Float64,))
compile_hint(a2t, (Array{Any,1},))
compile_hint(flush, (IOStream,))
compile_hint(ref, (Type{String}, ASCIIString, ASCIIString, ASCIIString))
compile_hint(int, (Int,))
compile_hint(uint, (Uint,))
compile_hint(_atexit, ())
compile_hint(read, (IOStream, Array{Uint32,1}))
compile_hint(copy, (Type,))
compile_hint(hex, (Char, Int))
compile_hint(abs, (Char,))
compile_hint(abstract_eval, (LambdaStaticData, ObjectIdDict, StaticVarInfo))
compile_hint(length, (Range1{Int},))
compile_hint(start, (Range1{Int},))
compile_hint(done, (Range1{Int},Int))
compile_hint(next, (Range1{Int},Int))
compile_hint(IOStream, (ASCIIString, Array{Uint8,1}))
compile_hint(_jl_mk_tupleref, (SymbolNode, Int))
compile_hint(_jl_abstract_interpret, (Bool, ObjectIdDict, StaticVarInfo))
compile_hint(eval_annotate, (LambdaStaticData, ObjectIdDict, StaticVarInfo, ObjectIdDict, Array{Any,1}))

# invoke type inference, running the existing inference code on the new
# inference code to cache an optimized version of it.
begin
    local atypes = (LambdaStaticData, Tuple, (), LambdaStaticData, Bool)
    local minf = methods(typeinf, atypes)
    typeinf_ext(minf[1][3], atypes, (), minf[1][3])
end

end # module Base

import Base.*

# create system image file
ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/../lib/julia/sys.ji", "start_image.jl")
