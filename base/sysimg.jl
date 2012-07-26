## Base module exports

export
    # Module
    Base,
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
    # PCRE constants
    #PCRE_VERSION,PCRE_COMPILE_MASK,PCRE_EXECUTE_MASK,PCRE_OPTIONS_MASK,
    PCRE_ANCHORED,PCRE_AUTO_CALLOUT,PCRE_BSR_ANYCRLF,PCRE_BSR_UNICODE,
    PCRE_CASELESS,PCRE_CONFIG_BSR,PCRE_CONFIG_JIT,PCRE_CONFIG_JITTARGET,
    PCRE_CONFIG_LINK_SIZE,PCRE_CONFIG_MATCH_LIMIT,
    PCRE_CONFIG_MATCH_LIMIT_RECURSION,PCRE_CONFIG_NEWLINE,
    PCRE_CONFIG_POSIX_MALLOC_THRESHOLD,PCRE_CONFIG_STACKRECURSE,
    PCRE_CONFIG_UNICODE_PROPERTIES,PCRE_CONFIG_UTF16,
    PCRE_CONFIG_UTF8,PCRE_DFA_RESTART,PCRE_DFA_SHORTEST,PCRE_DOLLAR_ENDONLY,
    PCRE_DOTALL,PCRE_DUPNAMES,PCRE_ERROR_BADCOUNT,PCRE_ERROR_BADENDIANNESS,
    PCRE_ERROR_BADMAGIC,PCRE_ERROR_BADMODE,PCRE_ERROR_BADNEWLINE,
    PCRE_ERROR_BADOFFSET,PCRE_ERROR_BADOPTION,PCRE_ERROR_BADPARTIAL,
    PCRE_ERROR_BADUTF16,PCRE_ERROR_BADUTF16_OFFSET,PCRE_ERROR_BADUTF8,
    PCRE_ERROR_BADUTF8_OFFSET,PCRE_ERROR_CALLOUT,PCRE_ERROR_DFA_RECURSE,
    PCRE_ERROR_DFA_UCOND,PCRE_ERROR_DFA_UITEM,PCRE_ERROR_DFA_UMLIMIT,
    PCRE_ERROR_DFA_WSSIZE,PCRE_ERROR_INTERNAL,PCRE_ERROR_JIT_STACKLIMIT,
    PCRE_ERROR_MATCHLIMIT,PCRE_ERROR_NOMATCH,PCRE_ERROR_NOMEMORY,
    PCRE_ERROR_NOSUBSTRING,PCRE_ERROR_NULL,PCRE_ERROR_NULLWSLIMIT,
    PCRE_ERROR_PARTIAL,PCRE_ERROR_RECURSELOOP,PCRE_ERROR_RECURSIONLIMIT,
    PCRE_ERROR_SHORTUTF16,PCRE_ERROR_SHORTUTF8,PCRE_ERROR_UNKNOWN_NODE,
    PCRE_ERROR_UNKNOWN_OPCODE,PCRE_EXTENDED,PCRE_EXTRA,
    PCRE_EXTRA_CALLOUT_DATA,PCRE_EXTRA_EXECUTABLE_JIT,PCRE_EXTRA_MARK,
    PCRE_EXTRA_MATCH_LIMIT,PCRE_EXTRA_MATCH_LIMIT_RECURSION,
    PCRE_EXTRA_STUDY_DATA,PCRE_EXTRA_TABLES,PCRE_FIRSTLINE,PCRE_INFO_BACKREFMAX,
    PCRE_INFO_CAPTURECOUNT,PCRE_INFO_DEFAULT_TABLES,PCRE_INFO_FIRSTBYTE,
    PCRE_INFO_FIRSTCHAR,PCRE_INFO_FIRSTTABLE,PCRE_INFO_HASCRORLF,
    PCRE_INFO_JCHANGED,PCRE_INFO_JIT,PCRE_INFO_JITSIZE,PCRE_INFO_LASTLITERAL,
    PCRE_INFO_MINLENGTH,PCRE_INFO_NAMECOUNT,PCRE_INFO_NAMEENTRYSIZE,
    PCRE_INFO_NAMETABLE,PCRE_INFO_OKPARTIAL,PCRE_INFO_OPTIONS,PCRE_INFO_SIZE,
    PCRE_INFO_STUDYSIZE,PCRE_JAVASCRIPT_COMPAT,PCRE_MAJOR,PCRE_MINOR,
    PCRE_MULTILINE,PCRE_NEWLINE_ANY,PCRE_NEWLINE_ANYCRLF,PCRE_NEWLINE_CR,
    PCRE_NEWLINE_CRLF,PCRE_NEWLINE_LF,PCRE_NOTBOL,PCRE_NOTEMPTY,
    PCRE_NOTEMPTY_ATSTART,PCRE_NOTEOL,PCRE_NO_AUTO_CAPTURE,
    PCRE_NO_START_OPTIMISE,PCRE_NO_START_OPTIMIZE,PCRE_NO_UTF16_CHECK,
    PCRE_NO_UTF8_CHECK,PCRE_PARTIAL,PCRE_PARTIAL_HARD,
    PCRE_PARTIAL_SOFT,PCRE_STUDY_JIT_COMPILE,PCRE_UCP,PCRE_UNGREEDY,PCRE_UTF16,
    PCRE_UTF16_ERR0,PCRE_UTF16_ERR1,PCRE_UTF16_ERR2,PCRE_UTF16_ERR3,
    PCRE_UTF16_ERR4,PCRE_UTF8,PCRE_UTF8_ERR0,PCRE_UTF8_ERR1,PCRE_UTF8_ERR10,
    PCRE_UTF8_ERR11,PCRE_UTF8_ERR12,PCRE_UTF8_ERR13,PCRE_UTF8_ERR14,
    PCRE_UTF8_ERR15,PCRE_UTF8_ERR16,PCRE_UTF8_ERR17,PCRE_UTF8_ERR18,
    PCRE_UTF8_ERR19,PCRE_UTF8_ERR2,PCRE_UTF8_ERR20,PCRE_UTF8_ERR21,
    PCRE_UTF8_ERR3,PCRE_UTF8_ERR4,PCRE_UTF8_ERR5,PCRE_UTF8_ERR6,PCRE_UTF8_ERR7,
    PCRE_UTF8_ERR8,PCRE_UTF8_ERR9,
    # Operators
    !,!=,$,%,&,*,+,-,.!=,.*,./,.<,.<=,.==,.>,.>=,.\,.^,/,//,:,<,<:,<<,<=,==,
    >,>=,>>,>>>,\,^,|,~,
    A_ldiv_Bc,A_ldiv_Bt,A_mul_Bc,A_mul_Bt,A_rdiv_Bc,A_rdiv_Bt,Ac_ldiv_B,
    Ac_ldiv_Bc,Ac_mul_B,Ac_mul_Bc,Ac_rdiv_B,Ac_rdiv_Bc,At_ldiv_B,At_ldiv_Bt,
    At_mul_B,At_mul_Bt,At_rdiv_B,At_rdiv_Bt,
    # Functions
    _c_free,abs,abs2,acos,acosd,acosh,acot,acotd,acoth,acsc,acscd,acsch,add,
    add_cmds,add_each,add_fd_handler,add_ports,add_weak_key,add_weak_value,
    addprocs_local,addprocs_sge,addprocs_ssh,alignment,all,allp,
    amap,and!,angle,ans,any,anyp,append,append!,apropos,areduce,
    ascii,asec,asecd,asech,asin,asind,asinh,assert,assign,at_each,atan,atan2,
    atand,atanh,basename,begins_with,betarnd,bfft,bfftn,bin,binomial,bitmix,bits,bool,
    brfft,brfftn,broadcast,bswap,bsxfun,byte_string_classify,cartesian_map,cat,
    cbrt,cd,ceil,cell,cell_1d,cell_2d,changedist,char,chars,charwidth,
    check_ascii,check_utf8,chi2rnd,chol,chol!,chomp,choose,chop,chr2ind,
    circshift,cis,clamp,close,cmd_stdin_stream,cmd_stdout_stream,cmds,cmp,
    colon,combinations,compile_hint,complement,complement!,complex,complex128,
    complex64,cond,conj,conj!,connect,consume,contains,contains_is,conv,conv2,
    convert,copy,copy_to,copy_to!,copysign,cor,cor_pearson,cor_spearman,cos,
    cosc,cosd,cosh,cot,cotd,coth,count,count_ones,count_zeros,countlines,countp,
    cov,cov_pearson,cov_spearman,cross,csc,cscd,csch,cstring,csvread,csvwrite,
    ctranspose,cumprod,cumsum,current_task,cwd,darray,dcell,dec,decile,deconv,
    defaultdist,degrees2radians,del,del_all,del_each,del_fd_handler,
    den,depth,deserialize,det,dfill,diag,diagm,diagmm,diagmm!,dict,diff,
    dist,distdim,distribute,div,dlmread,dlmwrite,dlopen,dlsym,done,dones,dot,
    drand,drandn,dump,dup2,dzeros,each_col,each_col!,each_line,each_match,
    each_row,each_row!,each_search,each_vec,each_vec!,
    eatwspace,eatwspace_comment,
    edit,eig,elements,eltype,
    ends_with,enq_work,enqueue,enumerate,eof,eps,erf,erfc,errno,error,
    esc,escape_string,exec,exit,exp,exp2,expm1,expr,exprnd,eye,
    factor,factorial,falses,fd,fdio,fetch,fft,fft2,fft3,fft_num_threads,
    fftn,fftshift,fftw_forget_wisdom,fftwd_import_wisdom_from_filename,
    fftwf_import_wisdom_from_filename,fill,fill!,filt,filter,filter!,finalizer,
    find,find_in_path,findmax,findmin,findn,findn_nzs,finfer,first,
    first_utf8_byte,fld,flipdim,fliplr,flipsign,flipud,float,float32,
    float32_isvalid,float64,float64_isvalid,float64_valued,floor,flush,
    force,foreach,fork,fpart,fprintf,frexp,full,fullfile,function_loc,gamma,gc,
    gc_disable,gc_enable,gcd,gcdx,gen_cartesian_map,gensym,get,getenv,
    gethostname,getipaddr,getmethods,getpid,gradient,
    grow,has,hasenv,hash,hcat,help,help_for,hex,hex2num,hist,histc,
    htol,hton,hvcat,hypot,iceil,identity,ifft,ifft2,ifft3,ifftn,ifftshift,
    ifloor,ignorestatus,ilogb,imag,in,include_string,ind2chr,ind2sub,inf,
    insert,int,int16,int2str,int32,int64,int8,integer,integer_partitions,
    integer_valued,inter,intersect,intersection,intersection!,intset,inv,invmod,
    invperm,ipart,ipermute,irfft,irfftn,iround,is_file_readable,is_go_member,
    is_hex_digit,is_utf8_start,is_valid_ascii,is_valid_utf8,isbool,isbuiltin,
    iscomplex,isconst,isdenormal,isempty,isequal,iseven,isfinite,isgeneric,
    ishermitian,isinf,isinteger,isinteractive,isleaftype,isless,islogical,isnan,
    isodd,isperm,ispow2,isprime,isready,isreal,issorted,issym,issym_rnd,
    istaskdone,istril,istriu,isvalid,iswalnum,iswalpha,iswascii,iswblank,
    iswcntrl,iswdigit,iswgraph,iswlower,iswprint,iswpunct,iswspace,iswupper,
    iswxdigit,itrunc,join,key,keys,kron,last,lc,lcfirst,lcm,ldexp,leading_ones,
    leading_zeros,length,less,lexcmp,lfact,lgamma,linreg,linspace,load,localize,
    localize_copy,locate,log,log10,log1p,log2,logb,logspace,lowercase,lpad,ls,
    lstrip,ltoh,lu,lu!,mad,make_pipe,make_scheduled,map,map_to,map_to2,
    map_vectorized,mapreduce,match,matches,matmul2x2,matmul3x3,max,maxdim,
    maxintfloat,mean,median,memcat,memchr,memio,merge,merge!,method_missing,min,
    mmap,mmap_array,mmap_grow,mmap_stream_settings,mod,mod1,modf,msync,munmap,
    myid,myindexes,nCr,nPr,names,nan,nb_available,ndigits,ndigits0z,ndims,next,
    nextfloat,nextind,nextpow2,nnz,nonzeros,norm,not!,nprocs,nthbyte,nthperm,
    nthperm!,ntoh,ntuple,num,num2hex,numel,oct,oftype,one,ones,open,or!,order,
    other,out,output,owner,pairs,parse,parse_bin,parse_float,parse_hex,
    parse_input_line,parse_int,parse_oct,parseatom,partitions,pascal,
    peakflops,permute,pfor,pieceindex,pieceindexes,pipeline_error,pmap,
    pmap_static,pointer,pointer_to_array,pop,position,pow,power_by_squaring,
    powermod,preduce,prevfloat,prevind,print,print_escaped,print_joined,
    print_matrix,print_quoted,print_quoted_literal,print_shortest,
    print_unescaped,print_unescaped_chars,printf,println,process_exit_status,
    process_exited,process_options,process_running,process_signaled,
    process_status,process_stop_signal,process_stopped,process_term_signal,
    procs,prod,produce,promote,promote_rule,promote_shape,promote_type,
    ptr_arg_convert,push,put,qr,quantile,quartile,quintile,quit,quote_string,
    radians2degrees,rand,rand!,randbeta,randbeta!,randbit,randbit!,randbool,
    randbool!,randchi2,randchi2!,randcycle,randexp,randexp!,randg,randg!,randg2,
    randi,randi!,randival,randival!,randn,randn!,randperm,randsym,rank,rational,
    read,read_from,readall,readchomp,readline,readlines,readuntil,real,
    real_valued,realmax,realmin,reduce,ref,rehash,reim,reinterpret,rem,
    remote_call,remote_call_fetch,remote_call_wait,remote_do,repeat,
    repl_show,replace,repmat,reshape,reverse,reverse!,rfft,rfftn,rot180,rot90,
    rotl90,rotr90,round,rpad,rr2id,rref,rstrip,run,safe_char,scan,search,
    searchsorted,sec,secd,sech,seek,select,select!,select_read,serialize,
    setenv,setfield,setsuccess,shift,show,showall,showcompact,shuffle,shuffle!,
    sign,signbit,signed,similar,sin,sinc,sind,sinh,size,sizeof,skip,
    sleep,slice,slicedim,sort,sort!,sort_by,sort_by!,sortperm,sortr,sortr!,
    spawn,spawnat,spawnlocal,split,sprint,sprintf,sqrt,square,squeeze,srand,
    sshow,start,std,stderr,stderr_stream,stdin,stdin_stream,stdout,
    stdout_stream,step,strcat,strchr,strerror,strftime,stride,strides,string,
    strip,strlen,strptime,strwidth,sub,sub2ind,success,successful,sum,summary,
    super,svd,svdvals,symbol,system,system_error,take,takebuf_string,tan,tand,
    tanh,thisind,tic,tiedrank,time,time_ns,tintersect,tls,tmpnam,toc,toggle,toggle_each,
    toq,trace,trailing_ones,trailing_zeros,transform_to_utf8,transpose,trideig,
    tril,triu,trues,trunc,truncate,tty_cols,tty_rows,typemax,typemin,uc,ucfirst,
    uid,uint,uint16,uint32,uint64,uint8,unescape_chars,unescape_string,union,
    union!,unique_name,unsetenv,unshift,unsigned,uppercase,utf8,values,var,vcat,
    vec,wait,wait_nohang,weighted_mean,which,whicht,whos,with_output_to_string,
    write,write_to,xcorr,xor!,yield,zero,zeros,zip, nextprod,
    prevprod, base, findfirst, qrp, sdd, require, ref_shape,
    assign_shape_check, to_index, indices, append_any, make_loop_nest,
    randstring, basename, dirname, dirname_basename, file_path, path_expand, file_copy,
    file_remove, file_create, path_rename, dir_create, dir_remove,
    file_exists, tempdir, tempfile, download_file, real_path,
    abs_path, abs_path_split, filemode, fileparts, filesize, fullfile, isrooted, split_extension, 
    split_path, tilde_expand, mtime, ctime, stat, lstat, isfifo, ispath,
    ischardev, isdir, isblockdev, isfile, islink, issocket, issetuid,
    issetgid, issticky, isreadable, iswriteable, isexecutable, uperm,
    gperm, operm, trailingsize, check_bounds, search_sorted_last,
    search_sorted_first,
    # Macros
    @v_str, @unexpected, @assert, @r_str, @str, @S_str, @I_str, @E_str,
    @B_str, @b_str, @cmd, @time, @elapsed, @windows_only, @unix_only,
    @sync, @spawn, @spawnlocal, @spawnat, @everywhere, @parallel,
    @gensym, @eval, @task, @f_str, @thunk, @L_str, @vectorize_1arg,
    @vectorize_2arg, @printf

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
include("signal.jl")
include("signal_fftw.jl")


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
compile_hint(ref, (Type{String}, ASCIIString, ASCIIString))
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
