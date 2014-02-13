# prime method cache with some things we know we'll need right after startup
precompile(pwd, ())
precompile(fdio, (Int32,))
precompile(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
precompile(next, (Dict{Any,Any}, Int))
precompile(start, (Dict{Any,Any},))
precompile(isempty, (Array{Any,1},))
precompile(getindex, (Dict{Any,Any}, Int32))
precompile(_start, ())
precompile(process_options, (Array{Any,1},))
precompile(run_repl, ())
precompile(any, (Function, Array{Any,1}))
precompile(Dict{Any,Any}, (Int,))
precompile(Set, ())
precompile(setindex!, (Dict{Any,Any}, Bool, Cmd))
precompile(rehash, (Dict{Any,Any}, Int))
precompile(wait, ())
precompile(systemerror, (Symbol, Bool))
precompile(SystemError, (ASCIIString,))
precompile(has, (EnvHash, ASCIIString))
precompile(parse_input_line, (ASCIIString,))
precompile(cmp, (Int32, Int32))
precompile(min, (Int32, Int32))
precompile(==, (ASCIIString, ASCIIString))
precompile(arg_gen, (ASCIIString,))
precompile(Random.librandom_init, ())
precompile(Random.srand, (ASCIIString, Int))
precompile(Random.srand, (Uint64,))
precompile(open, (ASCIIString, Bool, Bool, Bool, Bool))
precompile(done, (IntSet, Int64))
precompile(next, (IntSet, Int64))
precompile(ht_keyindex, (Dict{Any,Any}, Int32))
precompile(notify_cantake, (RemoteValue,))
precompile(notify_canput, (RemoteValue,))
precompile(work_result, (RemoteValue,))
precompile(take, (RemoteValue,))
precompile(wait_cantake, (RemoteValue,))
precompile(enq_work, (Task,))
precompile(string, (Int,))
precompile(parseint, (Type{Int}, ASCIIString, Int))
precompile(repeat, (ASCIIString, Int))
precompile(KeyError, (Int,))
precompile(show, (Float64,))
precompile(match, (Regex, ASCIIString))
precompile(length, (ASCIIString,))
precompile(alignment, (Float64,))
precompile(repl_callback, (Expr, Int))
precompile(istaskdone, (Task,))
precompile(int, (Uint64,))
precompile(copy, (Bool,))
precompile(bool, (Bool,))
precompile(bool, (RemoteRef,))
precompile(wait, (RemoteRef,))
precompile(hash, (RemoteRef,))
precompile(take, (RemoteRef,))
precompile(bitmix, (Int, Int))
precompile(bitmix, (Uint, Int))
precompile(bitmix, (Uint64, Int64))
precompile(hash, (Int,))
precompile(isequal, (Symbol, Symbol))
precompile(isequal, (Bool, Bool))
precompile(get, (EnvHash, ASCIIString, ASCIIString))
precompile(rr2id, (RemoteRef,))
precompile(isequal, (RemoteRef, WeakRef))
precompile(isequal, (RemoteRef, RemoteRef))
precompile(_ieval, (Symbol,))
precompile(static_convert, (Nothing, Nothing))
precompile(setindex!, (Array{Any,1}, WeakRef, Int))
precompile(isequal, ((Int,Int),(Int,Int)))
precompile(isequal, (Int,Int))
precompile(RemoteRef, (Int, Int, Int))
precompile(eval_user_input, (Expr, Bool))
precompile(print, (Float64,))
precompile(a2t, (Array{Any,1},))
precompile(flush, (IOStream,))
precompile(getindex, (Type{ByteString}, ASCIIString, ASCIIString))
precompile(bytestring, (ASCIIString,))
precompile(int, (Int,))
precompile(uint, (Uint,))
precompile(_atexit, ())
precompile(read, (IOStream, Array{Uint32,1}))
precompile(hex, (Char, Int))
precompile(abs, (Char,))
precompile(abstract_eval, (LambdaStaticData, ObjectIdDict, StaticVarInfo))
precompile(length, (Range1{Int},))
precompile(start, (Range1{Int},))
precompile(done, (Range1{Int},Int))
precompile(next, (Range1{Int},Int))
precompile(IOStream, (ASCIIString, Array{Uint8,1}))
precompile(mk_tupleref, (SymbolNode, Int))
precompile(abstract_interpret, (Bool, ObjectIdDict, StaticVarInfo))
precompile(eval_annotate, (LambdaStaticData, ObjectIdDict, StaticVarInfo, ObjectIdDict, Array{Any,1}))
precompile(occurs_more, (Bool, Function, Int))
precompile(isconstantfunc, (SymbolNode, StaticVarInfo))
precompile(CallStack, (Expr, Module, (Nothing,), EmptyCallStack))
precompile(convert, (Type{Module}, Module))
precompile(effect_free, (Expr,))
precompile(effect_free, (TopNode,))
precompile(abspath, (ASCIIString,))
precompile(isabspath, (ASCIIString,))
precompile(split, (ASCIIString,))
precompile(split, (ASCIIString, ASCIIString, Int, Bool))
precompile(split, (ASCIIString, Regex, Int, Bool))
precompile(print_joined, (IOBuffer, Array{String,1}, ASCIIString))
precompile(beginswith, (ASCIIString, ASCIIString))
precompile(resolve_globals, (Symbol, Module, Module, Vector{Any}, Vector{Any}))
precompile(resolve_globals, (SymbolNode, Module, Module, Vector{Any}, Vector{Any}))
precompile(BitArray, (Int,))
precompile(getindex, (BitArray{1}, Int,))
precompile(setindex!, (BitArray{1}, Bool, Int,))
precompile(fill!, (BitArray{1}, Bool))
precompile(pop!, (Array{Any,1},))
precompile(unshift!, (Array{Any,1}, Task))
precompile(nnz, (BitArray{1},))
precompile(get_chunks_id, (Int,))
precompile(occurs_more, (Uint8, Function, Int))
precompile(abstract_eval_arg, (Uint8, ObjectIdDict, StaticVarInfo))
precompile(occurs_outside_tupleref, (Function, Symbol, StaticVarInfo, Int))
precompile(search, (ASCIIString, Regex, Int))
precompile(setindex!, (Vector{Any}, Uint8, Int))
precompile(setindex!, (Vector{Any}, Vector{Any}, Int))
precompile(first, (Range1{Int},))
precompile(last, (Range1{Int},))
precompile(isempty, (ASCIIString,))
precompile(normpath, (ASCIIString,))
precompile(print, (ASCIIString,))
precompile(println, (TTY,))
precompile(print, (TTY,Char))
precompile(==, (Bool,Bool))
precompile(try_include, (ASCIIString,))
precompile(isfile, (ASCIIString,))
precompile(include_from_node1, (ASCIIString,))
precompile(source_path, (Nothing,))
precompile(task_local_storage, ())
precompile(atexit, (Function,))
precompile(print, (TTY, ASCIIString))
precompile(close, (TTY,))
precompile(put, (RemoteRef, Any))
precompile(getpid, ())
precompile(print, (IOStream, Int32))
precompile(show, (IOStream, Int32))
precompile(open, (ASCIIString, ASCIIString))
precompile(readline, (ASCIIString,))
precompile(endof, (Array{Any,1},))
precompile(sym_replace, (Uint8, Array{Any,1}, Array{Any,1}, Array{Any,1}, Array{Any,1}))
precompile(isslotempty, (Dict{Any,Any}, Int))
precompile(setindex!, (Array{Uint8,1}, Uint8, Int))
precompile(get, (Dict{Any,Any}, Symbol, ASCIIString))
precompile(*, (ASCIIString, ASCIIString, ASCIIString))
precompile(chop, (ASCIIString,))
precompile(ismatch, (Regex, ASCIIString))
precompile(!=, (Bool, Bool))
precompile(nextind, (ASCIIString, Int))
precompile(delete_var!, (Expr, Symbol))
precompile(close, (IOStream,))
precompile(haskey, (ObjectIdDict, Symbol))
