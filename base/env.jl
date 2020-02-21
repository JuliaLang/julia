# This file is a part of Julia. License is MIT: https://julialang.org/license

if Sys.iswindows()
    const ERROR_ENVVAR_NOT_FOUND = UInt32(203)

    _getenvlen(var::Vector{UInt16}) = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,C_NULL,0)
    _hasenv(s::Vector{UInt16}) = _getenvlen(s) != 0 || Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND
    _hasenv(s::AbstractString) = _hasenv(cwstring(s))

    function access_env(onError::Function, str::AbstractString)
        var = cwstring(str)
        len = _getenvlen(var)
        if len == 0
            return Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND ? "" : onError(str)
        end
        val = zeros(UInt16,len)
        ret = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,val,len)
        windowserror(:getenv, (ret == 0 && len != 1) || ret != len-1 || val[end] != 0)
        pop!(val) # NUL
        return transcode(String, val)
    end

    function _setenv(svar::AbstractString, sval::AbstractString, overwrite::Bool=true)
        var = cwstring(svar)
        val = cwstring(sval)
        if overwrite || !_hasenv(var)
            ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,val)
            windowserror(:setenv, ret == 0)
        end
    end

    function _unsetenv(svar::AbstractString)
        var = cwstring(svar)
        ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,C_NULL)
        windowserror(:setenv, ret == 0)
    end
else # !windows
    _getenv(var::AbstractString) = ccall(:getenv, Cstring, (Cstring,), var)
    _hasenv(s::AbstractString) = _getenv(s) != C_NULL

    function access_env(onError::Function, var::AbstractString)
        val = _getenv(var)
        val == C_NULL ? onError(var) : unsafe_string(val)
    end

    function _setenv(var::AbstractString, val::AbstractString, overwrite::Bool=true)
        ret = ccall(:setenv, Int32, (Cstring,Cstring,Int32), var, val, overwrite)
        systemerror(:setenv, ret != 0)
    end

    function _unsetenv(var::AbstractString)
        ret = ccall(:unsetenv, Int32, (Cstring,), var)
        systemerror(:unsetenv, ret != 0)
    end
end # os test

## ENV: hash interface ##

"""
    EnvDict() -> EnvDict

A singleton of this type provides a hash table interface to environment variables.
"""
struct EnvDict <: AbstractDict{String,String}; end

"""
    ENV

Reference to the singleton `EnvDict`, providing a dictionary interface to system environment
variables.

(On Windows, system environment variables are case-insensitive, and `ENV` correspondingly converts
all keys to uppercase for display, iteration, and copying. Portable code should not rely on the
ability to distinguish variables by case, and should beware that setting an ostensibly lowercase
variable may result in an uppercase `ENV` key.)
"""
const ENV = EnvDict()

getindex(::EnvDict, k::AbstractString) = access_env(k->throw(KeyError(k)), k)
get(::EnvDict, k::AbstractString, def) = access_env(k->def, k)
get(::EnvDict, k::AbstractString) = (v = get(ENV, k, nothing); v === nothing ? v : Some(v))
get(f::Callable, ::EnvDict, k::AbstractString) = access_env(k->f(), k)
in(k::AbstractString, ::KeySet{String, EnvDict}) = _hasenv(k)
pop!(::EnvDict, k::AbstractString) = (v = ENV[k]; _unsetenv(k); v)
pop!(::EnvDict, k::AbstractString, def) = haskey(ENV,k) ? pop!(ENV,k) : def
delete!(::EnvDict, k::AbstractString) = (_unsetenv(k); ENV)
setindex!(::EnvDict, v, k::AbstractString) = _setenv(k,string(v))
push!(::EnvDict, kv::Pair{<:AbstractString}) = setindex!(ENV, kv.second, kv.first)

if Sys.iswindows()
    GESW() = (pos = ccall(:GetEnvironmentStringsW,stdcall,Ptr{UInt16},()); (pos,pos))
    function winuppercase(s::AbstractString)
        isempty(s) && return s
        LOCALE_INVARIANT = 0x0000007f
        LCMAP_UPPERCASE  = 0x00000200
        ws = transcode(UInt16, String(s))
        result = ccall(:LCMapStringW, stdcall, Cint, (UInt32, UInt32, Ptr{UInt16}, Cint, Ptr{UInt16}, Cint),
                       LOCALE_INVARIANT, LCMAP_UPPERCASE, ws, length(ws), ws, length(ws))
        systemerror(:LCMapStringW, iszero(result))
        return transcode(String, ws)
    end
    function iterate(hash::EnvDict, block::Tuple{Ptr{UInt16},Ptr{UInt16}} = GESW())
        if unsafe_load(block[1]) == 0
            ccall(:FreeEnvironmentStringsW, stdcall, Int32, (Ptr{UInt16},), block[2])
            return nothing
        end
        pos = block[1]
        blk = block[2]
        len = ccall(:wcslen, UInt, (Ptr{UInt16},), pos)
        buf = Vector{UInt16}(undef, len)
        GC.@preserve buf unsafe_copyto!(pointer(buf), pos, len)
        env = transcode(String, buf)
        m = match(r"^(=?[^=]+)=(.*)$"s, env)
        if m === nothing
            error("malformed environment entry: $env")
        end
        return (Pair{String,String}(winuppercase(m.captures[1]), m.captures[2]), (pos+(len+1)*2, blk))
    end
else # !windows
    function iterate(::EnvDict, i=0)
        env = ccall(:jl_environ, Any, (Int32,), i)
        env === nothing && return nothing
        env = env::String
        m = match(r"^(.*?)=(.*)$"s, env)
        if m === nothing
            error("malformed environment entry: $env")
        end
        return (Pair{String,String}(m.captures[1], m.captures[2]), i+1)
    end
end # os-test

#TODO: Make these more efficient
function length(::EnvDict)
    i = 0
    for (k,v) in ENV
        i += 1
    end
    return i
end

function show(io::IO, ::EnvDict)
    for (k,v) = ENV
        println(io, "$k=$v")
    end
end

"""
    withenv(f::Function, kv::Pair...)

Execute `f` in an environment that is temporarily modified (not replaced as in `setenv`)
by zero or more `"var"=>val` arguments `kv`. `withenv` is generally used via the
`withenv(kv...) do ... end` syntax. A value of `nothing` can be used to temporarily unset an
environment variable (if it is set). When `withenv` returns, the original environment has
been restored.
"""
function withenv(f::Function, keyvals::Pair{T}...) where T<:AbstractString
    old = Dict{T,Any}()
    for (key,val) in keyvals
        old[key] = get(ENV,key,nothing)
        val !== nothing ? (ENV[key]=val) : delete!(ENV, key)
    end
    try f()
    finally
        for (key,val) in old
            val !== nothing ? (ENV[key]=val) : delete!(ENV, key)
        end
    end
end
withenv(f::Function) = f() # handle empty keyvals case; see #10853
