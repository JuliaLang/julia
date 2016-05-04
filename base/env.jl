# This file is a part of Julia. License is MIT: http://julialang.org/license

@unix_only begin

_getenv(var::AbstractString) = ccall(:getenv, Cstring, (Cstring,), var)
_hasenv(s::AbstractString) = _getenv(s) != C_NULL

function access_env(onError::Function, var::AbstractString)
    val = _getenv(var)
    val == C_NULL ? onError(var) : bytestring(val)
end

function _setenv(var::AbstractString, val::AbstractString, overwrite::Bool=true)
    ret = ccall(:setenv, Int32, (Cstring,Cstring,Int32), var, val, overwrite)
    systemerror(:setenv, ret != 0)
end

function _unsetenv(var::AbstractString)
    ret = ccall(:unsetenv, Int32, (Cstring,), var)
    systemerror(:unsetenv, ret != 0)
end

end # @unix_only

@windows_only begin

const ERROR_ENVVAR_NOT_FOUND = UInt32(203)

_getenvlen(var::Vector{UInt16}) = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,C_NULL,0)
_hasenv(s::Vector{UInt16}) = _getenvlen(s) != 0 || Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND
_hasenv(s::AbstractString) = _hasenv(cwstring(s))

function access_env(onError::Function, str::AbstractString)
    var = cwstring(str)
    len = _getenvlen(var)
    if len == 0
        return Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND ? utf8("") : onError(str)
    end
    val = zeros(UInt16,len)
    ret = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,val,len)
    if (ret == 0 && len != 1) || ret != len-1 || val[end] != 0
        error(string("getenv: ", str, ' ', len, "-1 != ", ret, ": ", Libc.FormatMessage()))
    end
    pop!(val) # NUL
    return String(utf16to8(val))
end

function _setenv(svar::AbstractString, sval::AbstractString, overwrite::Bool=true)
    var = cwstring(svar)
    val = cwstring(sval)
    if overwrite || !_hasenv(var)
        ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,val)
        systemerror(:setenv, ret == 0)
    end
end

function _unsetenv(svar::AbstractString)
    var = cwstring(svar)
    ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,C_NULL)
    systemerror(:setenv, ret == 0)
end

end # @windows_only

## ENV: hash interface ##

type EnvHash <: Associative{String,String}; end
const ENV = EnvHash()

similar(::EnvHash) = Dict{String,String}()

getindex(::EnvHash, k::AbstractString) = access_env(k->throw(KeyError(k)), k)
get(::EnvHash, k::AbstractString, def) = access_env(k->def, k)
in(k::AbstractString, ::KeyIterator{EnvHash}) = _hasenv(k)
pop!(::EnvHash, k::AbstractString) = (v = ENV[k]; _unsetenv(k); v)
pop!(::EnvHash, k::AbstractString, def) = haskey(ENV,k) ? pop!(ENV,k) : def
delete!(::EnvHash, k::AbstractString) = (_unsetenv(k); ENV)
delete!(::EnvHash, k::AbstractString, def) = haskey(ENV,k) ? delete!(ENV,k) : def
setindex!(::EnvHash, v, k::AbstractString) = _setenv(k,string(v))
push!(::EnvHash, k::AbstractString, v) = setindex!(ENV, v, k)

@unix_only begin
start(::EnvHash) = 0
done(::EnvHash, i) = (ccall(:jl_environ, Any, (Int32,), i) === nothing)

function next(::EnvHash, i)
    env = ccall(:jl_environ, Any, (Int32,), i)
    if env === nothing
        throw(BoundsError())
    end
    env::String
    m = match(r"^(.*?)=(.*)$"s, env)
    if m === nothing
        error("malformed environment entry: $env")
    end
    (Pair{String,String}(m.captures[1], m.captures[2]), i+1)
end
end

@windows_only begin
start(hash::EnvHash) = (pos = ccall(:GetEnvironmentStringsW,stdcall,Ptr{UInt16},()); (pos,pos))
function done(hash::EnvHash, block::Tuple{Ptr{UInt16},Ptr{UInt16}})
    if unsafe_load(block[1]) == 0
        ccall(:FreeEnvironmentStringsW,stdcall,Int32,(Ptr{UInt16},),block[2])
        return true
    end
    false
end
function next(hash::EnvHash, block::Tuple{Ptr{UInt16},Ptr{UInt16}})
    pos = block[1]
    blk = block[2]
    len = ccall(:wcslen, UInt, (Ptr{UInt16},), pos)
    buf = Array(UInt16, len)
    unsafe_copy!(pointer(buf), pos, len)
    env = String(utf16to8(buf))
    m = match(r"^(=?[^=]+)=(.*)$"s, env)
    if m === nothing
        error("malformed environment entry: $env")
    end
    (Pair{String,String}(m.captures[1], m.captures[2]), (pos+len*2, blk))
end
end

#TODO: Make these more efficent
function length(::EnvHash)
    i = 0
    for (k,v) in ENV
        i += 1
    end
    return i
end

function show(io::IO, ::EnvHash)
    for (k,v) = ENV
        println(io, "$k=$v")
    end
end

# temporarily set and then restore an environment value
function withenv{T<:AbstractString}(f::Function, keyvals::Pair{T}...)
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
