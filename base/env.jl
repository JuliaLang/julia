# This file is a part of Julia. License is MIT: http://julialang.org/license

@unix_only begin
    _getenv(var::AbstractString) = ccall(:getenv, Ptr{UInt8}, (Cstring,), var)
    _hasenv(s::AbstractString) = _getenv(s) != C_NULL
end

@windows_only begin
const ERROR_ENVVAR_NOT_FOUND = UInt32(203)
_getenvlen(var::AbstractString) = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Cwstring,Ptr{UInt8},UInt32),var,C_NULL,0)
_hasenv(s::AbstractString) = _getenvlen(s)!=0 || Libc.GetLastError()!=ERROR_ENVVAR_NOT_FOUND
function _jl_win_getenv(s::UTF16String,len::UInt32)
    val=zeros(UInt16,len)
    ret=ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Cwstring,Ptr{UInt16},UInt32),s,val,len)
    if (ret == 0 && len != 1) || ret != len-1 || val[end] != 0
        error(string("getenv: ", s, ' ', len, "-1 != ", ret, ": ", Libc.FormatMessage()))
    end
    val
end
end

macro accessEnv(var,errorcase)
    @unix_only return quote
         val=_getenv($(esc(var)))
         if val == C_NULL
            $(esc(errorcase))
         end
         bytestring(val)
    end
    @windows_only return quote
        let var = utf16($(esc(var)))
            len=_getenvlen(var)
            if len == 0
                if Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND
                    return utf8("")
                else
                    $(esc(errorcase))
                end
            end
            utf8(UTF16String(_jl_win_getenv(var,len)))
        end
    end
end

function _setenv(var::AbstractString, val::AbstractString, overwrite::Bool)
    @unix_only begin
        ret = ccall(:setenv, Int32, (Cstring,Cstring,Int32), var, val, overwrite)
        systemerror(:setenv, ret != 0)
    end
    @windows_only begin
        var = utf16(var)
        if overwrite || !_hasenv(var)
            ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Cwstring,Cwstring),var,val)
            systemerror(:setenv, ret == 0)
        end
    end
end

_setenv(var::AbstractString, val::AbstractString) = _setenv(var, val, true)

function _unsetenv(var::AbstractString)
    @unix_only begin
        ret = ccall(:unsetenv, Int32, (Cstring,), var)
        systemerror(:unsetenv, ret != 0)
    end
    @windows_only begin
        ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Cwstring,Ptr{UInt16}),var,C_NULL)
        systemerror(:setenv, ret == 0)
    end
end

## ENV: hash interface ##

type EnvHash <: Associative{UTF8String,UTF8String}; end
const ENV = EnvHash()

similar(::EnvHash) = Dict{UTF8String,UTF8String}()

getindex(::EnvHash, k::AbstractString) = @accessEnv k throw(KeyError(k))
get(::EnvHash, k::AbstractString, def) = @accessEnv k (return def)
in(k::AbstractString, ::KeyIterator{EnvHash}) = _hasenv(k)
pop!(::EnvHash, k::AbstractString) = (v = ENV[k]; _unsetenv(k); v)
pop!(::EnvHash, k::AbstractString, def) = haskey(ENV,k) ? pop!(ENV,k) : def
function delete!(::EnvHash, k::AbstractString)
    warn_once("""
        delete!(ENV,key) now returns the modified environment.
        Use pop!(ENV,key) to retrieve the value instead.
        """)
    _unsetenv(k)
    ENV
end
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
    env::UTF8String
    m = match(r"^(.*?)=(.*)$"s, env)
    if m === nothing
        error("malformed environment entry: $env")
    end
    (Pair{UTF8String,UTF8String}(m.captures[1], m.captures[2]), i+1)
end
end

@windows_only begin
start(hash::EnvHash) = (pos = ccall(:GetEnvironmentStringsW,stdcall,Ptr{UInt16},()); (pos,pos))
function done(hash::EnvHash, block::Tuple{Ptr{UInt16},Ptr{UInt16}})
    if unsafe_load(block[1])==0
        ccall(:FreeEnvironmentStringsW,stdcall,Int32,(Ptr{UInt16},),block[2])
        return true
    end
    false
end
function next(hash::EnvHash, block::Tuple{Ptr{UInt16},Ptr{UInt16}})
    pos = block[1]
    blk = block[2]
    len = ccall(:wcslen, UInt, (Ptr{UInt16},), pos)+1
    buf = Array(UInt16, len)
    unsafe_copy!(pointer(buf), pos, len)
    env = utf8(UTF16String(buf))
    m = match(r"^(=?[^=]+)=(.*)$"s, env)
    if m === nothing
        error("malformed environment entry: $env")
    end
    (Pair{UTF8String,UTF8String}(m.captures[1], m.captures[2]), (pos+len*2, blk))
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
        val !== nothing ? (ENV[key]=val) : _unsetenv(key)
    end
    try f()
    finally
        for (key,val) in old
            val !== nothing ? (ENV[key]=val) : _unsetenv(key)
        end
    end
end
withenv(f::Function) = f() # handle empty keyvals case; see #10853

## misc environment-related functionality ##

function tty_size()
    if isdefined(Base, :active_repl)
        os = REPL.outstream(Base.active_repl)
        if isa(os, Terminals.TTYTerminal)
            return size(os)
        end
    end
    return (parse(Int,get(ENV,"LINES","24")),
            parse(Int,get(ENV,"COLUMNS","80")))
end
