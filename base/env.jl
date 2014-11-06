## core libc calls ##

@unix_only begin
    _getenv(var::AbstractString) = ccall(:getenv, Ptr{UInt8}, (Ptr{UInt8},), var)
    _hasenv(s::AbstractString) = _getenv(s) != C_NULL
end
@windows_only begin
const ERROR_ENVVAR_NOT_FOUND = uint32(203)
const FORMAT_MESSAGE_ALLOCATE_BUFFER = uint32(0x100)
const FORMAT_MESSAGE_FROM_SYSTEM = uint32(0x1000)
const FORMAT_MESSAGE_IGNORE_INSERTS = uint32(0x200)
const FORMAT_MESSAGE_MAX_WIDTH_MASK = uint32(0xFF)
GetLastError() = ccall(:GetLastError,stdcall,UInt32,())
function FormatMessage(e=GetLastError())
    lpMsgBuf = Array(Ptr{UInt16})
    lpMsgBuf[1] = 0
    len = ccall(:FormatMessageW,stdcall,UInt32,(Cint, Ptr{Void}, Cint, Cint, Ptr{Ptr{UInt16}}, Cint, Ptr{Void}),
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
        C_NULL, e, 0, lpMsgBuf, 0, C_NULL)
    p = lpMsgBuf[1]
    len == 0 && return utf8("")
    len = len + 1
    buf = Array(UInt16, len)
    unsafe_copy!(pointer(buf), p, len)
    ccall(:LocalFree,stdcall,Ptr{Void},(Ptr{Void},),p)
    return utf8(UTF16String(buf))
end

_getenvlen(var::UTF16String) = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt8},UInt32),utf16(var),C_NULL,0)
_hasenv(s::UTF16String) = _getenvlen(s)!=0 || GetLastError()!=ERROR_ENVVAR_NOT_FOUND
_hasenv(s::AbstractString) = _hasenv(utf16(s))
function _jl_win_getenv(s::UTF16String,len::UInt32)
    val=zeros(UInt16,len)
    ret=ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),s,val,len)
    if ret==0 || ret != len-1 || val[end] != 0
        error(string("system error getenv: ", s, ' ', len, "-1 != ", ret, ": ", FormatMessage()))
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
                if GetLastError() != ERROR_ENVVAR_NOT_FOUND
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
        ret = ccall(:setenv, Int32, (Ptr{UInt8},Ptr{UInt8},Int32), var, val, overwrite)
        systemerror(:setenv, ret != 0)
    end
    @windows_only begin
        var = utf16(var)
        if overwrite || !_hasenv(var)
            ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),utf16(var),utf16(val))
            systemerror(:setenv, ret == 0)
        end
    end
end

_setenv(var::AbstractString, val::AbstractString) = _setenv(var, val, true)

function _unsetenv(var::AbstractString)
    @unix_only begin
        ret = ccall(:unsetenv, Int32, (Ptr{UInt8},), var)
        systemerror(:unsetenv, ret != 0)
    end
    @windows_only begin
        ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),utf16(var),C_NULL)
        systemerror(:setenv, ret == 0)
    end
end

## ENV: hash interface ##

type EnvHash <: Associative{ByteString,ByteString}; end
const ENV = EnvHash()

similar(::EnvHash) = Dict{ByteString,ByteString}()

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
done(::EnvHash, i) = (ccall(:jl_environ, Any, (Int32,), i) == nothing)

function next(::EnvHash, i)
    env = ccall(:jl_environ, Any, (Int32,), i)
    if env == nothing
        error(BoundsError)
    end
    env::ByteString
    m = match(r"^(.*?)=(.*)$"s, env)
    if m == nothing
        error("malformed environment entry: $env")
    end
    (ByteString[convert(typeof(env),x) for x in m.captures], i+1)
end
end

@windows_only begin
start(hash::EnvHash) = (pos = ccall(:GetEnvironmentStringsW,stdcall,Ptr{UInt16},()); (pos,pos))
function done(hash::EnvHash, block::(Ptr{UInt16},Ptr{UInt16}))
    if unsafe_load(block[1])==0
        ccall(:FreeEnvironmentStringsW,stdcall,Int32,(Ptr{UInt16},),block[2])
        return true
    end
    false
end
function next(hash::EnvHash, block::(Ptr{UInt16},Ptr{UInt16}))
    pos = block[1]
    blk = block[2]
    len = ccall(:wcslen, UInt, (Ptr{UInt16},), pos)+1
    buf = Array(UInt16, len)
    unsafe_copy!(pointer(buf), pos, len)
    env = utf8(UTF16String(buf))
    m = match(r"^(=?[^=]+)=(.*)$"s, env)
    if m == nothing
        error("malformed environment entry: $env")
    end
    (ByteString[convert(typeof(env),x) for x in m.captures], (pos+len*2, blk))
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
function with_env(f::Function, key::AbstractString, val)
    old = get(ENV,key,nothing)
    val != nothing ? (ENV[key]=val) : _unsetenv(key)
    try f()
    finally
        old != nothing ? (ENV[key]=old) : _unsetenv(key)
    catch
        rethrow()
    end
end

## misc environment-related functionality ##

function tty_size()
    if isdefined(Base, :active_repl)
        os = REPL.outstream(Base.active_repl)
        if isa(os, Terminals.TTYTerminal)
            return size(os)
        end
    end
    return (parseint(get(ENV,"LINES","24")),
            parseint(get(ENV,"COLUMNS","80")))
end
