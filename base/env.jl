## core libc calls ##

@unix_only begin
    _getenv(var::String) = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), var)
    _hasenv(s::String) = _getenv(s) != C_NULL
end
@windows_only begin
_getenvlen(var::String) = ccall(:GetEnvironmentVariableA,stdcall,Uint32,(Ptr{Uint8},Ptr{Uint8},Uint32),var,C_NULL,0)
_hasenv(s::String) = _getenvlen(s)!=0
function _jl_win_getenv(s::String,len::Uint32)
    val=zeros(Uint8,len-1)
    ret=ccall(:GetEnvironmentVariableA,stdcall,Uint32,(Ptr{Uint8},Ptr{Uint8},Uint32),s,val,len)
    if(ret==0||ret!=len-1) #Trailing 0 is only included on first call to GetEnvA
        error("getenv: unknown system error: ", s, len, ret)
    end
    val
end
end


macro accessEnv(var,errorcase)
@unix_only return quote
     val=_getenv($var)
     if val == C_NULL
        $errorcase
     end
     bytestring(val)
end
@windows_only return quote
    len=_getenvlen($var)
    if len == 0
        $errorcase
    end
    bytestring(_jl_win_getenv($var,len))
end
end

function _setenv(var::String, val::String, overwrite::Bool)
@unix_only begin
    ret = ccall(:setenv, Int32, (Ptr{Uint8},Ptr{Uint8},Int32), var, val, overwrite)
    system_error(:setenv, ret != 0)
end
@windows_only begin
    if(overwrite||!_hasenv(var))
        ret = ccall(:SetEnvironmentVariableA,stdcall,Int32,(Ptr{Uint8},Ptr{Uint8}),var,val)
        system_error(:setenv, ret == 0)
    end
end
end

_setenv(var::String, val::String) = _setenv(var, val, true)

function _unsetenv(var::String)
@unix_only begin
    ret = ccall(:unsetenv, Int32, (Ptr{Uint8},), var)
    system_error(:unsetenv, ret != 0)
end
@windows_only begin
    ret = ccall(:SetEnvironmentVariableA,stdcall,Int32,(Ptr{Uint8},Ptr{Uint8}),var,C_NULL)
    system_error(:setenv, ret == 0)
end
end

## ENV: hash interface ##

@unix_only type EnvHash <: Associative{ByteString,ByteString}; end
@windows_only type EnvHash <: Associative{ByteString,ByteString}
    block::Ptr{Uint8}
    EnvHash() = new(C_NULL)
end
const ENV = EnvHash()

getindex(::EnvHash, k::String) = @accessEnv k throw(KeyError(k))
get(::EnvHash, k::String, def) = @accessEnv k (return def)
has(::EnvHash, k::String) = _hasenv(k)
delete!(::EnvHash, k::String) = (v = ENV[k]; _unsetenv(k); v)
delete!(::EnvHash, k::String, def) = has(ENV,k) ? delete!(ENV,k) : def
setindex!(::EnvHash, v, k::String) = _setenv(k,string(v))

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
    (m.captures, i+1)
end
end

@windows_only begin
start(hash::EnvHash) = (hash.block = ccall(:GetEnvironmentStringsA,stdcall,Ptr{Uint8},()))
function done(hash::EnvHash, pos::Ptr{Uint8})
    if(ccall(:jl_env_done,Any,(Ptr{Uint8},),pos)::Bool)
        ccall(:FreeEnvironmentStringsA,stdcall,Int32,(Ptr{Uint8},),hash.block)
        hash.block=C_NULL
        return true
    end
    false
end
function next(hash::EnvHash, pos::Ptr{Uint8})
    len = ccall(:strlen, Uint, (Ptr{Uint8},), pos)
    env=ccall(:jl_pchar_to_string, Any, (Ptr{Uint8},Int), pos, len)::ByteString
    m = match(r"^(.*?)=(.*)$"s, env)
    if m == nothing
        error("malformed environment entry: $env")
    end
    (m.captures, pos+len+1)
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

## misc environment-related functionality ##

tty_cols() = parse_int(Int32, get(ENV,"COLUMNS","80"), 10)
tty_rows() = parse_int(Int32, get(ENV,"LINES","25"), 10)
