## core libc calls ##

@unix_only _getenv(var::String) = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), var)
@unix_only hasenv(s::String) = _getenv(s) != C_NULL

@windows_only begin
_getenvlen(var::String) = ccall(:GetEnvironmentVariableA,stdcall,Uint32,(Ptr{Uint8},Ptr{Uint8},Uint32),var,C_NULL,0)
hasenv(s::String) = false#_getenvlen(s)!=0
function _jl_win_getenv(s::String,len::Uint32)
    val=zeros(Uint8,len)
    ret=ccall(:GetEnvironmentVariableA,stdcall,Uint32,(Ptr{Uint8},Ptr{Uint8},Uint32),s,val,len)
    if(ret==0||ret!=len-1) #Trailing 0 is only included on first call to GetEnvA
        error("getenv: unknown system error: ", s, len, ret)
    end
    val
end
end

macro accessEnv(var,errorcase)
@unix_only quote
    val=_getenv($var)
    if val == C_NULL
        $errorcase
    end
    cstring(val)
end
@windows_only quote
    len=_getenvlen($var)
    if len == 0
        $errorcase
    end
    cstring(convert(Ptr{Uint8},_jl_win_getenv($var,len)))
end
end

getenv(var::String) = @accessEnv var error("getenv: undefined variable: ", var)

function setenv(var::String, val::String, overwrite::Bool)
@unix_only begin
    ret = ccall(:setenv, Int32, (Ptr{Uint8},Ptr{Uint8},Int32), var, val, overwrite)
    system_error(:setenv, ret != 0)
end
@windows_only begin
if(overwrite||!hasenv(var))
    ret = ccall(:SetEnvironmentVariableA,stdcall,Int32,(Ptr{Uint8},Ptr{Uint8}),var,val)
    system_error(:setenv, ret == 0)
end
end
end

setenv(var::String, val::String) = setenv(var, val, true)

function unsetenv(var::String)
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

type EnvHash <: Associative{ByteString,ByteString}; end

const ENV = EnvHash()

ref(::EnvHash, k::String) = @accessEnv k throw(KeyError(k))

get(::EnvHash, k::String, deflt) = @accessEnv k (return deflt)

has(::EnvHash, k::String) = hasenv(k)
del(::EnvHash, k::String) = unsetenv(k)
assign(::EnvHash, v::String, k::String) = (setenv(k,v); v)

start(::EnvHash) = 0
done(::EnvHash, i) = (ccall(:jl_environ, Any, (Int32,), i) == nothing)
function next(::EnvHash, i)
    env = ccall(:jl_environ, Any, (Int32,), i)
    if env == nothing
        error("index out of range")
    end
    env::ByteString
    m = match(r"^(.*?)=(.*)$"s, env)
    if m == nothing
        error("malformed environment entry: $env")
    end
    (m.captures, i+1)
end
function length(::EnvHash)
    i = 0
    for (k,v) in ENV
        i += 1
    end
    return i
end

function show(io, ::EnvHash)
    for (k,v) = ENV
        println(io, "$k=$v")
    end
end

## misc environment-related functionality ##

tty_cols() = parse_int(Int32, get(ENV,"COLUMNS","80"), 10)
tty_rows() = parse_int(Int32, get(ENV,"LINES","25"), 10)
