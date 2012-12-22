## core libc calls ##

hasenv(s::String) = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), s) != C_NULL

function getenv(var::String)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), var)
    if val == C_NULL
        error("getenv: undefined variable: ", var)
    end
    bytestring(val)
end

function setenv(var::String, val::String, overwrite::Bool)
@unix_only begin
    ret = ccall(:setenv, Int32, (Ptr{Uint8},Ptr{Uint8},Int32), var, val, overwrite)
    system_error(:setenv, ret != 0)
end
@windows_only begin
    ret = ccall(:SetEnvironmentVariableA,stdcall,Int32,(Ptr{Uint8},Ptr{Uint8}),var,val)
    system_error(:setenv, ret == 0)
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

function ref(::EnvHash, k::String)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), k)
    if val == C_NULL
        throw(KeyError(k))
    end
    bytestring(val)
end

function get(::EnvHash, k::String, deflt)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), k)
    if val == C_NULL
        return deflt
    end
    bytestring(val)
end

has(::EnvHash, k::String) = hasenv(k)
del(::EnvHash, k::String) = unsetenv(k)
assign(::EnvHash, v, k::String) = setenv(k,string(v))

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
