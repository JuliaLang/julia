## core libc calls ##

hasenv(s::String) = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), s) != C_NULL

function getenv(var::String)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), var)
    if val == C_NULL
        error("getenv: undefined variable: ", var)
    end
    cstring(val)
end

function setenv(var::String, val::String, overwrite::Bool)
    ret = ccall(:setenv, Int32, (Ptr{Uint8},Ptr{Uint8},Int32), var, val, overwrite)
    system_error(:setenv, ret != 0)
end

setenv(var::String, val::String) = setenv(var, val, true)

function unsetenv(var::String)
    ret = ccall(:unsetenv, Int32, (Ptr{Uint8},), var)
    system_error(:unsetenv, ret != 0)
end

## ENV: hash interface ##

type EnvHash <: Associative; end

const ENV = EnvHash()

function ref(::EnvHash, k::String)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), k)
    if val == C_NULL
        throw(KeyError(k))
    end
    cstring(val)
end

function get(::EnvHash, k::String, deflt)
    val = ccall(:getenv, Ptr{Uint8}, (Ptr{Uint8},), k)
    if val == C_NULL
        return deflt
    end
    cstring(val)
end

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

function show(::EnvHash)
    for (k,v) = ENV
        println("$k=$v")
    end
end

## misc environment-related functionality ##

tty_cols() = parse_int(Int32, get(ENV,"COLUMNS","80"), 10)
tty_rows() = parse_int(Int32, get(ENV,"LINES","25"), 10)
