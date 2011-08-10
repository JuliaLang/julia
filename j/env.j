## core libc calls ##

hasenv(var::String) =
    ccall(dlsym(libc, :getenv),
          Ptr{Uint8}, (Ptr{Uint8},), cstring(var)) != C_NULL

function getenv(var::String)
    val = ccall(dlsym(libc, :getenv),
                Ptr{Uint8}, (Ptr{Uint8},), cstring(var))
    if val == C_NULL
        error("getenv: Undefined variable: ", var)
    end
    cstring(val)
end

function setenv(var::String, val::String)
    ret = ccall(dlsym(libc, :setenv), Int32,
                (Ptr{Uint8}, Ptr{Uint8}, Int32),
                cstring(var), cstring(val), int32(1))
    system_error(:setenv, ret != 0)
end

function unsetenv(var::String)
    ret = ccall(dlsym(libc, :unsetenv), Int32, (Ptr{Uint8},), var)
    system_error(:unsetenv, ret != 0)
end

## ENV: hash interface ##

type EnvHash; end

ENV = EnvHash()

function ref(::EnvHash, k::String)
    try
        getenv(k)
    catch
        throw(KeyError(k))
    end
end

has(::EnvHash, k::String) = hasenv(k)
del(::EnvHash, k::String) = unsetenv(k)

assign(::EnvHash, v::String, k::String) = (setenv(k,v); v)

# TODO: make this implement Hash interface
# it should thereby inherit the ability to
# iterate its key-value pairs and display
# iteself to the world.

## misc environment functionality ##

tty_columns() = parse_int(Int32, ENV["COLUMNS"], 10)
