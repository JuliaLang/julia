# libc = dlopen("libc")

## time-related functions ##

sleep(s::Real) = ccall(dlsym(libc, :usleep), Uint32, (Uint32,), uint32(round(s*1e6)))
unixtime() = ccall(dlsym(libc, :time), Uint32, (Ptr{Uint32},), C_NULL)

## process-related functions ##

system(cmd::String) =
    ccall(dlsym(libc, :system), Int32, (Ptr{Uint8},), cstring(cmd))

## environment ##

hasenv(var::String) =
    ccall(dlsym(libc, :getenv), Ptr{Uint8}, (Ptr{Uint8},), var) != C_NULL

function getenv(var::String)
    val = ccall(dlsym(libc, :getenv), Ptr{Uint8}, (Ptr{Uint8},), var)
    if val == C_NULL; error("getenv: Undefined variable: ", var); end
    return string(val)
end

function setenv(var::String, val::String)
    ret = ccall(dlsym(libc, :setenv), Int32,
                (Ptr{Uint8}, Ptr{Uint8}, Int32),
                var, val, 1)
    system_error(:setenv, ret != 0)
end

function unsetenv(var::String)
    ret = ccall(dlsym(libc, :unsetenv), Int32, (Ptr{Uint8},), var)
    system_error(:unsetenv, ret != 0)
end

tty_columns() = parse_int(Int32, getenv("COLUMNS"), 10)
