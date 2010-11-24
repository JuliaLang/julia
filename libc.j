# libc = dlopen("libc")

## time-related functions ##

sleep(s::Real) = ccall(dlsym(libc,"usleep"), Uint32, (Uint32,), uint32(round(s*1e6)))
unixtime() = ccall(dlsym(libc,"time"), Uint32, (Ptr{Uint32},), C_NULL)
function ftime()
    t = Array(Uint64,2)
    ccall(dlsym(libc,"gettimeofday"), Int32,
          (Ptr{Uint64}, Ptr{Uint8}),
          t, C_NULL)
    # TODO: this is a hack and probably doesn't work on big-endian systems
    return float64(t[1]) + float64(t[2])/1e6
end

## process-related functions ##

system(cmd::String) =
    ccall(dlsym(libc,"system"), Int32, (Ptr{Uint8},), cstring(cmd))

function fork()
    pid = ccall(dlsym(libc,"fork"), Int32, ())
    system_error("fork", pid < 0)
    return pid
end

function exec(cmd::String, args::String...)
    cmd = cstring(cmd)
    arr = Array(Ptr{Uint8}, length(args)+2)
    arr[1] = cmd
    for i = 1:length(args); arr[i+1] = cstring(args[i]); end
    arr[length(args)+2] = C_NULL
    ccall(dlsym(libc,"execvp"), Int32,
          (Ptr{Uint8}, Ptr{Ptr{Uint8}}),
          cmd, arr)
    system_error("exec", true)
end

exit(n) = ccall(dlsym(libc,"exit"), Void, (Int32,), int32(n))
exit() = exit(0)

## environment ##

hasenv(var::String) =
    ccall(dlsym(libc,"getenv"), Ptr{Uint8}, (Ptr{Uint8},), var) != C_NULL

function getenv(var::String)
    val = ccall(dlsym(libc,"getenv"), Ptr{Uint8}, (Ptr{Uint8},), var)
    if val == C_NULL; error("getenv: Undefined variable: ", var); end
    return string(val)
end

function setenv(var::String, val::String)
    ret = ccall(dlsym(libc,"setenv"), Int32,
                (Ptr{Uint8}, Ptr{Uint8}, Int32),
                var, val, 1)
    system_error("setenv", ret != 0)
end

function unsetenv(var::String)
    ret = ccall(dlsym(libc,"unsetenv"), Int32, (Ptr{Uint8},), var)
    system_error("unsetenv", ret != 0)
end

tty_columns() = parse_int(Int32, getenv("COLUMNS"), 10)
