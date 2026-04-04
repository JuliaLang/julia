# This file is a part of Julia. License is MIT: https://julialang.org/license

if Sys.iswindows()
    const ERROR_ENVVAR_NOT_FOUND = UInt32(203)

    const env_dict = Lockable(Dict{String, Vector{Cwchar_t}}())

    function memoized_env_lookup(str::AbstractString)
        # Windows environment variables have a different format from Linux / MacOS, and previously
        # incurred allocations because we had to convert a String to a Vector{Cwchar_t} each time
        # an environment variable was looked up. This function memoizes that lookup process, storing
        # the String => Vector{Cwchar_t} pairs in env_dict
        @lock env_dict begin
            var = get(env_dict[], str, nothing)
            if isnothing(var)
                var = cwstring(str)
                env_dict[][str] = var
            end
            return var
        end
    end

    _getenvlen(var::Vector{UInt16}) = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,C_NULL,0)
    _hasenv(s::Vector{UInt16}) = _getenvlen(s) != 0 || Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND
    _hasenv(s::AbstractString) = _hasenv(memoized_env_lookup(s))

    function access_env(onError::Function, str::AbstractString)
        var = memoized_env_lookup(str)
        len = _getenvlen(var)
        if len == 0
            return Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND ? "" : onError(str)
        end
        val = zeros(UInt16,len)
        ret = ccall(:GetEnvironmentVariableW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32),var,val,len)
        windowserror(:getenv, (ret == 0 && len != 1) || ret != len-1 || val[end] != 0)
        pop!(val) # NUL
        return transcode(String, val)
    end

    function _setenv(svar::AbstractString, sval::AbstractString, overwrite::Bool=true)
        var = memoized_env_lookup(svar)
        val = cwstring(sval)
        if overwrite || !_hasenv(var)
            ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,val)
            windowserror(:setenv, ret == 0)
        end
    end

    function _unsetenv(svar::AbstractString)
        var = memoized_env_lookup(svar)
        ret = ccall(:SetEnvironmentVariableW,stdcall,Int32,(Ptr{UInt16},Ptr{UInt16}),var,C_NULL)
        windowserror(:setenv, ret == 0 && Libc.GetLastError() != ERROR_ENVVAR_NOT_FOUND)
    end
else # !windows
    _getenv(var::AbstractString) = ccall(:getenv, Cstring, (Cstring,), var)
    _hasenv(s::AbstractString) = _getenv(s) != C_NULL

    function access_env(onError::Function, var::AbstractString)
        val = _getenv(var)
        val == C_NULL ? onError(var) : unsafe_string(val)
    end

    function _setenv(var::AbstractString, val::AbstractString, overwrite::Bool=true)
        ret = ccall(:setenv, Int32, (Cstring,Cstring,Int32), var, val, overwrite)
        systemerror(:setenv, ret != 0)
    end

    function _unsetenv(var::AbstractString)
        ret = ccall(:unsetenv, Int32, (Cstring,), var)
        systemerror(:unsetenv, ret != 0)
    end
end # os test

## ENV: hash interface ##

"""
    EnvDict()::EnvDict

A singleton of this type provides a hash table interface to environment variables.
"""
struct EnvDict <: AbstractDict{String,String}; end

"""
    ENV

Reference to the singleton `EnvDict`, providing a dictionary interface to system environment
variables.

(On Windows, system environment variables are case-insensitive, and `ENV` correspondingly converts
all keys to uppercase for display, iteration, and copying. Portable code should not rely on the
ability to distinguish variables by case, and should beware that setting an ostensibly lowercase
variable may result in an uppercase `ENV` key.)

!!! warning
    Mutating the environment is not thread-safe.

# Examples
```julia-repl
julia> ENV
Base.EnvDict with "50" entries:
  "SECURITYSESSIONID"            => "123"
  "USER"                         => "username"
  "MallocNanoZone"               => "0"
  ⋮                              => ⋮

julia> ENV["JULIA_EDITOR"] = "vim"
"vim"

julia> ENV["JULIA_EDITOR"]
"vim"
```

See also: [`withenv`](@ref), [`addenv`](@ref).
"""
const ENV = EnvDict()

const get_bool_env_truthy = (
    "t", "T",
    "true", "True", "TRUE",
    "y", "Y",
    "yes", "Yes", "YES",
    "1")
const get_bool_env_falsy = (
    "f", "F",
    "false", "False", "FALSE",
    "n", "N",
    "no", "No", "NO",
    "0")

"""
    Base.get_bool_env(name::String, default::Bool; throw=false)::Union{Bool,Nothing}
    Base.get_bool_env(f_default::Callable, name::String; throw=false)::Union{Bool,Nothing}

Evaluate whether the value of environment variable `name` is a truthy or falsy string,
and return `nothing` (or throw if `throw=true`) if it is not recognized as either. If
the variable is not set, or is set to "", return `default` or the result of executing `f_default()`.

Recognized values are the following, and their Capitalized and UPPERCASE forms:
    truthy: "t", "true", "y", "yes", "1"
    falsy:  "f", "false", "n", "no", "0"
"""
get_bool_env(name::String, default::Bool; kwargs...) = get_bool_env(Returns(default), name; kwargs...)
function get_bool_env(f_default::Callable, name::String; kwargs...)
    if haskey(ENV, name)
        val = ENV[name]
        if !isempty(val)
            return parse_bool_env(name, val; kwargs...)
        end
    end
    return f_default()
end
function parse_bool_env(name::String, val::String = ENV[name]; throw::Bool=false)
    if val in get_bool_env_truthy
        return true
    elseif val in get_bool_env_falsy
        return false
    elseif throw
        Base.throw(ArgumentError("Value for environment variable `$name` could not be parsed as Boolean: $(repr(val))"))
    else
        return nothing
    end
end

getindex(::EnvDict, k::AbstractString) = access_env(k->throw(KeyError(k)), k)
get(::EnvDict, k::AbstractString, def) = access_env(Returns(def), k)
get(f::Callable, ::EnvDict, k::AbstractString) = access_env(k->f(), k)
function get!(default::Callable, ::EnvDict, k::AbstractString)
    haskey(ENV, k) && return ENV[k]
    ENV[k] = default()
end
in(k::AbstractString, ::KeySet{String, EnvDict}) = _hasenv(k)
pop!(::EnvDict, k::AbstractString) = (v = ENV[k]; _unsetenv(k); v)
pop!(::EnvDict, k::AbstractString, def) = haskey(ENV,k) ? pop!(ENV,k) : def
delete!(::EnvDict, k::AbstractString) = (_unsetenv(k); ENV)
setindex!(::EnvDict, v, k::AbstractString) = _setenv(k,string(v))
push!(::EnvDict, kv::Pair{<:AbstractString}) = setindex!(ENV, kv.second, kv.first)

if Sys.iswindows()
    GESW() = (pos = ccall(:GetEnvironmentStringsW, stdcall, Ptr{UInt16}, ()); (pos, pos))
    function winuppercase(s::AbstractString)
        isempty(s) && return s
        LOCALE_INVARIANT = 0x0000007f
        LCMAP_UPPERCASE  = 0x00000200
        ws = transcode(UInt16, String(s))
        result = ccall(:LCMapStringW, stdcall, Cint, (UInt32, UInt32, Ptr{UInt16}, Cint, Ptr{UInt16}, Cint),
                       LOCALE_INVARIANT, LCMAP_UPPERCASE, ws, length(ws), ws, length(ws))
        systemerror(:LCMapStringW, iszero(result))
        return transcode(String, ws)
    end
    function iterate(hash::EnvDict, block::Tuple{Ptr{UInt16},Ptr{UInt16}} = GESW())
        while true
            if unsafe_load(block[1]) == 0
                ccall(:FreeEnvironmentStringsW, stdcall, Int32, (Ptr{UInt16},), block[2])
                return nothing
            end
            pos = block[1]
            blk = block[2]
            len = ccall(:wcslen, UInt, (Ptr{UInt16},), pos)
            buf = Vector{UInt16}(undef, len)
            GC.@preserve buf unsafe_copyto!(pointer(buf), pos, len)
            env = transcode(String, buf)
            pos += (len + 1) * 2
            if !isempty(env)
                m = findnext('=', env, nextind(env, firstindex(env)))
            else
                m = nothing
            end
            if m === nothing
                @warn "malformed environment entry" env
                continue
            end
            return (Pair{String,String}(winuppercase(env[1:prevind(env, m)]), env[nextind(env, m):end]), (pos, blk))
        end
    end
else # !windows
    function iterate(::EnvDict, i=0)
        while true
            env = ccall(:jl_environ, Any, (Int32,), i)
            env === nothing && return nothing
            env = env::String
            m = findfirst('=', env)
            if m === nothing
                @warn "malformed environment entry" env
                continue
            end
            return (Pair{String,String}(env[1:prevind(env, m)], env[nextind(env, m):end]), i+1)
        end
    end
end # os-test

#TODO: Make these more efficient
function length(::EnvDict)
    i = 0
    for (k,v) in ENV
        i += 1
    end
    return i
end

function show(io::IO, ::EnvDict)
    for (k,v) = ENV
        println(io, "$k=$v")
    end
end

"""
    withenv(f, kv::Pair...)

Execute `f` in an environment that is temporarily modified (not replaced as in `setenv`)
by zero or more `"var"=>val` arguments `kv`. `withenv` is generally used via the
`withenv(kv...) do ... end` syntax. A value of `nothing` can be used to temporarily unset an
environment variable (if it is set). When `withenv` returns, the original environment has
been restored.

!!! warning
    Changing the environment is not thread-safe. For running external commands with a different
    environment from the parent process, prefer using [`addenv`](@ref) over `withenv`.
"""
function withenv(f, keyvals::Pair{T}...) where T<:AbstractString
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
withenv(f) = f() # handle empty keyvals case; see #10853
