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
    ENV
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

type ConfigHashItem
    value::Any
    hasvalue::Bool
    default::Any
    hasdefault::Bool
    getter::Callback # (key,value) -> value
    setter::Callback # (key,value,hadvalue,newvalue)
    delete::Callback # (key,value)
    ConfigHashItem(v::ANY, hasvalue::Bool, def::ANY, hasdefault::Bool,
            getter::Callback, setter::Callback,
            delete::Callback) =
        new(v,hasvalue,def,hasdefault,getter,setter,delete)
    ConfigHashItem(v::ANY) = new(v,true,nothing,false,false,false,false)
end
setindex!(chi::ConfigHashItem, v::ANY, k::String) =
    setindex!(chi, v, k, :value)
function setindex!(chi::ConfigHashItem, v::ANY, k::String, what::Symbol)
    value = v
    if isa(chi.setter, Function) && !(what == :default && chi.hasvalue)
        if chi.hasvalue
            value = chi.value
            hasvalue = true
        elseif chi.hasdefault
            value = chi.default
            hasvalue = true
        else
            value = nothing
            hasvalue = false
        end
        chi.setter(k, value, hasvalue, v)
    end
    if what == :default
        chi.default = v
        chi.hasdefault = true
    else
        chi.value = v
        chi.hasvalue = true
    end
    chi
end
delete!(chi::ConfigHashItem, k::String) = 
    delete!(chi, k, :value)
function delete!(chi::ConfigHashItem, k::String, what::Symbol)
    if what == :default
        if !chi.hasdefault
            error("Key $k doesn't have a default")
        end
    elseif !chi.hasvalue
        error("Key $k doesn't have a value")
    end
    v = getindex(chi, k)
    if isa(chi.delete, Function)
        if what == :default
            if !chi.hasvalue
                chi.delete(k, chi.default)
            end
        else
            chi.delete(k, chi.value)
        end
    end
    if what == :default
        chi.hasdefault = false
    else
        chi.hasvalue = false
        if isa(chi.setter, Function) && chi.hasdefault
            chi.setter(k, chi.value, true, chi.default)
        end
    end
    return v
end
function getindex(chi::ConfigHashItem, k::String)
    if chi.hasvalue
        value = chi.value
    elseif chi.hasdefault
        value = chi.default
    else
        error("Key $k doesn't have a value")
    end
    if isa(chi.getter, Function)
        return chi.getter(k, value)
    end
    return value
end

type ConfigHash <: Associative{String,Any}
    dict::Associative{String,ConfigHashItem}
    ConfigHash() = new(Dict{String,ConfigHashItem}())
end
const CONFIG = ConfigHash()

function getindex(ch::ConfigHash, k::String)
    return ch.dict[k][k]
end
function get(ch::ConfigHash, k::String, def)
    item = get(ch.dict, k, nothing)
    if isa(item,ConfigHashItem)
        return item[k]
    end
    return def
end
function has(ch::ConfigHash, k::String)
    item = get(ch.dict, k, nothing)
    if isa(item,ConfigHashItem)
        if item.hasvalue || item.hasdefault
            return true
        end
        return false
    end
    return false
end
function delete!(ch::ConfigHash, k::String)
    item = ch.dict[k]
    delete!(item, k)
end
function delete!(ch::ConfigHash, k::String, what::Symbol...)
    # example usage:
    # delete!(CONFIG, "hamster", :value, :entry)
    item = ch.dict[k]
    ret = nothing
    for x in what
        if x == :value || x == :default
            ret = delete!(item, k, x)
        elseif x == :getter
            item.getter = false
        elseif x == :setter
            item.setter = false
        elseif x == :delete
            item.delete = false
        elseif x == :entry
            delete!(ch.dict, k)
        else
            error("Invalid attribute type $x for ConfigHash")
        end
    end
    return ret
end
delete!(ch::ConfigHash, k::String, def) = has(ch.dict,k) ? delete!(ch,k) : def
function setindex!(ch::ConfigHash, v, k::String)
    item = get(ch.dict, k, nothing)
    if isa(item,ConfigHashItem)
        item[k] = v
    else
        ch.dict[k] = ConfigHashItem(v)
    end
    return ch
end
function setindex!(ch::ConfigHash, values, k::String, what::Symbol...)
    # example usage:
    # CONFIG["hamster",:default,:getter,:setter] = ("pet", getter_f, setter_f)
    if !isa(values,Tuple)
        values = (values,)
    end
    v::Tuple = values
    if length(v) != length(what)
        error("Mismatch in assignment to ConfigHash")
    end
    itm = get(ch.dict, k, nothing)
    if !isa(itm,ConfigHashItem)
        itm = ConfigHashItem(nothing)
        itm.hasvalue = false
        ch.dict[k] = itm
    end
    item::ConfigHashItem = itm
    for i = 1:length(v)
        x = what[i]
        value = v[i]
        if x == :value || x == :default
            item[k, x] = value
        elseif x == :getter
            item.getter = value
        elseif x == :setter
            item.setter = value
            if item.hasvalue
                item.setter(k, nothing, false, item.value)
            elseif item.hasdefault
                item.setter(k, nothing, false, item.default)
            end
        elseif x == :delete
            item.delete = value
        else
            error("Invalid attribute type $x for ConfigHash")
        end
    end
    return ch
end
#start(ch::ConfigHash) = start(ch.dict)
#done(ch::ConfigHash, i) = done(ch.dict, i)
#function next(ch::ConfigHash, i)
#    ((k,item),i) = next(ch.dict, i)
#    if isa(item.getter, Function)
#        return (item.getter(k, item.value), i)
#    end
#    return (item.value, i)
#end
#length(ch::ConfigHash) = length(ch.dict)
function show(io::IO, ch::ConfigHash)
    println("Julia Configuration:")
    ks = sort!(keys(ch.dict))
    for k in ks
        item = getindex(ch.dict, k)
        if item.hasvalue || item.hasdefault
            v = getindex(item, k)
            print(io, "$k = ")
            show(io, v)
            println(io)
        else
            println(io, "$k = <no value>")
        end
    end
end

CONFIG["OUTPUT_STREAM", :default, :setter] =
    (STDOUT,
    (key,value,hadvalue,newvalue::IO) -> global OUTPUT_STREAM = newvalue)
CONFIG["gfx/backend", :default] = "gtk"

