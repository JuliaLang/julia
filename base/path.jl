# This file is a part of Julia. License is MIT: http://julialang.org/license

export
    abspath,
    basename,
    dirname,
    expanduser,
    homedir,
    isabspath,
    isdirpath,
    joinpath,
    normpath,
    realpath,
    relpath,
    splitdir,
    splitdrive,
    splitext

if is_unix()
    const path_separator    = "/"
    const path_separator_re = r"/+"
    const path_absolute_re  = r"^/"
    const path_directory_re = r"(?:^|/)\.{0,2}$"
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"

    splitdrive(path::AbstractString) = ("",path)
    homedir() = ENV["HOME"]
elseif is_windows()
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"
    const path_absolute_re  = r"^(?:\w+:)?[/\\]"
    const path_directory_re = r"(?:^|[/\\])\.{0,2}$"
    const path_dir_splitter = r"^(.*?)([/\\]+)([^/\\]*)$"
    const path_ext_splitter = r"^((?:.*[/\\])?(?:\.|[^/\\\.])[^/\\]*?)(\.[^/\\\.]*|)$"

    function splitdrive(path::AbstractString)
        m = match(r"^(\w+:|\\\\\w+\\\w+|\\\\\?\\UNC\\\w+\\\w+|\\\\\?\\\w+:|)(.*)$", path)
        String(m.captures[1]), String(m.captures[2])
    end
    homedir() = get(ENV,"HOME",string(ENV["HOMEDRIVE"],ENV["HOMEPATH"]))
else
    error("path primitives for this OS need to be defined")
end

isabspath(path::AbstractString) = ismatch(path_absolute_re, path)
isdirpath(path::AbstractString) = ismatch(path_directory_re, splitdrive(path)[2])

function splitdir(path::String)
    a, b = splitdrive(path)
    m = match(path_dir_splitter,b)
    m === nothing && return (a,b)
    a = string(a, isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1])
    a, String(m.captures[3])
end
splitdir(path::AbstractString) = splitdir(String(path))

 dirname(path::AbstractString) = splitdir(path)[1]
basename(path::AbstractString) = splitdir(path)[2]

function splitext(path::AbstractString)
    a, b = splitdrive(path)
    m = match(path_ext_splitter, b)
    m === nothing && return (path,"")
    a*m.captures[1], String(m.captures[2])
end

function pathsep(paths::AbstractString...)
    for path in paths
        m = match(path_separator_re, path)
        m !== nothing && return m.match[1:1]
    end
    return path_separator
end

joinpath(a::AbstractString) = a
joinpath(a::AbstractString, b::AbstractString, c::AbstractString...) = joinpath(joinpath(a,b), c...)

function joinpath(a::AbstractString, b::AbstractString)
    isabspath(b) && return b
    A, a = splitdrive(a)
    B, b = splitdrive(b)
    !isempty(B) && A != B && throw(ArgumentError("drive mismatch: $A$a $B$b"))
    C = isempty(B) ? A : B
    isempty(a)                             ? string(C,b) :
    ismatch(path_separator_re, a[end:end]) ? string(C,a,b) :
                                             string(C,a,pathsep(a,b),b)
end

function normpath(path::AbstractString)
    isabs = isabspath(path)
    isdir = isdirpath(path)
    drive, path = splitdrive(path)
    parts = split(path, path_separator_re)
    filter!(x->!isempty(x) && x!=".", parts)
    while true
        clean = true
        for j = 1:length(parts)-1
            if parts[j] != ".." && parts[j+1] == ".."
                deleteat!(parts, j:j+1)
                clean = false
                break
            end
        end
        clean && break
    end
    if isabs
        while !isempty(parts) && parts[1] == ".."
            shift!(parts)
        end
    elseif isempty(parts)
        push!(parts, ".")
    end
    path = join(parts, path_separator)
    if isabs
        path = path_separator*path
    end
    if isdir && !isdirpath(path)
        path *= path_separator
    end
    string(drive,path)
end
normpath(a::AbstractString, b::AbstractString...) = normpath(joinpath(a,b...))

abspath(a::AbstractString) = normpath(isabspath(a) ? a : joinpath(pwd(),a))
abspath(a::AbstractString, b::AbstractString...) = abspath(joinpath(a,b...))

if is_windows()
function realpath(path::AbstractString)
    path = cwstring(path)
    buf = zeros(UInt16, length(path))
    while true
        n = ccall((:GetFullPathNameW, "kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, UInt32, Ptr{UInt16}, Ptr{Void}),
            path, length(buf), buf, C_NULL)
        systemerror(:realpath, n == 0)
        x = n < length(buf) # is the buffer big enough?
        resize!(buf, n) # shrink if x, grow if !x
        x && return String(transcode(UInt8, buf))
    end
end

function longpath(path::AbstractString)
    path = cwstring(path)
    buf = zeros(UInt16, length(path))
    while true
        n = ccall((:GetLongPathNameW, "kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, Ptr{UInt16}, UInt32),
            path, buf, length(buf))
        systemerror(:longpath, n == 0)
        x = n < length(buf) # is the buffer big enough?
        resize!(buf, n) # shrink if x, grow if !x
        x && return String(transcode(UInt8, buf))
    end
end

else # !windows
function realpath(path::AbstractString)
    p = ccall(:realpath, Ptr{UInt8}, (Cstring, Ptr{UInt8}), path, C_NULL)
    systemerror(:realpath, p == C_NULL)
    return unsafe_wrap(String, p, true)
end
end # os-test

if is_windows()
expanduser(path::AbstractString) = path # on windows, ~ means "temporary file"
else
function expanduser(path::AbstractString)
    i = start(path)
    c, i = next(path,i)
    if c != '~' return path end
    if done(path,i) return homedir() end
    c, j = next(path,i)
    if c == '/' return homedir()*path[i:end] end
    throw(ArgumentError("~user tilde expansion not yet implemented"))
end
end

function relpath(path::AbstractString, startpath::AbstractString = ".")
    isempty(path) && throw(ArgumentError("`path` must be specified"))
    isempty(startpath) && throw(ArgumentError("`startpath` must be specified"))
    curdir = "."
    pardir = ".."
    path == startpath && return curdir
    path_arr  = split(abspath(path),      path_separator_re)
    start_arr = split(abspath(startpath), path_separator_re)
    i = 0
    while i < min(length(path_arr), length(start_arr))
        i += 1
        if path_arr[i] != start_arr[i]
            i -= 1
            break
        end
    end
    pathpart = join(path_arr[i+1:findlast(x -> !isempty(x), path_arr)], path_separator)
    prefix_num = findlast(x -> !isempty(x), start_arr) - i - 1
    if prefix_num >= 0
        prefix = pardir * path_separator
        relpath_ = isempty(pathpart)     ?
            (prefix^prefix_num) * pardir :
            (prefix^prefix_num) * pardir * path_separator * pathpart
    else
        relpath_ = pathpart
    end
    return isempty(relpath_) ? curdir :  relpath_
end
