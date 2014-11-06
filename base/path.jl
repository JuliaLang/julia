@unix_only begin
    const path_separator    = "/"
    const path_separator_re = r"/+"
    const path_absolute_re  = r"^/"
    const path_directory_re = r"(?:^|/)\.{0,2}$"
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"

    splitdrive(path::AbstractString) = ("",path)
    homedir() = ENV["HOME"]
end
@windows_only begin
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"
    const path_absolute_re  = r"^(?:\w+:)?[/\\]"
    const path_directory_re = r"(?:^|[/\\])\.{0,2}$"
    const path_dir_splitter = r"^(.*?)([/\\]+)([^/\\]*)$"
    const path_ext_splitter = r"^((?:.*[/\\])?(?:\.|[^/\\\.])[^/\\]*?)(\.[^/\\\.]*|)$"

    function splitdrive(path::AbstractString)
        m = match(r"^(\w+:|\\\\\w+\\\w+|\\\\\?\\UNC\\\w+\\\w+|\\\\\?\\\w+:|)(.*)$", path)
        bytestring(m.captures[1]), bytestring(m.captures[2])
    end
    homedir() = get(ENV,"HOME",string(ENV["HOMEDRIVE"],ENV["HOMEPATH"]))
end

isabspath(path::AbstractString) = ismatch(path_absolute_re, path)
isdirpath(path::AbstractString) = ismatch(path_directory_re, splitdrive(path)[2])

function splitdir(path::ByteString)
    a, b = splitdrive(path)
    m = match(path_dir_splitter,b)
    m == nothing && return (a,b)
    a = string(a, isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1])
    a, bytestring(m.captures[3])
end
splitdir(path::AbstractString) = splitdir(bytestring(path))

 dirname(path::AbstractString) = splitdir(path)[1]
basename(path::AbstractString) = splitdir(path)[2]

function splitext(path::AbstractString)
    a, b = splitdrive(path)
    m = match(path_ext_splitter, b)
    m == nothing && return (path,"")
    a*m.captures[1], bytestring(m.captures[2])
end

function pathsep(paths::AbstractString...)
    for path in paths
        m = match(path_separator_re, path)
        m != nothing && return m.match[1]
    end
    return path_separator
end

joinpath(a::AbstractString) = a
joinpath(a::AbstractString, b::AbstractString, c::AbstractString...) = joinpath(joinpath(a,b), c...)

function joinpath(a::AbstractString, b::AbstractString)
    isabspath(b) && return b
    A, a = splitdrive(a)
    B, b = splitdrive(b)
    !isempty(B) && A != B && error("drive mismatch: $A$a $B$b")
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

@windows_only realpath(path::AbstractString) = realpath(utf16(path))
@windows_only function realpath(path::UTF16String)
    p = uint32((sizeof(path)>>2) + 1)
    while true
        buflength = p
        buf = zeros(UInt16,buflength)
        p = ccall((:GetFullPathNameW, "Kernel32"), stdcall,
            UInt32, (Ptr{UInt16}, UInt32, Ptr{UInt16}, Ptr{Void}),
            path, buflength, buf, C_NULL)
        systemerror(:realpath, p == 0)
        if (p < buflength)
            resize!(buf, p+1)
            return utf8(UTF16String(buf))
        end
    end
end

@unix_only function realpath(path::AbstractString)
    p = ccall(:realpath, Ptr{UInt8}, (Ptr{UInt8}, Ptr{UInt8}), path, C_NULL)
    systemerror(:realpath, p == C_NULL)
    s = bytestring(p)
    c_free(p)
    return s
end

@windows_only expanduser(path::AbstractString) = path # on windows, ~ means "temporary file"
@unix_only function expanduser(path::AbstractString)
    i = start(path)
    c, i = next(path,i)
    if c != '~' return path end
    if done(path,i) return homedir() end
    c, j = next(path,i)
    if c == '/' return homedir()*path[i:end] end
    error("~user tilde expansion not yet implemented")
end
