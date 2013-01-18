@unix_only begin
    const path_separator    = "/"
    const path_separator_re = r"/+"
    const path_absolute_re  = r"^/"
    const path_directory_re = r"(?:^|/)\.{0,2}$"
    const path_dir_splitter = r"^(.*?)(/+)([^/]*)$"
    const path_ext_splitter = r"^((?:.*/)?(?:\.|[^/\.])[^/]*?)(\.[^/\.]*|)$"

    splitdrive(path::String) = ("",path)
end
@windows_only begin
    const path_separator    = "\\"
    const path_separator_re = r"[/\\]+"
    const path_absolute_re  = r"^(?:\w+:)[/\\]"
    const path_directory_re = r"(?:^|[/\\])\.{0,2}$"
    const path_dir_splitter = r"^(.*?)([/\\]+)([^/\\]*)$"
    const path_ext_splitter = r"^((?:.*[/\\])?(?:\.|[^/\\\.])[^/\\]*?)(\.[^/\\\.]*|)$"

    function splitdrive(path::String)
        m = match(r"^(\w+:|\\\\\w+\\\w+|\\\\\?\\UNC\\\w+\\\w+|\\\\\?\\\w+:|)(.*)$", path)
        m.captures[1], m.captures[2]
    end
end

isabspath(path::String) = ismatch(path_absolute_re, path)
isdirpath(path::String) = ismatch(path_directory_re, splitdrive(path)[2])

function splitdir(path::ByteString)
    a, b = splitdrive(path)
    m = match(path_dir_splitter,b)
    m == nothing && return (a,b)
    a *= isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1]
    a, m.captures[3]
end
splitdir(path::String) = splitdir(bytestring(path))

 dirname(path::String) = splitdir(path)[1]
basename(path::String) = splitdir(path)[2]

function splitext(path::String)
    a, b = splitdrive(path)
    m = match(path_ext_splitter, b)
    m == nothing && return (path,"")
    a*m.captures[1], m.captures[2]
end

function pathsep(paths::String...)
    for path in paths
        m = match(path_separator_re, path)
        m != nothing && return m.match[1]
    end
    return path_separator
end

joinpath(a::String) = a
joinpath(a::String, b::String, c::String...) = joinpath(joinpath(a,b), c...)

function joinpath(a::String, b::String)
    isabspath(b) && return b
    A,a = splitdrive(a)
    B,b = splitdrive(b)
    !isempty(B) && A != B && error("drive mismatch: $A$a $B$b")
    C = isempty(B) ? A : B
    isempty(a)                             ? strcat(C,b) :
    ismatch(path_separator_re, a[end:end]) ? strcat(C,a,b) :
                                             strcat(C,a,pathsep(a,b),b)
end

function normpath(path::String)
    isabs = isabspath(path)
    isdir = isdirpath(path)
    drive, path = splitdrive(path)
    parts = split(path, path_separator_re)
    parts = filter(x->!isempty(x) && x!=".", parts)
    while true
        clean = true
        for j = 1:length(parts)-1
            if parts[j] != ".." && parts[j+1] == ".."
                delete!(parts, j:j+1)
                clean = false
                break
            end
        end
        clean && break
    end
    if isabs
        while isabs && !isempty(parts) && parts[1] == ".."
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
    strcat(drive,path)
end
normpath(a::String, b::String...) = normpath(joinpath(a,b...))

abspath(a::String) = normpath(isabspath(a) ? a : joinpath(pwd(),a))
abspath(a::String, b::String...) = abspath(joinpath(a,b...))

function realpath(path::String)
    p = ccall(:realpath, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}), path, C_NULL)
    system_error(:realpath, p == C_NULL)
    s = bytestring(p)
    c_free(p)
    return s
end

@windows_only expanduser(path::String) = path # on windows, ~ means "temporary file"
@unix_only function expanduser(path::String)
    i = start(path)
    c, i = next(path,i)
    if c != '~' return path end
    if done(path,i) return ENV["HOME"] end
    c, j = next(path,i)
    if c == '/' return ENV["HOME"]*path[i:end] end
    error("~user tilde expansion not yet implemented")
end
