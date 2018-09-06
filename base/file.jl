# This file is a part of Julia. License is MIT: https://julialang.org/license

# Operations with the file system (paths) ##

export
    cd,
    chmod,
    chown,
    cp,
    cptree,
    mkdir,
    mkpath,
    mktemp,
    mktempdir,
    mv,
    pwd,
    rename,
    readlink,
    readdir,
    rm,
    samefile,
    sendfile,
    symlink,
    tempdir,
    tempname,
    touch,
    unlink,
    walkdir

import .Base.RefValue

# get and set current directory

"""
    pwd() -> AbstractString

Get the current working directory.

# Examples
```julia-repl
julia> pwd()
"/home/JuliaUser"

julia> cd("/home/JuliaUser/Projects/julia")

julia> pwd()
"/home/JuliaUser/Projects/julia"
```
"""
function pwd()
    b = Vector{UInt8}(undef, 1024)
    len = RefValue{Csize_t}(length(b))
    uv_error(:getcwd, ccall(:uv_cwd, Cint, (Ptr{UInt8}, Ptr{Csize_t}), b, len))
    String(b[1:len[]])
end

"""
    cd(dir::AbstractString=homedir())

Set the current working directory.

# Examples
```julia-repl
julia> cd("/home/JuliaUser/Projects/julia")

julia> pwd()
"/home/JuliaUser/Projects/julia"

julia> cd()

julia> pwd()
"/home/JuliaUser"
```
"""
function cd(dir::AbstractString)
    uv_error("chdir $dir", ccall(:uv_chdir, Cint, (Cstring,), dir))
end
cd() = cd(homedir())

if Sys.iswindows()
    function cd(f::Function, dir::AbstractString)
        old = pwd()
        try
            cd(dir)
            f()
       finally
            cd(old)
        end
    end
else
    function cd(f::Function, dir::AbstractString)
        fd = ccall(:open, Int32, (Cstring, Int32), :., 0)
        systemerror(:open, fd == -1)
        try
            cd(dir)
            f()
        finally
            systemerror(:fchdir, ccall(:fchdir, Int32, (Int32,), fd) != 0)
            systemerror(:close, ccall(:close, Int32, (Int32,), fd) != 0)
        end
    end
end
"""
    cd(f::Function, dir::AbstractString=homedir())

Temporarily change the current working directory to `dir`, apply function `f` and
finally return to the original directory.

# Examples
```julia-repl
julia> pwd()
"/home/JuliaUser"

julia> cd(readdir, "/home/JuliaUser/Projects/julia")
34-element Array{String,1}:
 ".circleci"
 ".freebsdci.sh"
 ".git"
 ".gitattributes"
 ".github"
 ⋮
 "test"
 "ui"
 "usr"
 "usr-staging"

julia> pwd()
"/home/JuliaUser"
```
"""
cd(f::Function) = cd(f, homedir())

function checkmode(mode::Integer)
    if !(0 <= mode <= 511)
        throw(ArgumentError("Mode must be between 0 and 511 = 0o777"))
    end
    mode
end

"""
    mkdir(path::AbstractString; mode::Unsigned = 0o777)

Make a new directory with name `path` and permissions `mode`. `mode` defaults to `0o777`,
modified by the current file creation mask. This function never creates more than one
directory. If the directory already exists, or some intermediate directories do not exist,
this function throws an error. See [`mkpath`](@ref) for a function which creates all
required intermediate directories.
Return `path`.

# Examples
```julia-repl
julia> mkdir("testingdir")
"testingdir"

julia> cd("testingdir")

julia> pwd()
"/home/JuliaUser/testingdir"
```
"""
function mkdir(path::AbstractString; mode::Integer = 0o777)
    @static if Sys.iswindows()
        ret = ccall(:_wmkdir, Int32, (Cwstring,), path)
    else
        ret = ccall(:mkdir, Int32, (Cstring, UInt32), path, checkmode(mode))
    end
    systemerror(:mkdir, ret != 0; extrainfo=path)
    path
end

"""
    mkpath(path::AbstractString; mode::Unsigned = 0o777)

Create all directories in the given `path`, with permissions `mode`. `mode` defaults to
`0o777`, modified by the current file creation mask.
Return `path`.

# Examples
```julia-repl
julia> mkdir("testingdir")
"testingdir"

julia> cd("testingdir")

julia> pwd()
"/home/JuliaUser/testingdir"

julia> mkpath("my/test/dir")
"my/test/dir"

julia> readdir()
1-element Array{String,1}:
 "my"

julia> cd("my")

julia> readdir()
1-element Array{String,1}:
 "test"

julia> readdir("test")
1-element Array{String,1}:
 "dir"
```
"""
function mkpath(path::AbstractString; mode::Integer = 0o777)
    isdirpath(path) && (path = dirname(path))
    dir = dirname(path)
    (path == dir || isdir(path)) && return
    mkpath(dir, mode = checkmode(mode))
    try
        mkdir(path, mode = mode)
    # If there is a problem with making the directory, but the directory
    # does in fact exist, then ignore the error. Else re-throw it.
    catch err
        if !isa(err, SystemError) || !isdir(path)
            rethrow()
        end
    end
    path
end

"""
    rm(path::AbstractString; force::Bool=false, recursive::Bool=false)

Delete the file, link, or empty directory at the given path. If `force=true` is passed, a
non-existing path is not treated as error. If `recursive=true` is passed and the path is a
directory, then all contents are removed recursively.

# Examples
```jldoctest
julia> mkpath("my/test/dir");

julia> rm("my", recursive=true)

julia> rm("this_file_does_not_exist", force=true)

julia> rm("this_file_does_not_exist")
ERROR: IOError: unlink: no such file or directory (ENOENT)
Stacktrace:
[...]
```
"""
function rm(path::AbstractString; force::Bool=false, recursive::Bool=false)
    if islink(path) || !isdir(path)
        try
            @static if Sys.iswindows()
                # is writable on windows actually means "is deletable"
                if (filemode(path) & 0o222) == 0
                    chmod(path, 0o777)
                end
            end
            unlink(path)
        catch err
            if force && isa(err, IOError) && err.code==Base.UV_ENOENT
                return
            end
            rethrow()
        end
    else
        if recursive
            for p in readdir(path)
                rm(joinpath(path, p), force=force, recursive=true)
            end
        end
        @static if Sys.iswindows()
            ret = ccall(:_wrmdir, Int32, (Cwstring,), path)
        else
            ret = ccall(:rmdir, Int32, (Cstring,), path)
        end
        systemerror(:rmdir, ret != 0, extrainfo=path)
    end
end


# The following use Unix command line facilities
function checkfor_mv_cp_cptree(src::AbstractString, dst::AbstractString, txt::AbstractString;
                                                          force::Bool=false)
    if ispath(dst)
        if force
            # Check for issue when: (src == dst) or when one is a link to the other
            # https://github.com/JuliaLang/julia/pull/11172#issuecomment-100391076
            if Base.samefile(src, dst)
                abs_src = islink(src) ? abspath(readlink(src)) : abspath(src)
                abs_dst = islink(dst) ? abspath(readlink(dst)) : abspath(dst)
                throw(ArgumentError(string("'src' and 'dst' refer to the same file/dir.",
                                           "This is not supported.\n  ",
                                           "`src` refers to: $(abs_src)\n  ",
                                           "`dst` refers to: $(abs_dst)\n")))
            end
            rm(dst; recursive=true)
        else
            throw(ArgumentError(string("'$dst' exists. `force=true` ",
                                       "is required to remove '$dst' before $(txt).")))
        end
    end
end

function cptree(src::AbstractString, dst::AbstractString; force::Bool=false,
                                                          follow_symlinks::Bool=false)
    isdir(src) || throw(ArgumentError("'$src' is not a directory. Use `cp(src, dst)`"))
    checkfor_mv_cp_cptree(src, dst, "copying"; force=force)
    mkdir(dst)
    for name in readdir(src)
        srcname = joinpath(src, name)
        if !follow_symlinks && islink(srcname)
            symlink(readlink(srcname), joinpath(dst, name))
        elseif isdir(srcname)
            cptree(srcname, joinpath(dst, name); force=force,
                                                 follow_symlinks=follow_symlinks)
        else
            sendfile(srcname, joinpath(dst, name))
        end
    end
end

"""
    cp(src::AbstractString, dst::AbstractString; force::Bool=false, follow_symlinks::Bool=false)

Copy the file, link, or directory from `src` to `dst`.
`force=true` will first remove an existing `dst`.

If `follow_symlinks=false`, and `src` is a symbolic link, `dst` will be created as a
symbolic link. If `follow_symlinks=true` and `src` is a symbolic link, `dst` will be a copy
of the file or directory `src` refers to.
Return `dst`.
"""
function cp(src::AbstractString, dst::AbstractString; force::Bool=false,
                                                      follow_symlinks::Bool=false)
    checkfor_mv_cp_cptree(src, dst, "copying"; force=force)
    if !follow_symlinks && islink(src)
        symlink(readlink(src), dst)
    elseif isdir(src)
        cptree(src, dst; force=force, follow_symlinks=follow_symlinks)
    else
        sendfile(src, dst)
    end
    dst
end

"""
    mv(src::AbstractString, dst::AbstractString; force::Bool=false)

Move the file, link, or directory from `src` to `dst`.
`force=true` will first remove an existing `dst`.
Return `dst`.

# Examples
```jldoctest; filter = r"Stacktrace:(\\n \\[[0-9]+\\].*)*"
julia> write("hello.txt", "world");

julia> mv("hello.txt", "goodbye.txt")
"goodbye.txt"

julia> "hello.txt" in readdir()
false

julia> readline("goodbye.txt")
"world"

julia> write("hello.txt", "world2");

julia> mv("hello.txt", "goodbye.txt")
ERROR: ArgumentError: 'goodbye.txt' exists. `force=true` is required to remove 'goodbye.txt' before moving.
Stacktrace:
 [1] #checkfor_mv_cp_cptree#10(::Bool, ::Function, ::String, ::String, ::String) at ./file.jl:293
[...]

julia> mv("hello.txt", "goodbye.txt", force=true)
"goodbye.txt"

julia> rm("goodbye.txt");

```
"""
function mv(src::AbstractString, dst::AbstractString; force::Bool=false)
    checkfor_mv_cp_cptree(src, dst, "moving"; force=force)
    rename(src, dst)
    dst
end

"""
    touch(path::AbstractString)

Update the last-modified timestamp on a file to the current time.
Return `path`.

# Examples
```julia-repl
julia> write("my_little_file", 2);

julia> mtime("my_little_file")
1.5273815391135583e9

julia> touch("my_little_file");

julia> mtime("my_little_file")
1.527381559163435e9
```

We can see the [`mtime`](@ref) has been modified by `touch`.
"""
function touch(path::AbstractString)
    f = open(path, JL_O_WRONLY | JL_O_CREAT, 0o0666)
    try
        t = time()
        futime(f,t,t)
    finally
        close(f)
    end
    path
end

if Sys.iswindows()

function tempdir()
    temppath = Vector{UInt16}(undef, 32767)
    lentemppath = ccall(:GetTempPathW,stdcall,UInt32,(UInt32,Ptr{UInt16}),length(temppath),temppath)
    if lentemppath >= length(temppath) || lentemppath == 0
        error("GetTempPath failed: $(Libc.FormatMessage())")
    end
    resize!(temppath,lentemppath)
    return transcode(String, temppath)
end

const temp_prefix = cwstring("jl_")
function _win_tempname(temppath::AbstractString, uunique::UInt32)
    tempp = cwstring(temppath)
    tname = Vector{UInt16}(undef, 32767)
    uunique = ccall(:GetTempFileNameW,stdcall,UInt32,(Ptr{UInt16},Ptr{UInt16},UInt32,Ptr{UInt16}), tempp,temp_prefix,uunique,tname)
    lentname = something(findfirst(iszero,tname), 0)-1
    if uunique == 0 || lentname <= 0
        error("GetTempFileName failed: $(Libc.FormatMessage())")
    end
    resize!(tname,lentname)
    return transcode(String, tname)
end

function mktemp(parent=tempdir())
    filename = _win_tempname(parent, UInt32(0))
    return (filename, Base.open(filename, "r+"))
end

function mktempdir(parent=tempdir())
    seed::UInt32 = Libc.rand(UInt32)
    while true
        if (seed & typemax(UInt16)) == 0
            seed += 1
        end
        filename = _win_tempname(parent, seed)
        ret = ccall(:_wmkdir, Int32, (Ptr{UInt16},), cwstring(filename))
        if ret == 0
            return filename
        end
        systemerror(:mktempdir, Libc.errno()!=Libc.EEXIST)
        seed += 1
    end
end

function tempname()
    parent = tempdir()
    seed::UInt32 = rand(UInt32)
    while true
        if (seed & typemax(UInt16)) == 0
            seed += 1
        end
        filename = _win_tempname(parent, seed)
        if !ispath(filename)
            return filename
        end
        seed += 1
    end
end

else # !windows
# Obtain a temporary filename.
function tempname()
    d = get(ENV, "TMPDIR", C_NULL) # tempnam ignores TMPDIR on darwin
    p = ccall(:tempnam, Cstring, (Cstring,Cstring), d, :julia)
    systemerror(:tempnam, p == C_NULL)
    s = unsafe_string(p)
    Libc.free(p)
    return s
end

# Obtain a temporary directory's path.
tempdir() = dirname(tempname())

# Create and return the name of a temporary file along with an IOStream
function mktemp(parent=tempdir())
    b = joinpath(parent, "tmpXXXXXX")
    p = ccall(:mkstemp, Int32, (Cstring,), b) # modifies b
    systemerror(:mktemp, p == -1)
    return (b, fdio(p, true))
end

# Create and return the name of a temporary directory
function mktempdir(parent=tempdir())
    b = joinpath(parent, "tmpXXXXXX")
    p = ccall(:mkdtemp, Cstring, (Cstring,), b)
    systemerror(:mktempdir, p == C_NULL)
    return unsafe_string(p)
end

end # os-test


"""
    tempdir()

Obtain the path of a temporary directory (possibly shared with other processes).
"""
tempdir()

"""
    tempname()

Generate a temporary file path. This function only returns a path; no file is
created. The path is likely to be unique, but this cannot be guaranteed.

!!! warning

    This can lead to race conditions if another process obtains the same
    file name and creates the file before you are able to.
    Using [`mktemp()`](@ref) is recommended instead.
"""
tempname()

"""
    mktemp(parent=tempdir())

Return `(path, io)`, where `path` is the path of a new temporary file in `parent` and `io`
is an open file object for this path.
"""
mktemp(parent)

"""
    mktempdir(parent=tempdir())

Create a temporary directory in the `parent` directory and return its path.
If `parent` does not exist, throw an error.
"""
mktempdir(parent)


"""
    mktemp(f::Function, parent=tempdir())

Apply the function `f` to the result of [`mktemp(parent)`](@ref) and remove the
temporary file upon completion.
"""
function mktemp(fn::Function, parent=tempdir())
    (tmp_path, tmp_io) = mktemp(parent)
    try
        fn(tmp_path, tmp_io)
    finally
        # TODO: should we call GC.gc() first on error, to make it much more likely that `rm` succeeds?
        try
            close(tmp_io)
            rm(tmp_path)
        catch ex
            @error "mktemp cleanup" _group=:file exception=(ex, catch_backtrace())
        end
    end
end

"""
    mktempdir(f::Function, parent=tempdir())

Apply the function `f` to the result of [`mktempdir(parent)`](@ref) and remove the
temporary directory upon completion.
"""
function mktempdir(fn::Function, parent=tempdir())
    tmpdir = mktempdir(parent)
    try
        fn(tmpdir)
    finally
        # TODO: should we call GC.gc() first on error, to make it much more likely that `rm` succeeds?
        try
            rm(tmpdir, recursive=true)
        catch ex
            @error "mktempdir cleanup" _group=:file exception=(ex, catch_backtrace())
        end
    end
end

struct uv_dirent_t
    name::Ptr{UInt8}
    typ::Cint
end

"""
    readdir(dir::AbstractString=".") -> Vector{String}

Return the files and directories in the directory `dir` (or the current working directory if not given).

# Examples
```julia-repl
julia> readdir("/home/JuliaUser/Projects/julia")
34-element Array{String,1}:
 ".circleci"
 ".freebsdci.sh"
 ".git"
 ".gitattributes"
 ".github"
 ⋮
 "test"
 "ui"
 "usr"
 "usr-staging"
```
"""
function readdir(path::AbstractString)
    # Allocate space for uv_fs_t struct
    uv_readdir_req = zeros(UInt8, ccall(:jl_sizeof_uv_fs_t, Int32, ()))

    # defined in sys.c, to call uv_fs_readdir, which sets errno on error.
    err = ccall(:uv_fs_scandir, Int32, (Ptr{Cvoid}, Ptr{UInt8}, Cstring, Cint, Ptr{Cvoid}),
                eventloop(), uv_readdir_req, path, 0, C_NULL)
    err < 0 && throw(SystemError("unable to read directory $path", -err))
    #uv_error("unable to read directory $path", err)

    # iterate the listing into entries
    entries = String[]
    ent = Ref{uv_dirent_t}()
    while Base.UV_EOF != ccall(:uv_fs_scandir_next, Cint, (Ptr{Cvoid}, Ptr{uv_dirent_t}), uv_readdir_req, ent)
        push!(entries, unsafe_string(ent[].name))
    end

    # Clean up the request string
    ccall(:jl_uv_fs_req_cleanup, Cvoid, (Ptr{UInt8},), uv_readdir_req)

    return entries
end

readdir() = readdir(".")

"""
    walkdir(dir; topdown=true, follow_symlinks=false, onerror=throw)

Return an iterator that walks the directory tree of a directory.
The iterator returns a tuple containing `(rootpath, dirs, files)`.
The directory tree can be traversed top-down or bottom-up.
If `walkdir` encounters a [`SystemError`](@ref)
it will rethrow the error by default.
A custom error handling function can be provided through `onerror` keyword argument.
`onerror` is called with a `SystemError` as argument.

# Examples
```julia
for (root, dirs, files) in walkdir(".")
    println("Directories in \$root")
    for dir in dirs
        println(joinpath(root, dir)) # path to directories
    end
    println("Files in \$root")
    for file in files
        println(joinpath(root, file)) # path to files
    end
end
```

```julia-repl
julia> mkpath("my/test/dir");

julia> itr = walkdir("my");

julia> (root, dirs, files) = first(itr)
("my", ["test"], String[])

julia> (root, dirs, files) = first(itr)
("my/test", ["dir"], String[])

julia> (root, dirs, files) = first(itr)
("my/test/dir", String[], String[])
```
"""
function walkdir(root; topdown=true, follow_symlinks=false, onerror=throw)
    content = nothing
    try
        content = readdir(root)
    catch err
        isa(err, SystemError) || throw(err)
        onerror(err)
        # Need to return an empty closed channel to skip the current root folder
        chnl = Channel(0)
        close(chnl)
        return chnl
    end
    dirs = Vector{eltype(content)}()
    files = Vector{eltype(content)}()
    for name in content
        if isdir(joinpath(root, name))
            push!(dirs, name)
        else
            push!(files, name)
        end
    end

    function _it(chnl)
        if topdown
            put!(chnl, (root, dirs, files))
        end
        for dir in dirs
            path = joinpath(root,dir)
            if follow_symlinks || !islink(path)
                for (root_l, dirs_l, files_l) in walkdir(path, topdown=topdown, follow_symlinks=follow_symlinks, onerror=onerror)
                    put!(chnl, (root_l, dirs_l, files_l))
                end
            end
        end
        if !topdown
            put!(chnl, (root, dirs, files))
        end
    end

    return Channel(_it)
end

function unlink(p::AbstractString)
    err = ccall(:jl_fs_unlink, Int32, (Cstring,), p)
    uv_error("unlink", err)
    nothing
end

# For move command
function rename(src::AbstractString, dst::AbstractString)
    err = ccall(:jl_fs_rename, Int32, (Cstring, Cstring), src, dst)
    # on error, default to cp && rm
    if err < 0
        # force: is already done in the mv function
        cp(src, dst; force=false, follow_symlinks=false)
        rm(src; recursive=true)
    end
    nothing
end

function sendfile(src::AbstractString, dst::AbstractString)
    src_open = false
    dst_open = false
    local src_file, dst_file
    try
        src_file = open(src, JL_O_RDONLY)
        src_open = true
        dst_file = open(dst, JL_O_CREAT | JL_O_TRUNC | JL_O_WRONLY, filemode(src_file))
        dst_open = true

        bytes = filesize(stat(src_file))
        sendfile(dst_file, src_file, Int64(0), Int(bytes))
    finally
        if src_open && isopen(src_file)
            close(src_file)
        end
        if dst_open && isopen(dst_file)
            close(dst_file)
        end
    end
end

if Sys.iswindows()
    const UV_FS_SYMLINK_JUNCTION = 0x0002
end

"""
    symlink(target::AbstractString, link::AbstractString)

Creates a symbolic link to `target` with the name `link`.

!!! note
    This function raises an error under operating systems that do not support
    soft symbolic links, such as Windows XP.
"""
function symlink(p::AbstractString, np::AbstractString)
    @static if Sys.iswindows()
        if Sys.windows_version() < Sys.WINDOWS_VISTA_VER
            error("Windows XP does not support soft symlinks")
        end
    end
    flags = 0
    @static if Sys.iswindows()
        if isdir(p)
            flags |= UV_FS_SYMLINK_JUNCTION
            p = abspath(p)
        end
    end
    err = ccall(:jl_fs_symlink, Int32, (Cstring, Cstring, Cint), p, np, flags)
    @static if Sys.iswindows()
        if err < 0 && !isdir(p)
            @warn "On Windows, creating file symlinks requires Administrator privileges" maxlog=1 _group=:file
        end
    end
    uv_error("symlink",err)
end

"""
    readlink(path::AbstractString) -> AbstractString

Return the target location a symbolic link `path` points to.
"""
function readlink(path::AbstractString)
    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_readlink, Int32,
            (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
            eventloop(), req, path, C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
            uv_error("readlink", ret)
            @assert false
        end
        tgt = unsafe_string(ccall(:jl_uv_fs_t_ptr, Ptr{Cchar}, (Ptr{Cvoid},), req))
        ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
        return tgt
    finally
        Libc.free(req)
    end
end

"""
    chmod(path::AbstractString, mode::Integer; recursive::Bool=false)

Change the permissions mode of `path` to `mode`. Only integer `mode`s (e.g. `0o777`) are
currently supported. If `recursive=true` and the path is a directory all permissions in
that directory will be recursively changed.
Return `path`.
"""
function chmod(path::AbstractString, mode::Integer; recursive::Bool=false)
    err = ccall(:jl_fs_chmod, Int32, (Cstring, Cint), path, mode)
    uv_error("chmod", err)
    if recursive && isdir(path)
        for p in readdir(path)
            if !islink(joinpath(path, p))
                chmod(joinpath(path, p), mode, recursive=true)
            end
        end
    end
    path
end

"""
    chown(path::AbstractString, owner::Integer, group::Integer=-1)

Change the owner and/or group of `path` to `owner` and/or `group`. If the value entered for `owner` or `group`
is `-1` the corresponding ID will not change. Only integer `owner`s and `group`s are currently supported.
Return `path`.
"""
function chown(path::AbstractString, owner::Integer, group::Integer=-1)
    err = ccall(:jl_fs_chown, Int32, (Cstring, Cint, Cint), path, owner, group)
    uv_error("chown",err)
    path
end
