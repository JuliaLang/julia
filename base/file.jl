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
    buf = Base.StringVector(AVG_PATH - 1) # space for null-terminator implied by StringVector
    sz = RefValue{Csize_t}(length(buf) + 1) # total buffer size including null
    while true
        rc = ccall(:uv_cwd, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            return String(buf)
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1) # space for null-terminator implied by StringVector
        else
            uv_error(:cwd, rc)
        end
    end
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
    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_mkdir, Cint,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Cint, Ptr{Cvoid}),
                    C_NULL, req, path, checkmode(mode), C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
            uv_error("mkdir", ret)
        end
        ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
        return path
    finally
        Libc.free(req)
    end
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
    (path == dir || isdir(path)) && return path
    mkpath(dir, mode = checkmode(mode))
    try
        mkdir(path, mode = mode)
    catch err
        # If there is a problem with making the directory, but the directory
        # does in fact exist, then ignore the error. Else re-throw it.
        if !isa(err, IOError) || !isdir(path)
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
                if (filemode(lstat(path)) & 0o222) == 0
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

If the file does not exist a new file is created.

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
        if Sys.isunix()
            ret = ccall(:futimes, Cint, (Cint, Ptr{Cvoid}), fd(f), C_NULL)
            systemerror(:futimes, ret != 0, extrainfo=path)
        else
            t = time()
            futime(f,t,t)
        end
    finally
        close(f)
    end
    path
end

"""
    tempdir()

Gets the path of the temporary directory. On Windows, `tempdir()` uses the first environment
variable found in the ordered list `TMP`, `TEMP`, `USERPROFILE`. On all other operating
systems, `tempdir()` uses the first environment variable found in the ordered list `TMPDIR`,
`TMP`, `TEMP`, and `TEMPDIR`. If none of these are found, the path `"/tmp"` is used.
"""
function tempdir()
    buf = Base.StringVector(AVG_PATH - 1) # space for null-terminator implied by StringVector
    sz = RefValue{Csize_t}(length(buf) + 1) # total buffer size including null
    while true
        rc = ccall(:uv_os_tmpdir, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            return String(buf)
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1)  # space for null-terminator implied by StringVector
        else
            uv_error(:tmpdir, rc)
        end
    end
end

const TEMP_CLEANUP_MIN = Ref(1024)
const TEMP_CLEANUP_MAX = Ref(1024)
const TEMP_CLEANUP = Dict{String,Bool}()
const TEMP_CLEANUP_LOCK = ReentrantLock()

function temp_cleanup_later(path::AbstractString; asap::Bool=false)
    lock(TEMP_CLEANUP_LOCK)
    # each path should only be inserted here once, but if there
    # is a collision, let !asap win over asap: if any user might
    # still be using the path, don't delete it until process exit
    TEMP_CLEANUP[path] = get(TEMP_CLEANUP, path, true) & asap
    if length(TEMP_CLEANUP) > TEMP_CLEANUP_MAX[]
        temp_cleanup_purge()
        TEMP_CLEANUP_MAX[] = max(TEMP_CLEANUP_MIN[], 2*length(TEMP_CLEANUP))
    end
    unlock(TEMP_CLEANUP_LOCK)
    return nothing
end

function temp_cleanup_purge(; force::Bool=false)
    need_gc = Sys.iswindows()
    for (path, asap) in TEMP_CLEANUP
        try
            if (force || asap) && ispath(path)
                need_gc && GC.gc(true)
                need_gc = false
                rm(path, recursive=true, force=true)
            end
            !ispath(path) && delete!(TEMP_CLEANUP, path)
        catch ex
            @warn "temp cleanup" _group=:file exception=(ex, catch_backtrace())
        end
    end
end

const temp_prefix = "jl_"

if Sys.iswindows()

function _win_tempname(temppath::AbstractString, uunique::UInt32)
    tempp = cwstring(temppath)
    temppfx = cwstring(temp_prefix)
    tname = Vector{UInt16}(undef, 32767)
    uunique = ccall(:GetTempFileNameW, stdcall, UInt32,
                    (Ptr{UInt16}, Ptr{UInt16}, UInt32, Ptr{UInt16}),
                    tempp, temppfx, uunique, tname)
    windowserror("GetTempFileName", uunique == 0)
    lentname = something(findfirst(iszero, tname))
    @assert lentname > 0
    resize!(tname, lentname - 1)
    return transcode(String, tname)
end

function mktemp(parent::AbstractString=tempdir(); cleanup::Bool=true)
    filename = _win_tempname(parent, UInt32(0))
    cleanup && temp_cleanup_later(filename)
    return (filename, Base.open(filename, "r+"))
end

# generate a random string from random bytes
function _rand_string()
    nchars = 10
    A = Vector{UInt8}(undef, nchars)
    windowserror("SystemFunction036 (RtlGenRandom)", 0 == ccall(
        (:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Cvoid}, UInt32),
            A, sizeof(A)))

    slug = Base.StringVector(10)
    chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for i = 1:nchars
        slug[i] = chars[(A[i] % length(chars)) + 1]
    end
    return name = String(slug)
end

function tempname(parent::AbstractString=tempdir(); cleanup::Bool=true)
    isdir(parent) || throw(ArgumentError("$(repr(parent)) is not a directory"))
    name = _rand_string()
    filename = joinpath(parent, temp_prefix * name)
    @assert !ispath(filename)
    cleanup && temp_cleanup_later(filename)
    return filename
end

else # !windows

# Obtain a temporary filename.
function tempname(parent::AbstractString=tempdir(); cleanup::Bool=true)
    isdir(parent) || throw(ArgumentError("$(repr(parent)) is not a directory"))
    p = ccall(:tempnam, Cstring, (Cstring, Cstring), parent, temp_prefix)
    systemerror(:tempnam, p == C_NULL)
    s = unsafe_string(p)
    Libc.free(p)
    cleanup && temp_cleanup_later(s)
    return s
end

# Create and return the name of a temporary file along with an IOStream
function mktemp(parent::AbstractString=tempdir(); cleanup::Bool=true)
    b = joinpath(parent, temp_prefix * "XXXXXX")
    p = ccall(:mkstemp, Int32, (Cstring,), b) # modifies b
    systemerror(:mktemp, p == -1)
    cleanup && temp_cleanup_later(b)
    return (b, fdio(p, true))
end


end # os-test


"""
    tempname(parent=tempdir(); cleanup=true) -> String

Generate a temporary file path. This function only returns a path; no file is
created. The path is likely to be unique, but this cannot be guaranteed due to
the very remote posibility of two simultaneous calls to `tempname` generating
the same file name. The name is guaranteed to differ from all files already
existing at the time of the call to `tempname`.

When called with no arguments, the temporary name will be an absolute path to a
temporary name in the system temporary directory as given by `tempdir()`. If a
`parent` directory argument is given, the temporary path will be in that
directory instead.

The `cleanup` option controls whether the process attempts to delete the
returned path automatically when the process exits. Note that the `tempname`
function does not create any file or directory at the returned location, so
there is nothing to cleanup unless you create a file or directory there. If
you do and `clean` is `true` it will be deleted upon process termination.

!!! compat "Julia 1.4"
    The `parent` and `cleanup` arguments were added in 1.4. Prior to Julia 1.4
    the path `tempname` would never be cleaned up at process termination.

!!! warning

    This can lead to security holes if another process obtains the same
    file name and creates the file before you are able to. Open the file with
    `JL_O_EXCL` if this is a concern. Using [`mktemp()`](@ref) is also
    recommended instead.
"""
tempname()

"""
    mktemp(parent=tempdir(); cleanup=true) -> (path, io)

Return `(path, io)`, where `path` is the path of a new temporary file in `parent`
and `io` is an open file object for this path. The `cleanup` option controls whether
the temporary file is automatically deleted when the process exits.
"""
mktemp(parent)

"""
    mktempdir(parent=tempdir(); prefix=$(repr(temp_prefix)), cleanup=true) -> path

Create a temporary directory in the `parent` directory with a name
constructed from the given prefix and a random suffix, and return its path.
Additionally, any trailing `X` characters may be replaced with random characters.
If `parent` does not exist, throw an error. The `cleanup` option controls whether
the temporary directory is automatically deleted when the process exits.
"""
function mktempdir(parent::AbstractString=tempdir();
    prefix::AbstractString=temp_prefix, cleanup::Bool=true)
    if isempty(parent) || occursin(path_separator_re, parent[end:end])
        # append a path_separator only if parent didn't already have one
        tpath = "$(parent)$(prefix)XXXXXX"
    else
        tpath = "$(parent)$(path_separator)$(prefix)XXXXXX"
    end

    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_mkdtemp, Cint,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                    C_NULL, req, tpath, C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
            uv_error("mktempdir", ret)
        end
        path = unsafe_string(ccall(:jl_uv_fs_t_path, Cstring, (Ptr{Cvoid},), req))
        ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
        cleanup && temp_cleanup_later(path)
        return path
    finally
        Libc.free(req)
    end
end


"""
    mktemp(f::Function, parent=tempdir())

Apply the function `f` to the result of [`mktemp(parent)`](@ref) and remove the
temporary file upon completion.
"""
function mktemp(fn::Function, parent::AbstractString=tempdir())
    (tmp_path, tmp_io) = mktemp(parent, cleanup=false)
    try
        fn(tmp_path, tmp_io)
    finally
        try
            close(tmp_io)
            ispath(tmp_path) && rm(tmp_path)
        catch ex
            @error "mktemp cleanup" _group=:file exception=(ex, catch_backtrace())
            # might be possible to remove later
            temp_cleanup_later(tmp_path, asap=true)
        end
    end
end

"""
    mktempdir(f::Function, parent=tempdir(); prefix=$(repr(temp_prefix)))

Apply the function `f` to the result of [`mktempdir(parent; prefix)`](@ref) and remove the
temporary directory all of its contents upon completion.
"""
function mktempdir(fn::Function, parent::AbstractString=tempdir();
    prefix::AbstractString=temp_prefix)
    tmpdir = mktempdir(parent; prefix=prefix, cleanup=false)
    try
        fn(tmpdir)
    finally
        try
            ispath(tmpdir) && rm(tmpdir, recursive=true)
        catch ex
            @error "mktempdir cleanup" _group=:file exception=(ex, catch_backtrace())
            # might be possible to remove later
            temp_cleanup_later(tmpdir, asap=true)
        end
    end
end

struct uv_dirent_t
    name::Ptr{UInt8}
    typ::Cint
end

"""
    readdir(dir::AbstractString=pwd();
        join::Bool = false,
        sort::Bool = true,
    ) -> Vector{String}

Return the names in the directory `dir` or the current working directory if not
given. When `join` is false, `readdir` returns just the names in the directory
as is; when `join` is true, it returns `joinpath(dir, name)` for each `name` so
that the returned strings are full paths. If you want to get absolute paths
back, call `readdir` with an absolute directory path and `join` set to true.

By default, `readdir` sorts the list of names it returns. If you want to skip
sorting the names and get them in the order that the file system lists them,
you can use `readdir(dir, sort=false)` to opt out of sorting.

!!! compat "Julia 1.4"
    The `join` and `sort` keyword arguments require at least Julia 1.4.

# Examples
```julia-repl
julia> cd("/home/JuliaUser/dev/julia")

julia> readdir()
30-element Array{String,1}:
 ".appveyor.yml"
 ".git"
 ".gitattributes"
 ⋮
 "ui"
 "usr"
 "usr-staging"

julia> readdir(join=true)
30-element Array{String,1}:
 "/home/JuliaUser/dev/julia/.appveyor.yml"
 "/home/JuliaUser/dev/julia/.git"
 "/home/JuliaUser/dev/julia/.gitattributes"
 ⋮
 "/home/JuliaUser/dev/julia/ui"
 "/home/JuliaUser/dev/julia/usr"
 "/home/JuliaUser/dev/julia/usr-staging"

julia> readdir("base")
145-element Array{String,1}:
 ".gitignore"
 "Base.jl"
 "Enums.jl"
 ⋮
 "version_git.sh"
 "views.jl"
 "weakkeydict.jl"

julia> readdir("base", join=true)
145-element Array{String,1}:
 "base/.gitignore"
 "base/Base.jl"
 "base/Enums.jl"
 ⋮
 "base/version_git.sh"
 "base/views.jl"
 "base/weakkeydict.jl"```

julia> readdir(abspath("base"), join=true)
145-element Array{String,1}:
 "/home/JuliaUser/dev/julia/base/.gitignore"
 "/home/JuliaUser/dev/julia/base/Base.jl"
 "/home/JuliaUser/dev/julia/base/Enums.jl"
 ⋮
 "/home/JuliaUser/dev/julia/base/version_git.sh"
 "/home/JuliaUser/dev/julia/base/views.jl"
 "/home/JuliaUser/dev/julia/base/weakkeydict.jl"
```
"""
function readdir(dir::AbstractString; join::Bool=false, sort::Bool=true)
    # Allocate space for uv_fs_t struct
    uv_readdir_req = zeros(UInt8, ccall(:jl_sizeof_uv_fs_t, Int32, ()))

    # defined in sys.c, to call uv_fs_readdir, which sets errno on error.
    err = ccall(:uv_fs_scandir, Int32, (Ptr{Cvoid}, Ptr{UInt8}, Cstring, Cint, Ptr{Cvoid}),
                C_NULL, uv_readdir_req, dir, 0, C_NULL)
    err < 0 && throw(SystemError("unable to read directory $dir", -err))

    # iterate the listing into entries
    entries = String[]
    ent = Ref{uv_dirent_t}()
    while Base.UV_EOF != ccall(:uv_fs_scandir_next, Cint, (Ptr{Cvoid}, Ptr{uv_dirent_t}), uv_readdir_req, ent)
        name = unsafe_string(ent[].name)
        push!(entries, join ? joinpath(dir, name) : name)
    end

    # Clean up the request string
    ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{UInt8},), uv_readdir_req)

    # sort entries unless opted out
    sort && sort!(entries)

    return entries
end
readdir(; join::Bool=false, sort::Bool=true) =
    readdir(join ? pwd() : ".", join=join, sort=sort)

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
        path = joinpath(root, name)

        # If we're not following symlinks, then treat all symlinks as files
        if (!follow_symlinks && islink(path)) || !isdir(path)
            push!(files, name)
        else
            push!(dirs, name)
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
function rename(src::AbstractString, dst::AbstractString; force::Bool=false)
    err = ccall(:jl_fs_rename, Int32, (Cstring, Cstring), src, dst)
    # on error, default to cp && rm
    if err < 0
        cp(src, dst; force=force, follow_symlinks=false)
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
            C_NULL, req, path, C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
            uv_error("readlink", ret)
            @assert false
        end
        tgt = unsafe_string(ccall(:jl_uv_fs_t_ptr, Cstring, (Ptr{Cvoid},), req))
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

!!! note
     Prior to Julia 1.6, this did not correctly manipulate filesystem ACLs
     on Windows, therefore it would only set read-only bits on files.  It
     now is able to manipulate ACLs.
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
