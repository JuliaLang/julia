# This file is a part of Julia. License is MIT: https://julialang.org/license

# Operations with the file system (paths) ##

export
    cd,
    chmod,
    chown,
    cp,
    cptree,
    DirEntry,
    diskstat,
    hardlink,
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
    scandir,
    sendfile,
    symlink,
    tempdir,
    tempname,
    touch,
    unlink,
    walkdir

# get and set current directory

"""
    pwd()::String

Get the current working directory.

See also: [`cd`](@ref), [`tempdir`](@ref).

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
            uv_error("pwd()", rc)
        end
    end
end


"""
    cd(dir::AbstractString=homedir())

Set the current working directory.

See also: [`pwd`](@ref), [`mkdir`](@ref), [`mkpath`](@ref), [`mktempdir`](@ref).

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
    err = ccall(:uv_chdir, Cint, (Cstring,), dir)
    err < 0 && uv_error("cd($(repr(dir)))", err)
    return nothing
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
        fd = ccall(:open, Int32, (Cstring, Int32, UInt32...), :., 0)
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
34-element Vector{String}:
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
```jldoctest; setup = :(curdir = pwd(); testdir = mktempdir(); cd(testdir)), teardown = :(cd(curdir); rm(testdir, recursive=true)), filter = r"^\\".*testingdir\\"\$"
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
            uv_fs_req_cleanup(req)
            uv_error("mkdir($(repr(path)); mode=0o$(string(mode,base=8)))", ret)
        end
        uv_fs_req_cleanup(req)
        return path
    finally
        Libc.free(req)
    end
end

"""
    mkpath(path::AbstractString; mode::Unsigned = 0o777)

Create all intermediate directories in the `path` as required. Directories are created with
the permissions `mode` which defaults to `0o777` and is modified by the current file
creation mask. Unlike [`mkdir`](@ref), `mkpath` does not error if `path` (or parts of it)
already exists. However, an error will be thrown if `path` (or parts of it) points to an
existing file. Return `path`.

If `path` includes a filename you will probably want to use `mkpath(dirname(path))` to
avoid creating a directory using the filename.

# Examples
```julia-repl
julia> cd(mktempdir())

julia> mkpath("my/test/dir") # creates three directories
"my/test/dir"

julia> readdir()
1-element Vector{String}:
 "my"

julia> cd("my")

julia> readdir()
1-element Vector{String}:
 "test"

julia> readdir("test")
1-element Vector{String}:
 "dir"

julia> mkpath("intermediate_dir/actually_a_directory.txt") # creates two directories
"intermediate_dir/actually_a_directory.txt"

julia> isdir("intermediate_dir/actually_a_directory.txt")
true

julia> mkpath("my/test/dir/") # returns the original `path`
"my/test/dir/"
```
"""
function mkpath(path::AbstractString; mode::Integer = 0o777)
    parent = dirname(path)
    # stop recursion for `""`, `"/"`, or existing dir
    (path == parent || isdir(path)) && return path
    mkpath(parent, mode = checkmode(mode))
    try
        # The `isdir` check could be omitted, then `mkdir` will throw an error in cases like `x/`.
        # Although the error will not be rethrown, we avoid it in advance for performance reasons.
        isdir(path) || mkdir(path, mode = mode)
    catch err
        # If there is a problem with making the directory, but the directory
        # does in fact exist, then ignore the error. Else re-throw it.
        if !isa(err, IOError) || !isdir(path)
            rethrow()
        end
    end
    return path
end

# Files that were requested to be deleted but can't be by the current process,
# i.e. loaded DLLs on Windows, are listed in the directory below
delayed_delete_ref() = joinpath(tempdir(), "julia_delayed_deletes_ref")

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
ERROR: IOError: unlink("this_file_does_not_exist"): no such file or directory (ENOENT)
Stacktrace:
[...]
```
"""
function rm(path::AbstractString; force::Bool=false, recursive::Bool=false, allow_delayed_delete::Bool=true)
    # allow_delayed_delete is used by Pkg.gc() but is otherwise not part of the public API
    if islink(path) || !isdir(path)
        try
            unlink(path)
        catch err
            if isa(err, IOError)
                force && err.code==Base.UV_ENOENT && return
                @static if Sys.iswindows()
                    if allow_delayed_delete && err.code==Base.UV_EACCES && endswith(path, ".dll")
                        delayed_delete_dll(path)
                        return
                    end
                end
            end
            rethrow()
        end
    else
        if recursive
            try
                for p in readdir(path)
                    try
                        rm(joinpath(path, p), force=force, recursive=true)
                    catch err
                        (isa(err, IOError) && err.code==Base.UV_EACCES) || rethrow()
                    end
                end
            catch err
                (isa(err, IOError) && err.code==Base.UV_EACCES) || rethrow()
            end
        end
        req = Libc.malloc(_sizeof_uv_fs)
        try
            ret = ccall(:uv_fs_rmdir, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}), C_NULL, req, path, C_NULL)
            uv_fs_req_cleanup(req)
            if ret < 0 && !(force && ret == Base.UV_ENOENT)
                uv_error("rm($(repr(path)))", ret)
            end
            nothing
        finally
            Libc.free(req)
        end
    end
end


# Loaded DLLs cannot be deleted on Windows, even with posix delete mode but they can be renamed.
# delayed_delete_dll(path) does so temporarily, until later cleanup by Pkg.gc().
function delayed_delete_dll(path)
    # in-use DLL must be kept on the same drive
    temp_path = tempname(abspath(dirname(path)); cleanup=false, suffix=string("_", basename(path)))
    @debug "Could not delete DLL most likely because it is loaded, moving to a temporary path" path temp_path
    mkpath(delayed_delete_ref())
    io = last(mktemp(delayed_delete_ref(); cleanup=false))
    try
        print(io, temp_path) # record the temporary path for Pkg.gc()
    finally
        close(io)
    end
    rename(path, temp_path) # do not call mv which could recursively call rm(path)
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
                throw(ArgumentError(string("'src' and 'dst' refer to the same file/dir. ",
                                           "This is not supported.\n  ",
                                           "`src` refers to: $(abs_src)\n  ",
                                           "`dst` refers to: $(abs_dst)\n")))
            end
            rm(dst; recursive=true, force=true)
        else
            throw(ArgumentError(string("'$dst' exists. `force=true` ",
                                       "is required to remove '$dst' before $(txt).")))
        end
    end
end

function cptree(src::String, dst::String; force::Bool=false,
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
cptree(src::AbstractString, dst::AbstractString; kwargs...) =
    cptree(String(src)::String, String(dst)::String; kwargs...)

"""
    cp(src::AbstractString, dst::AbstractString; force::Bool=false, follow_symlinks::Bool=false)

Copy the file, link, or directory from `src` to `dst`.
`force=true` will first remove an existing `dst`.

If `follow_symlinks=false`, and `src` is a symbolic link, `dst` will be created as a
symbolic link. If `follow_symlinks=true` and `src` is a symbolic link, `dst` will be a copy
of the file or directory `src` refers to.
Return `dst`.

The timestamps, permissions, and ownership (if possible) of the destination file(s) are copied
from those of the source file(s), similar to the Unix `cp -p` command.

!!! note
    The `cp` function is different from the `cp` Unix command. The `cp` function always operates on
    the assumption that `dst` is a file, while the command does different things depending
    on whether `dst` is a directory or a file.
    Using `force=true` when `dst` is a directory will result in loss of all the contents present
    in the `dst` directory, and `dst` will become a file that has the contents of `src` instead.

!!! compat "Julia 1.13"
    Prior to Julia 1.13, the file permissions and other metadata were not necessarily
    preserved (e.g. the permissions were modified by the current `umask` on Unix systems).
"""
function cp(src::AbstractString, dst::AbstractString; force::Bool=false,
                                                      follow_symlinks::Bool=false)
    checkfor_mv_cp_cptree(src, dst, "copying"; force=force)
    if !follow_symlinks && islink(src)
        symlink(readlink(src), dst)
    elseif isdir(src)
        cptree(src, dst; force=force, follow_symlinks=follow_symlinks)
    else
        sendfile(src, dst; force)
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

!!! note
    The `mv` function is different from the `mv` Unix command. The `mv` function by
    default will error if `dst` exists, while the command will delete
    an existing `dst` file by default.
    Also the `mv` function always operates on
    the assumption that `dst` is a file, while the command does different things depending
    on whether `dst` is a directory or a file.
    Using `force=true` when `dst` is a directory will result in loss of all the contents present
    in the `dst` directory, and `dst` will become a file that has the contents of `src` instead.
"""
function mv(src::AbstractString, dst::AbstractString; force::Bool=false)
    if force
        _mv_replace(src, dst)
    else
        _mv_noreplace(src, dst)
    end
end

function _mv_replace(src::AbstractString, dst::AbstractString)
    # This check is copied from checkfor_mv_cp_cptree
    if ispath(dst) && Base.samefile(src, dst)
        abs_src = islink(src) ? abspath(readlink(src)) : abspath(src)
        abs_dst = islink(dst) ? abspath(readlink(dst)) : abspath(dst)
        throw(ArgumentError(string("'src' and 'dst' refer to the same file/dir. ",
                                   "This is not supported.\n  ",
                                   "`src` refers to: $(abs_src)\n  ",
                                   "`dst` refers to: $(abs_dst)\n")))
    end
    # First try to do a regular rename, because this might avoid a situation
    # where dst is deleted or truncated.
    try
        rename(src, dst)
    catch err
        err isa IOError || rethrow()
        err.code==Base.UV_ENOENT && rethrow()
        # on rename error try to delete dst if it exists and isn't the same as src
        checkfor_mv_cp_cptree(src, dst, "moving"; force=true)
        try
            rename(src, dst)
        catch err
            err isa IOError || rethrow()
            # on second error, default to force cp && rm
            cp(src, dst; force=true, follow_symlinks=false)
            rm(src; recursive=true)
        end
    end
    dst
end

function _mv_noreplace(src::AbstractString, dst::AbstractString)
    # Error if dst exists.
    # This check currently has TOCTTOU issues.
    checkfor_mv_cp_cptree(src, dst, "moving"; force=false)
    try
        rename(src, dst)
    catch err
        err isa IOError || rethrow()
        err.code==Base.UV_ENOENT && rethrow()
        # on error, default to cp && rm
        cp(src, dst; force=false, follow_symlinks=false)
        rm(src; recursive=true)
    end
    dst
end


"""
    touch(path::AbstractString)
    touch(fd::File)

Update the last-modified timestamp on a file to the current time.

If the file does not exist a new file is created.

Return `path`.

# Examples
```jldoctest; setup = :(curdir = pwd(); testdir = mktempdir(); cd(testdir)), teardown = :(cd(curdir); rm(testdir, recursive=true)), filter = r"[\\d\\.]+e[\\+\\-]?\\d+"
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
        touch(f)
    finally
        close(f)
    end
    path
end


"""
    tempdir()

Get the path of the temporary directory. On Windows, `tempdir()` uses the first environment
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
            break
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1)  # space for null-terminator implied by StringVector
        else
            uv_error("tempdir()", rc)
        end
    end
    tempdir = String(buf)
    try
        s = stat(tempdir)
        if !ispath(s)
            @warn "tempdir path does not exist" tempdir
        elseif !isdir(s)
            @warn "tempdir path is not a directory" tempdir
        end
    catch ex
        ex isa IOError || ex isa SystemError || rethrow()
        @warn "accessing tempdir path failed" _exception=ex
    end
    return tempdir
end

"""
    prepare_for_deletion(path::AbstractString)

Prepares the given `path` for deletion by ensuring that all directories within that
`path` have write permissions, so that files can be removed from them.  This is
automatically invoked by methods such as `mktempdir()` to ensure that no matter what
weird permissions a user may have created directories with within the temporary prefix,
it will always be deleted.
"""
function prepare_for_deletion(path::AbstractString)
    # Nothing to do for non-directories
    if !isdir(path)
        return
    end

    try
        chmod(path, filemode(path) | 0o333)
    catch ex
        ex isa IOError || ex isa SystemError || rethrow()
    end
    for (root, dirs, files) in walkdir(path; onerror=x->())
        for dir in dirs
            dpath = joinpath(root, dir)
            try
                chmod(dpath, filemode(dpath) | 0o333)
            catch ex
                ex isa IOError || ex isa SystemError || rethrow()
            end
        end
    end
end

const TEMP_CLEANUP_MIN = Ref(1024)
const TEMP_CLEANUP_MAX = Ref(1024)
const TEMP_CLEANUP = Dict{String,Bool}()
const TEMP_CLEANUP_LOCK = ReentrantLock()

function temp_cleanup_later(path::AbstractString; asap::Bool=false)
    @lock TEMP_CLEANUP_LOCK begin
    # each path should only be inserted here once, but if there
    # is a collision, let !asap win over asap: if any user might
    # still be using the path, don't delete it until process exit
    TEMP_CLEANUP[path] = get(TEMP_CLEANUP, path, true) & asap
    if length(TEMP_CLEANUP) > TEMP_CLEANUP_MAX[]
        temp_cleanup_purge_prelocked(false)
        TEMP_CLEANUP_MAX[] = max(TEMP_CLEANUP_MIN[], 2*length(TEMP_CLEANUP))
    end
    end
    nothing
end

function temp_cleanup_forget(path::AbstractString)
    @lock TEMP_CLEANUP_LOCK delete!(TEMP_CLEANUP, path)
    nothing
end

function temp_cleanup_purge_prelocked(force::Bool)
    filter!(TEMP_CLEANUP) do (path, asap)
        try
            ispath(path) || return false
            if force || asap
                prepare_for_deletion(path)
                rm(path, recursive=true, force=true)
            end
            return ispath(path)
        catch ex
            @warn """
                Failed to clean up temporary path $(repr(path))
                $ex
                """ _group=:file
            ex isa InterruptException && rethrow()
            return true
        end
    end
    nothing
end

function temp_cleanup_purge_all()
    may_need_gc = Ref(false)
    @lock TEMP_CLEANUP_LOCK filter!(TEMP_CLEANUP) do (path, asap)
        try
            ispath(path) || return false
            may_need_gc[] = true
            return true
        catch ex
            ex isa InterruptException && rethrow()
            return true
        end
    end
    if may_need_gc[]
        # this is only usually required on Sys.iswindows(), but may as well do it everywhere
        GC.gc(true)
    end
    @lock TEMP_CLEANUP_LOCK temp_cleanup_purge_prelocked(true)
    nothing
end

# deprecated internal function used by some packages
temp_cleanup_purge(; force=false) = force ? temp_cleanup_purge_all() : @lock TEMP_CLEANUP_LOCK temp_cleanup_purge_prelocked(false)

function temp_cleanup_postprocess(cleanup_dirs)
    if !isempty(cleanup_dirs)
        rmcmd = """
        cleanuplist = readlines(stdin) # This loop won't start running until stdin is closed, which is supposed to be sequenced after the process exits
        sleep(1) # Wait for the operating system to hopefully be ready, since the OS implementation is probably incorrect, given the history of buggy work-arounds like this that have existed for ages in dotNet and libuv
        for path in cleanuplist
            try
                rm(path, force=true, recursive=true)
            catch ex
                @warn "Failed to clean up temporary path \$(repr(path))\n\$ex" _group=:file
            end
        end
        """
        cmd = Cmd(Base.cmd_gen(((Base.julia_cmd(),), ("--startup-file=no",), ("-e",), (rmcmd,))); ignorestatus = true, detach = true)
        pw = Base.PipeEndpoint()
        run(cmd, pw, devnull, stderr; wait=false)
        join(pw, cleanup_dirs, "\n")
        Base.dup(Base._fd(pw)) # intentionally leak a reference, until the process exits
        close(pw)
    end
end

function temp_cleanup_atexit()
    temp_cleanup_purge_all()
    @lock TEMP_CLEANUP_LOCK temp_cleanup_postprocess(keys(TEMP_CLEANUP))
end

function __postinit__()
    Base.atexit(temp_cleanup_atexit)
end

const temp_prefix = "jl_"

# Use `Libc.rand()` to generate random strings
function _rand_filename(len = 10)
    slug = Base.StringVector(len)
    chars = b"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for i = 1:len
        slug[i] = chars[(Libc.rand() % length(chars)) + 1]
    end
    return String(slug)
end


# Obtain a temporary filename.
function tempname(parent::AbstractString=tempdir(); max_tries::Int = 100, cleanup::Bool=true, suffix::AbstractString="")
    isdir(parent) || throw(ArgumentError("$(repr(parent)) is not a directory"))

    prefix = joinpath(parent, temp_prefix)
    filename = nothing
    for i in 1:max_tries
        filename = string(prefix, _rand_filename(), suffix)
        if ispath(filename)
            filename = nothing
        else
            break
        end
    end

    if filename === nothing
        error("tempname: max_tries exhausted")
    end

    cleanup && temp_cleanup_later(filename)
    return filename
end

if Sys.iswindows()
# While this isn't a true analog of `mkstemp`, it _does_ create an
# empty file for us, ensuring that other simultaneous calls to
# `_win_mkstemp()` won't collide, so it's a better name for the
# function than `tempname()`.
function _win_mkstemp(temppath::AbstractString)
    tempp = cwstring(temppath)
    temppfx = cwstring(temp_prefix)
    tname = Vector{UInt16}(undef, 32767)
    uunique = ccall(:GetTempFileNameW, stdcall, UInt32,
                    (Ptr{UInt16}, Ptr{UInt16}, UInt32, Ptr{UInt16}),
                    tempp, temppfx, UInt32(0), tname)
    windowserror("GetTempFileName", uunique == 0)
    lentname = something(findfirst(iszero, tname))
    @assert lentname > 0 "unexpected index"
    resize!(tname, lentname - 1)
    return transcode(String, tname)
end

function mktemp(parent::AbstractString=tempdir(); cleanup::Bool=true)
    filename = _win_mkstemp(parent)
    cleanup && temp_cleanup_later(filename)
    return (filename, Base.open(filename, "r+"))
end

else # !windows

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
    tempname(parent=tempdir(); cleanup=true, suffix="")::String

Generate a temporary file path. This function only returns a path; no file is
created. The path is likely to be unique, but this cannot be guaranteed due to
the very remote possibility of two simultaneous calls to `tempname` generating
the same file name. The name is guaranteed to differ from all files already
existing at the time of the call to `tempname`.

When called with no arguments, the temporary name will be an absolute path to a
temporary name in the system temporary directory as given by `tempdir()`. If a
`parent` directory argument is given, the temporary path will be in that
directory instead. If a suffix is given the tempname will end with that suffix
and be tested for uniqueness with that suffix.

The `cleanup` option controls whether the process attempts to delete the
returned path automatically when the process exits. Note that the `tempname`
function does not create any file or directory at the returned location, so
there is nothing to cleanup unless you create a file or directory there. If
you do and `cleanup` is `true` it will be deleted upon process termination.

!!! compat "Julia 1.4"
    The `parent` and `cleanup` arguments were added in 1.4. Prior to Julia 1.4
    the path `tempname` would never be cleaned up at process termination.

!!! compat "Julia 1.12"
    The `suffix` keyword argument was added in Julia 1.12.

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

!!! compat "Julia 1.3"
    The `cleanup` keyword argument was added in Julia 1.3. Relatedly, starting from 1.3,
    Julia will remove the temporary paths created by `mktemp` when the Julia process exits,
    unless `cleanup` is explicitly set to `false`.
"""
mktemp(parent)

"""
    mktempdir(parent=tempdir(); prefix=$(repr(temp_prefix)), cleanup=true) -> path

Create a temporary directory in the `parent` directory with a name
constructed from the given `prefix` and a random suffix, and return its path.
Additionally, on some platforms, any trailing `'X'` characters in `prefix` may be replaced
with random characters.
If `parent` does not exist, throw an error. The `cleanup` option controls whether
the temporary directory is automatically deleted when the process exits.

!!! compat "Julia 1.2"
    The `prefix` keyword argument was added in Julia 1.2.

!!! compat "Julia 1.3"
    The `cleanup` keyword argument was added in Julia 1.3. Relatedly, starting from 1.3,
    Julia will remove the temporary paths created by `mktempdir` when the Julia process
    exits, unless `cleanup` is explicitly set to `false`.

See also: [`mktemp`](@ref), [`mkdir`](@ref).
"""
function mktempdir(parent::AbstractString=tempdir();
    prefix::AbstractString=temp_prefix, cleanup::Bool=true)
    if isempty(parent) || isseparator(last(parent))
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
            uv_fs_req_cleanup(req)
            uv_error("mktempdir($(repr(parent)))", ret)
        end
        path = unsafe_string(ccall(:jl_uv_fs_t_path, Cstring, (Ptr{Cvoid},), req))
        uv_fs_req_cleanup(req)
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

See also: [`mktempdir`](@ref).
"""
function mktemp(fn::Function, parent::AbstractString=tempdir())
    (tmp_path, tmp_io) = mktemp(parent)
    try
        fn(tmp_path, tmp_io)
    finally
        temp_cleanup_forget(tmp_path)
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
temporary directory and all of its contents upon completion.

See also: [`mktemp`](@ref), [`mkdir`](@ref).

!!! compat "Julia 1.2"
    The `prefix` keyword argument was added in Julia 1.2.
"""
function mktempdir(fn::Function, parent::AbstractString=tempdir();
    prefix::AbstractString=temp_prefix)
    tmpdir = mktempdir(parent; prefix=prefix)
    try
        fn(tmpdir)
    finally
        temp_cleanup_forget(tmpdir)
        try
            if ispath(tmpdir)
                prepare_for_deletion(tmpdir)
                rm(tmpdir, recursive=true)
            end
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
    )::Vector{String}

Return the names in the directory `dir` or the current working directory if not
given. When `join` is false, `readdir` returns just the names in the directory
as is; when `join` is true, it returns `joinpath(dir, name)` for each `name` so
that the returned strings are full paths. If you want to get absolute paths
back, call `readdir` with an absolute directory path and `join` set to true.

By default, `readdir` sorts the list of names it returns. If you want to skip
sorting the names and get them in the order that the file system lists them,
you can use `readdir(dir, sort=false)` to opt out of sorting.

See also: [`walkdir`](@ref).

!!! compat "Julia 1.4"
    The `join` and `sort` keyword arguments require at least Julia 1.4.

# Examples
```julia-repl
julia> cd("/home/JuliaUser/dev/julia")

julia> readdir()
30-element Vector{String}:
 ".appveyor.yml"
 ".git"
 ".gitattributes"
 ⋮
 "ui"
 "usr"
 "usr-staging"

julia> readdir(join=true)
30-element Vector{String}:
 "/home/JuliaUser/dev/julia/.appveyor.yml"
 "/home/JuliaUser/dev/julia/.git"
 "/home/JuliaUser/dev/julia/.gitattributes"
 ⋮
 "/home/JuliaUser/dev/julia/ui"
 "/home/JuliaUser/dev/julia/usr"
 "/home/JuliaUser/dev/julia/usr-staging"

julia> readdir("base")
145-element Vector{String}:
 ".gitignore"
 "Base.jl"
 "Enums.jl"
 ⋮
 "version_git.sh"
 "views.jl"
 "weakkeydict.jl"

julia> readdir("base", join=true)
145-element Vector{String}:
 "base/.gitignore"
 "base/Base.jl"
 "base/Enums.jl"
 ⋮
 "base/version_git.sh"
 "base/views.jl"
 "base/weakkeydict.jl"

julia> readdir(abspath("base"), join=true)
145-element Vector{String}:
 "/home/JuliaUser/dev/julia/base/.gitignore"
 "/home/JuliaUser/dev/julia/base/Base.jl"
 "/home/JuliaUser/dev/julia/base/Enums.jl"
 ⋮
 "/home/JuliaUser/dev/julia/base/version_git.sh"
 "/home/JuliaUser/dev/julia/base/views.jl"
 "/home/JuliaUser/dev/julia/base/weakkeydict.jl"
```
"""
readdir(; join::Bool=false, kwargs...) = readdir(join ? pwd() : "."; join, kwargs...)::Vector{String}
readdir(dir::AbstractString; kwargs...) = _readdir(dir; return_objects=false, kwargs...)::Vector{String}

# this might be better as an Enum but they're not available here
# UV_DIRENT_T
const UV_DIRENT_UNKNOWN = Cint(0)
const UV_DIRENT_FILE = Cint(1)
const UV_DIRENT_DIR = Cint(2)
const UV_DIRENT_LINK = Cint(3)
const UV_DIRENT_FIFO = Cint(4)
const UV_DIRENT_SOCKET = Cint(5)
const UV_DIRENT_CHAR = Cint(6)
const UV_DIRENT_BLOCK = Cint(7)

"""
    DirEntry

A type representing a filesystem entry that contains the name of the entry, the directory, and
the raw type of the entry. The full path of the entry can be obtained lazily via [`joinpath(entry)`](@ref).

The directory and name components are accessed via [`dirname`](@ref) and [`basename`](@ref),
respectively, mirroring the behavior of the corresponding string-returning functions.

The type of the entry can be checked for by calling [`isfile`](@ref), [`isdir`](@ref),
[`islink`](@ref), [`isfifo`](@ref), [`issocket`](@ref), [`ischardev`](@ref), and [`isblockdev`](@ref)
on the entry object. These predicates use the raw type cached at scan time when available; on
filesystems that report `UV_DIRENT_UNKNOWN` (some network/FUSE mounts) and for symlinks they fall
through to a `stat` syscall on each call. Callers in tight loops that need multiple predicates for
the same entry should call [`stat`](@ref) once and reuse the result.

!!! warning "Staleness"
    A `DirEntry` is a snapshot from when the directory was scanned. The underlying filesystem may
    have changed in the meantime: the entry may no longer exist, may have been replaced by a
    different type, or the cached `rawtype` may be wrong. Treat `DirEntry` values as advisory
    and re-`stat` if up-to-date information is required.
"""
struct DirEntry
    dir::String
    name::String
    rawtype::Cint
end
basename(obj::DirEntry) = getfield(obj, :name)
dirname(obj::DirEntry) = getfield(obj, :dir)
joinpath(obj::DirEntry, args...) = joinpath(dirname(obj), basename(obj), args...)
Base.isless(a::DirEntry, b::DirEntry) = dirname(a) == dirname(b) ? isless(basename(a), basename(b)) : isless(dirname(a), dirname(b))
Base.hash(o::DirEntry, h::UInt) = hash(dirname(o), hash(basename(o), hash(o.rawtype, h)))
Base.:(==)(a::DirEntry, b::DirEntry) = basename(a) == basename(b) && dirname(a) == dirname(b) && a.rawtype == b.rawtype
isunknown(obj::DirEntry) =  obj.rawtype == UV_DIRENT_UNKNOWN
islink(obj::DirEntry) =     isunknown(obj) ? islink(joinpath(obj)) : obj.rawtype == UV_DIRENT_LINK
isfile(obj::DirEntry) =     (isunknown(obj) || islink(obj)) ? isfile(joinpath(obj))      : obj.rawtype == UV_DIRENT_FILE
isdir(obj::DirEntry) =      (isunknown(obj) || islink(obj)) ? isdir(joinpath(obj))       : obj.rawtype == UV_DIRENT_DIR
isfifo(obj::DirEntry) =     (isunknown(obj) || islink(obj)) ? isfifo(joinpath(obj))      : obj.rawtype == UV_DIRENT_FIFO
issocket(obj::DirEntry) =   (isunknown(obj) || islink(obj)) ? issocket(joinpath(obj))    : obj.rawtype == UV_DIRENT_SOCKET
ischardev(obj::DirEntry) =  (isunknown(obj) || islink(obj)) ? ischardev(joinpath(obj))   : obj.rawtype == UV_DIRENT_CHAR
isblockdev(obj::DirEntry) = (isunknown(obj) || islink(obj)) ? isblockdev(joinpath(obj))  : obj.rawtype == UV_DIRENT_BLOCK
realpath(obj::DirEntry) = realpath(joinpath(obj))

"""
    readdir(::Type{DirEntry}, dir::AbstractString=pwd(); sort::Bool = true) -> Vector{DirEntry}
    readdir(::Type{DirEntry}, entry::DirEntry; sort::Bool=true) -> Vector{DirEntry}
    readdir(entry::DirEntry; join::Bool=false, sort::Bool=true) -> Vector{String}

Return a vector of [`DirEntry`](@ref) objects representing the contents of the directory `dir`,
or the current working directory if not given. If `sort` is true, the returned vector is
sorted by name.

The element type is selected by the leading type argument: pass `DirEntry` to get
[`DirEntry`](@ref) objects (which carry the type of each entry as determined during the
directory scan, so [`isfile`](@ref), [`isdir`](@ref), etc. can be called without further
`stat` calls), or omit it to get a `Vector{String}` of names — independent of whether the
directory was specified as an `AbstractString` or a `DirEntry`. For some filesystems the
type of the entry cannot be determined without a `stat` call; in those cases the `rawtype`
field of the [`DirEntry`](@ref) is 0 (`UV_DIRENT_UNKNOWN`) and the predicates fall back to
a `stat` call.

# Examples
```julia
for entry in readdir(DirEntry, ".")
    if isfile(entry)
        println("\$(basename(entry)) is a file with path \$(joinpath(entry))")
        continue
    end
    isdir(entry) || continue
    for entry2 in readdir(DirEntry, entry)
        ...
    end
end
```
"""
readdir(::Type{DirEntry}, dir::AbstractString=pwd(); sort::Bool=true) = _readdir(dir; return_objects=true, sort)::Vector{DirEntry}
readdir(::Type{DirEntry}, entry::DirEntry; sort::Bool=true) = readdir(DirEntry, joinpath(entry); sort)::Vector{DirEntry}
readdir(entry::DirEntry; kwargs...) = readdir(joinpath(entry); kwargs...)::Vector{String}

function _readdir(dir::AbstractString; return_objects::Bool=false, join::Bool=false, sort::Bool=true)
    # Allocate space for uv_fs_t struct
    req = Libc.malloc(_sizeof_uv_fs)
    try
        err = ccall(:uv_fs_scandir, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Cint, Ptr{Cvoid}),
                    C_NULL, req, dir, 0, C_NULL)
        err < 0 && uv_error("readdir($(repr(dir)))", err)

        # iterate the listing into entries
        entries = return_objects ? DirEntry[] : String[]
        ent = Ref{uv_dirent_t}()
        while Base.UV_EOF != ccall(:uv_fs_scandir_next, Cint, (Ptr{Cvoid}, Ptr{uv_dirent_t}), req, ent)
            name = unsafe_string(ent[].name)
            if return_objects
                rawtype = ent[].typ
                push!(entries, DirEntry(dir, name, rawtype))
            else
                push!(entries, join ? joinpath(dir, name) : name)
            end
        end

        # Clean up the request string
        uv_fs_req_cleanup(req)

        # sort entries unless opted out
        sort && sort!(entries)

        return entries
    finally
        Libc.free(req)
    end
end

"""
    DirEntryIterator{T}

A stateful, single-pass iterator over the entries of a directory, yielding
either filename `String`s or [`DirEntry`](@ref) objects depending on the
element type `T`. Created by [`scandir`](@ref).

The underlying directory handle is opened lazily on the first call to
`iterate` (via `uv_fs_opendir`) and is released when iteration reaches
end-of-directory, when [`close`](@ref) is called, or by a finalizer.
Entries are streamed one at a time from the operating system; the full
listing is never materialized in memory. For deterministic cleanup on
very large directories, prefer the do-block form of `scandir`.

A `DirEntryIterator` is single-consumer: do not iterate or close it from
multiple tasks concurrently. Calling `close` from a finalizer while the
owning task has finished iterating is safe.
"""
mutable struct DirEntryIterator{T}
    const dir::String
    # Opaque uv_dir_t* returned by jl_uv_fs_opendir; C_NULL until opened and
    # C_NULL again after close. Acts as the ownership gate: the thread that
    # atomically swaps it to C_NULL is the sole owner of the resources below
    # and is responsible for cleanup.
    @atomic uvdir::Ptr{Cvoid}
    # Single-entry uv_dirent_t buffer that libuv writes into via uv_fs_readdir.
    # Must outlive `uvdir`; held here so its address remains valid for the
    # lifetime of the iterator.
    ent::Base.RefValue{uv_dirent_t}
    closed::Bool
    function DirEntryIterator{T}(dir::AbstractString) where {T}
        T === String || T === DirEntry || throw(ArgumentError("element type must be String or DirEntry"))
        finalizer(close, new{T}(String(dir), C_NULL, Ref{uv_dirent_t}(), false))
    end
end

function _scandir_open!(it::DirEntryIterator)
    it.uvdir == C_NULL || return
    it.closed && throw(ArgumentError("DirEntryIterator has already been consumed"))
    dir_out = Ref{Ptr{Cvoid}}(C_NULL)
    err = ccall(:jl_uv_fs_opendir, Cint,
                (Cstring, Ptr{Ptr{Cvoid}}, Ptr{uv_dirent_t}),
                it.dir, dir_out, it.ent)
    if err < 0
        it.closed = true
        uv_error("scandir($(repr(it.dir)))", err)
    end
    @atomic it.uvdir = dir_out[]
    return
end

function Base.close(it::DirEntryIterator)
    # Atomically claim the dir pointer so a concurrent finalizer cannot double-free.
    uvdir = @atomicswap it.uvdir = C_NULL
    if uvdir != C_NULL
        ccall(:jl_uv_fs_closedir, Cint, (Ptr{Cvoid},), uvdir)
    end
    it.closed = true
    return
end

Base.IteratorSize(::Type{<:DirEntryIterator}) = Base.SizeUnknown()
Base.eltype(::Type{DirEntryIterator{T}}) where {T} = T
Base.isdone(it::DirEntryIterator, _=nothing) = it.closed

function Base.iterate(it::DirEntryIterator{T}, _=nothing) where {T}
    _scandir_open!(it)
    name_out = Ref{Ptr{UInt8}}(C_NULL)
    type_out = Ref{Cint}(0)
    rc = ccall(:jl_uv_fs_readdir, Cssize_t,
               (Ptr{Cvoid}, Ptr{Ptr{UInt8}}, Ptr{Cint}),
               it.uvdir, name_out, type_out)
    if rc <= 0
        if rc < 0
            err = rc
            close(it)
            uv_error("scandir($(repr(it.dir)))", err)
        end
        close(it)
        return nothing
    end
    name_ptr = name_out[]
    name = unsafe_string(name_ptr)
    Libc.free(name_ptr)
    rawtype = type_out[]
    value = T === DirEntry ? DirEntry(it.dir, name, rawtype) : name
    return value, nothing
end

"""
    scandir(dir::AbstractString=pwd())                     -> DirEntryIterator{String}
    scandir(entry::DirEntry)                               -> DirEntryIterator{String}
    scandir(::Type{DirEntry}, dir::AbstractString=pwd())   -> DirEntryIterator{DirEntry}
    scandir(::Type{DirEntry}, entry::DirEntry)             -> DirEntryIterator{DirEntry}
    scandir(f, [::Type{DirEntry},] dir)                    -> f(::DirEntryIterator)

Return a stateful, single-pass iterator over the contents of the directory `dir`
(or the current working directory if not given), without first materializing the
full listing. Useful for very large directories or when iteration may be
short-circuited (e.g. with `break` or [`Iterators.take`](@ref)).

By default the iterator yields filename `String`s, matching [`readdir`](@ref).
Pass `DirEntry` as the leading type argument to yield [`DirEntry`](@ref)
objects, which carry the entry type as cached at scan time so [`isfile`](@ref),
[`isdir`](@ref), etc. can be checked without further `stat` calls.

Unlike [`readdir`](@ref), the entries are returned in filesystem order (not
sorted) and the iterator can only be traversed once. The do-block form ensures
the underlying directory handle is released as soon as `f` returns; otherwise
cleanup happens at end-of-iteration, on [`close`](@ref), or via a finalizer.

See also [`readdir`](@ref), [`DirEntry`](@ref), [`walkdir`](@ref).

!!! compat "Julia 1.14"
    `scandir` requires Julia 1.14 or later.

# Examples
```julia
# short-circuit a huge directory
for entry in scandir(DirEntry, "/very/large/dir")
    isfile(entry) && basename(entry) == "needle.txt" && break
end

# deterministic cleanup
scandir("/very/large/dir") do names
    for name in names
        ...
    end
end
```
"""
scandir(dir::AbstractString=pwd()) = DirEntryIterator{String}(dir)
scandir(entry::DirEntry) = DirEntryIterator{String}(joinpath(entry))
scandir(::Type{DirEntry}, dir::AbstractString=pwd()) = DirEntryIterator{DirEntry}(dir)
scandir(::Type{DirEntry}, entry::DirEntry) = DirEntryIterator{DirEntry}(joinpath(entry))
function scandir(f, dir)
    it = scandir(dir)
    try
        return f(it)
    finally
        close(it)
    end
end
function scandir(f, ::Type{DirEntry}, dir)
    it = scandir(DirEntry, dir)
    try
        return f(it)
    finally
        close(it)
    end
end

"""
    walkdir(dir = pwd(); topdown=true, follow_symlinks=false, onerror=throw)

Return an iterator that walks the directory tree of a directory.

The iterator returns a tuple containing `(path, dirs, files)`.
Each iteration `path` will change to the next directory in the tree;
then `dirs` and `files` will be vectors containing the directories and files
in the current `path` directory.
The directory tree can be traversed top-down or bottom-up.
If `walkdir` or `stat` encounters a `IOError` it will rethrow the error by default.
A custom error handling function can be provided through `onerror` keyword argument.
`onerror` is called with a `IOError` as argument.
The returned iterator is stateful so when accessed repeatedly each access will
resume where the last left off, like [`Iterators.Stateful`](@ref).

See also: [`readdir`](@ref).

!!! compat "Julia 1.12"
    `pwd()` as the default directory was added in Julia 1.12.

# Examples
```julia
for (path, dirs, files) in walkdir(".")
    println("Directories in \$path")
    for dir in dirs
        println(joinpath(path, dir)) # path to directories
    end
    println("Files in \$path")
    for file in files
        println(joinpath(path, file)) # path to files
    end
end
```

```jldoctest; setup = :(prevdir = pwd(); tmpdir = mktempdir(); cd(tmpdir)), teardown = :(cd(prevdir); rm(tmpdir, recursive=true))
julia> mkpath("my/test/dir");

julia> itr = walkdir("my");

julia> (path, dirs, files) = first(itr)
("my", ["test"], String[])

julia> (path, dirs, files) = first(itr)
("my/test", ["dir"], String[])

julia> (path, dirs, files) = first(itr)
("my/test/dir", String[], String[])
```
"""
function walkdir(path = pwd(); topdown=true, follow_symlinks=false, onerror=throw)
    return Channel{Tuple{String,Vector{String},Vector{String}}}(chnl ->
        _walkdir(chnl, path, topdown, follow_symlinks, onerror))
end

function _walkdir(chnl, path, topdown, follow_symlinks, onerror)
    tryf(f, p) = try
            f(p)
        catch err
            isa(err, IOError) || rethrow()
            try
                onerror(err)
            catch err2
                close(chnl, err2)
            end
            return
        end
    entries = tryf(p -> readdir(DirEntry, p), path)
    entries === nothing && return
    dirs = Vector{String}()
    files = Vector{String}()
    for entry in entries
        # If we're not following symlinks, then treat all symlinks as files
        if (!follow_symlinks && something(tryf(islink, entry), true)) || !something(tryf(isdir, entry), false)
            push!(files, basename(entry))
        else
            push!(dirs, basename(entry))
        end
    end

    if topdown
        push!(chnl, (path, dirs, files))
    end
    for dir in dirs
        _walkdir(chnl, joinpath(path, dir), topdown, follow_symlinks, onerror)
    end
    if !topdown
        push!(chnl, (path, dirs, files))
    end
    nothing
end

function unlink(p::AbstractString)
    err = ccall(:jl_fs_unlink, Int32, (Cstring,), p)
    err < 0 && uv_error("unlink($(repr(p)))", err)
    nothing
end

"""
    Base.rename(oldpath::AbstractString, newpath::AbstractString)

Change the name of a file or directory from `oldpath` to `newpath`.
If `newpath` is an existing file or empty directory it may be replaced.
Equivalent to [rename(2)](https://man7.org/linux/man-pages/man2/rename.2.html) on Unix.
If a path contains a "\\0" throw an `ArgumentError`.
On other failures throw an `IOError`.
Return `newpath`.

This is a lower level filesystem operation used to implement [`mv`](@ref).

OS-specific restrictions may apply when `oldpath` and `newpath` are in different directories.

Currently there are a few differences in behavior on Windows which may be resolved in a future release.
Specifically, currently on Windows:
1. `rename` will fail if `oldpath` or `newpath` are opened files.
2. `rename` will fail if `newpath` is an existing directory.
3. `rename` may work if `newpath` is a file and `oldpath` is a directory.
4. `rename` may remove `oldpath` if it is a hardlink to `newpath`.

See also: [`mv`](@ref).

!!! compat "Julia 1.12"
    This method was made public in Julia 1.12.
"""
function rename(oldpath::AbstractString, newpath::AbstractString)
    err = ccall(:jl_fs_rename, Int32, (Cstring, Cstring), oldpath, newpath)
    if err < 0
        uv_error("rename($(repr(oldpath)), $(repr(newpath)))", err)
    end
    newpath
end

const UV_FS_COPYFILE_EXCL = 0x0001
const UV_FS_COPYFILE_FICLONE = 0x0002

function sendfile(src::AbstractString, dst::AbstractString; force::Bool=true)
    flags = force ? UV_FS_COPYFILE_FICLONE : UV_FS_COPYFILE_FICLONE | UV_FS_COPYFILE_EXCL
    result = ccall(:jl_fs_copyfile, Cint, (Cstring, Cstring, Cint),
                    src, dst, flags % Cint)
    uv_error("copyfile", result)
    return nothing
end

if Sys.iswindows()
    const UV_FS_SYMLINK_DIR      = 0x0001
    const UV_FS_SYMLINK_JUNCTION = 0x0002
    const UV__EPERM              = -4048
end

"""
    hardlink(src::AbstractString, dst::AbstractString)

Create a hard link to an existing source file `src` with the name `dst`. The
destination, `dst`, must not exist.

See also: [`symlink`](@ref).

!!! compat "Julia 1.8"
    This method was added in Julia 1.8.
"""
function hardlink(src::AbstractString, dst::AbstractString)
    err = ccall(:jl_fs_hardlink, Int32, (Cstring, Cstring), src, dst)
    if err < 0
        msg = "hardlink($(repr(src)), $(repr(dst)))"
        uv_error(msg, err)
    end
    return nothing
end

"""
    symlink(target::AbstractString, link::AbstractString; dir_target = false)

Create a symbolic link to `target` with the name `link`.

On Windows, symlinks must be explicitly declared as referring to a directory
or not.  If `target` already exists, by default the type of `link` will be auto-
detected, however if `target` does not exist, this function defaults to creating
a file symlink unless `dir_target` is set to `true`.  Note that if the user
sets `dir_target` but `target` exists and is a file, a directory symlink will
still be created, but dereferencing the symlink will fail, just as if the user
creates a file symlink (by calling `symlink()` with `dir_target` set to `false`
before the directory is created) and tries to dereference it to a directory.

Additionally, there are two methods of making a link on Windows; symbolic links
and junction points.  Junction points are slightly more efficient, but do not
support relative paths, so if a relative directory symlink is requested (as
denoted by `isabspath(target)` returning `false`) a symlink will be used, else
a junction point will be used.  Best practice for creating symlinks on Windows
is to create them only after the files/directories they reference are already
created.

See also: [`hardlink`](@ref).

!!! note
    This function raises an error under operating systems that do not support
    soft symbolic links, such as Windows XP.

!!! compat "Julia 1.6"
    The `dir_target` keyword argument was added in Julia 1.6.  Prior to this,
    symlinks to nonexistent paths on windows would always be file symlinks, and
    relative symlinks to directories were not supported.
"""
function symlink(target::AbstractString, link::AbstractString;
                 dir_target::Bool = false)
    @static if Sys.iswindows()
        if Sys.windows_version() < Sys.WINDOWS_VISTA_VER
            error("Windows XP does not support soft symlinks")
        end
    end
    flags = 0
    @static if Sys.iswindows()
        # If we're going to create a directory link, we need to know beforehand.
        # First, if `target` is not an absolute path, let's immediately resolve
        # it so that we can peek and see if it's a directory.
        resolved_target = target
        if !isabspath(target)
            resolved_target = joinpath(dirname(link), target)
        end

        # If it is a directory (or `dir_target` is set), we'll need to add one
        # of `UV_FS_SYMLINK_{DIR,JUNCTION}` to the flags, depending on whether
        # `target` is an absolute path or not.
        if (ispath(resolved_target) && isdir(resolved_target)) || dir_target
            if isabspath(target)
                flags |= UV_FS_SYMLINK_JUNCTION
            else
                flags |= UV_FS_SYMLINK_DIR
            end
        end
    end
    err = ccall(:jl_fs_symlink, Int32, (Cstring, Cstring, Cint), target, link, flags)
    if err < 0
        msg = "symlink($(repr(target)), $(repr(link)))"
        @static if Sys.iswindows()
            # creating file/directory symlinks requires Administrator privileges
            # while junction points apparently do not
            if flags & UV_FS_SYMLINK_JUNCTION == 0 && err == UV__EPERM
                msg = "On Windows, creating symlinks requires Administrator privileges.\n$msg"
            end
        end
        uv_error(msg, err)
    end
    return nothing
end

"""
    readlink(path::AbstractString)::String

Return the target location a symbolic link `path` points to.
"""
function readlink(path::AbstractString)
    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_readlink, Int32,
            (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
            C_NULL, req, path, C_NULL)
        if ret < 0
            uv_fs_req_cleanup(req)
            uv_error("readlink($(repr(path)))", ret)
            @assert false "unexpected uv readlink error"
        end
        tgt = unsafe_string(ccall(:jl_uv_fs_t_ptr, Cstring, (Ptr{Cvoid},), req))
        uv_fs_req_cleanup(req)
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
    err < 0 && uv_error("chmod($(repr(path)), 0o$(string(mode, base=8)))", err)
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
    err < 0 && uv_error("chown($(repr(path)), $owner, $group)", err)
    path
end


# - http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_statfs (libuv function docs)
# - http://docs.libuv.org/en/v1.x/fs.html#c.uv_statfs_t (libuv docs of the returned struct)
"""
    DiskStat

Stores information about the disk in bytes. Populate by calling `diskstat`.
"""
struct DiskStat
    ftype::UInt64
    bsize::UInt64
    blocks::UInt64
    bfree::UInt64
    bavail::UInt64
    files::UInt64
    ffree::UInt64
    fspare::NTuple{4, UInt64} # reserved
end

function Base.getproperty(stats::DiskStat, field::Symbol)
    total = Int64(getfield(stats, :bsize) * getfield(stats, :blocks))
    available = Int64(getfield(stats, :bsize) * getfield(stats, :bavail))
    field === :total && return total
    field === :available && return available
    field === :used && return total - available
    return getfield(stats, field)
end

@eval Base.propertynames(stats::DiskStat) =
    $((fieldnames(DiskStat)[1:end-1]..., :available, :total, :used))

Base.show(io::IO, x::DiskStat) =
    print(io, "DiskStat(total=$(x.total), used=$(x.used), available=$(x.available))")

"""
    diskstat(path=pwd())

Return statistics in bytes about the disk that contains the file or directory pointed at by
`path`. If no argument is passed, statistics about the disk that contains the current
working directory are returned.

!!! compat "Julia 1.8"
    This method was added in Julia 1.8.
"""
function diskstat(path::AbstractString=pwd())
    req = zeros(UInt8, _sizeof_uv_fs)
    err = ccall(:uv_fs_statfs, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
                C_NULL, req, path, C_NULL)
    err < 0 && uv_error("diskstat($(repr(path)))", err)
    statfs_ptr = ccall(:jl_uv_fs_t_ptr, Ptr{Nothing}, (Ptr{Cvoid},), req)

    return unsafe_load(reinterpret(Ptr{DiskStat}, statfs_ptr))
end
