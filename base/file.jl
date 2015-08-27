# This file is a part of Julia. License is MIT: http://julialang.org/license

# get and set current directory

function pwd()
    b = Array(UInt8,1024)
    len = Csize_t[length(b),]
    uv_error(:getcwd, ccall(:uv_cwd, Cint, (Ptr{UInt8}, Ptr{Csize_t}), b, len))
    bytestring(b[1:len[1]-1])
end

function cd(dir::AbstractString)
    uv_error("chdir $dir", ccall(:uv_chdir, Cint, (Cstring,), dir))
end
cd() = cd(homedir())

@unix_only function cd(f::Function, dir::AbstractString)
    fd = ccall(:open,Int32,(Ptr{UInt8},Int32),".",0)
    systemerror(:open, fd == -1)
    try
        cd(dir)
        f()
    finally
        systemerror(:fchdir, ccall(:fchdir,Int32,(Int32,),fd) != 0)
        systemerror(:close, ccall(:close,Int32,(Int32,),fd) != 0)
    end
end
@windows_only function cd(f::Function, dir::AbstractString)
    old = pwd()
    try
        cd(dir)
        f()
   finally
        cd(old)
    end
end
cd(f::Function) = cd(f, homedir())

function mkdir(path::AbstractString, mode::Unsigned=0o777)
    @unix_only ret = ccall(:mkdir, Int32, (Cstring,UInt32), path, mode)
    @windows_only ret = ccall(:_wmkdir, Int32, (Cwstring,), path)
    systemerror(:mkdir, ret != 0)
end

function mkpath(path::AbstractString, mode::Unsigned=0o777)
    isdirpath(path) && (path = dirname(path))
    dir = dirname(path)
    (path == dir || isdir(path)) && return
    mkpath(dir, mode)
    mkdir(path, mode)
end

mkdir(path::AbstractString, mode::Signed) = throw(ArgumentError("mode must be an unsigned integer; try 0o$mode"))
mkpath(path::AbstractString, mode::Signed) = throw(ArgumentError("mode must be an unsigned integer; try 0o$mode"))

function rm(path::AbstractString; recursive::Bool=false)
    if islink(path) || !isdir(path)
        @windows_only if !iswritable(path); chmod(path, 0o777); end
        FS.unlink(path)
    else
        if recursive
            for p in readdir(path)
                rm(joinpath(path, p), recursive=true)
            end
        end
        @unix_only ret = ccall(:rmdir, Int32, (Cstring,), path)
        @windows_only ret = ccall(:_wrmdir, Int32, (Cwstring,), path)
        systemerror(:rmdir, ret != 0)
    end
end


# The following use Unix command line facilites
function checkfor_mv_cp_cptree(src::AbstractString, dst::AbstractString, txt::AbstractString;
                                                          remove_destination::Bool=false)
    if ispath(dst)
        if remove_destination
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
            throw(ArgumentError(string("'$dst' exists. `remove_destination=true` ",
                                       "is required to remove '$dst' before $(txt).")))
        end
    end
end

function cptree(src::AbstractString, dst::AbstractString; remove_destination::Bool=false,
                                                             follow_symlinks::Bool=false)
    isdir(src) || throw(ArgumentError("'$src' is not a directory. Use `cp(src, dst)`"))
    checkfor_mv_cp_cptree(src, dst, "copying"; remove_destination=remove_destination)
    mkdir(dst)
    for name in readdir(src)
        srcname = joinpath(src, name)
        if !follow_symlinks && islink(srcname)
            symlink(readlink(srcname), joinpath(dst, name))
        elseif isdir(srcname)
            cptree(srcname, joinpath(dst, name); remove_destination=remove_destination,
                                                 follow_symlinks=follow_symlinks)
        else
            FS.sendfile(srcname, joinpath(dst, name))
        end
    end
end

function cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false,
                                                         follow_symlinks::Bool=false)
    checkfor_mv_cp_cptree(src, dst, "copying"; remove_destination=remove_destination)
    if !follow_symlinks && islink(src)
        symlink(readlink(src), dst)
    elseif isdir(src)
        cptree(src, dst; remove_destination=remove_destination, follow_symlinks=follow_symlinks)
    else
        FS.sendfile(src, dst)
    end
end

function mv(src::AbstractString, dst::AbstractString; remove_destination::Bool=false)
    checkfor_mv_cp_cptree(src, dst, "moving"; remove_destination=remove_destination)
    FS.rename(src, dst)
end

function touch(path::AbstractString)
    f = FS.open(path,JL_O_WRONLY | JL_O_CREAT, 0o0666)
    @assert f.handle >= 0
    try
        t = time()
        futime(f,t,t)
    finally
        FS.close(f)
    end
end

@unix_only begin
# Obtain a temporary filename.
function tempname()
    d = get(ENV, "TMPDIR", C_NULL) # tempnam ignores TMPDIR on darwin
    p = ccall(:tempnam, Ptr{UInt8}, (Ptr{UInt8},Ptr{UInt8}), d, "julia")
    systemerror(:tempnam, p == C_NULL)
    s = bytestring(p)
    Libc.free(p)
    return s
end

# Obtain a temporary directory's path.
tempdir() = dirname(tempname())

# Create and return the name of a temporary file along with an IOStream
function mktemp(parent=tempdir())
    b = joinpath(parent, "tmpXXXXXX")
    p = ccall(:mkstemp, Int32, (Ptr{UInt8},), b) # modifies b
    systemerror(:mktemp, p == -1)
    return (b, fdio(p, true))
end

# Create and return the name of a temporary directory
function mktempdir(parent=tempdir())
    b = joinpath(parent, "tmpXXXXXX")
    p = ccall(:mkdtemp, Ptr{UInt8}, (Ptr{UInt8},), b)
    systemerror(:mktempdir, p == C_NULL)
    return bytestring(p)
end
end

@windows_only begin
function tempdir()
    temppath = Array(UInt16,32767)
    lentemppath = ccall(:GetTempPathW,stdcall,UInt32,(UInt32,Ptr{UInt16}),length(temppath),temppath)
    if lentemppath >= length(temppath) || lentemppath == 0
        error("GetTempPath failed: $(Libc.FormatMessage())")
    end
    resize!(temppath,lentemppath+1)
    return utf8(UTF16String(temppath))
end
tempname(uunique::UInt32=UInt32(0)) = tempname(tempdir(), uunique)
function tempname(temppath::AbstractString,uunique::UInt32)
    tname = Array(UInt16,32767)
    uunique = ccall(:GetTempFileNameW,stdcall,UInt32,(Cwstring,Ptr{UInt16},UInt32,Ptr{UInt16}), temppath,utf16("jul"),uunique,tname)
    lentname = findfirst(tname,0)-1
    if uunique == 0 || lentname <= 0
        error("GetTempFileName failed: $(Libc.FormatMessage())")
    end
    resize!(tname,lentname+1)
    return utf8(UTF16String(tname))
end
function mktemp(parent=tempdir())
    filename = tempname(parent, UInt32(0))
    return (filename, open(filename,"r+"))
end
function mktempdir(parent=tempdir())
    seed::UInt32 = rand(UInt32)
    while true
        if (seed & typemax(UInt16)) == 0
            seed += 1
        end
        filename = tempname(parent, seed)
        ret = ccall(:_wmkdir, Int32, (Ptr{UInt16},), utf16(filename))
        if ret == 0
            return filename
        end
        systemerror(:mktempdir, Libc.errno()!=Libc.EEXIST)
        seed += 1
    end
end
end

function mktemp(fn::Function, parent=tempdir())
    (tmp_path, tmp_io) = mktemp(parent)
    try
        fn(tmp_path, tmp_io)
    finally
        close(tmp_io)
        rm(tmp_path)
    end
end

function mktempdir(fn::Function, parent=tempdir())
    tmpdir = mktempdir(parent)
    try
        fn(tmpdir)
    finally
        rm(tmpdir, recursive=true)
    end
end

function readdir(path::AbstractString)
    # Allocate space for uv_fs_t struct
    uv_readdir_req = zeros(UInt8, ccall(:jl_sizeof_uv_fs_t, Int32, ()))

    # defined in sys.c, to call uv_fs_readdir, which sets errno on error.
    file_count = ccall(:jl_readdir, Int32, (Cstring, Ptr{UInt8}),
                        path, uv_readdir_req)
    systemerror("unable to read directory $path", file_count < 0)

    # The list of dir entries is returned as a contiguous sequence of null-terminated
    # strings, the first of which is pointed to by ptr in uv_readdir_req.
    # The following lines extracts those strings into dirent
    entries = UTF8String[]
    offset = 0

    for i = 1:file_count
        entry = bytestring(ccall(:jl_uv_fs_t_ptr_offset, Ptr{UInt8},
                                 (Ptr{UInt8}, Int32), uv_readdir_req, offset))
        push!(entries, entry)
        offset += sizeof(entry) + 1   # offset to the next entry
    end

    # Clean up the request string
    ccall(:jl_uv_fs_req_cleanup, Void, (Ptr{UInt8},), uv_readdir_req)

    entries
end

readdir() = readdir(".")
