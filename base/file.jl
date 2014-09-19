# get and set current directory

function pwd()
    b = Array(Uint8,1024)
    len = Csize_t[length(b),]
    uv_error(:getcwd, ccall(:uv_cwd, Cint, (Ptr{Uint8}, Ptr{Csize_t}), b, len))
    bytestring(b[1:len[1]-1])
end

function cd(dir::String)
    uv_error("chdir $dir", ccall(:uv_chdir, Cint, (Ptr{Uint8},), dir))
end
cd() = cd(homedir())

@unix_only function cd(f::Function, dir::String, args...)
    fd = ccall(:open,Int32,(Ptr{Uint8},Int32),".",0)
    systemerror(:open, fd == -1)
    try
        cd(dir)
        f(args...)
    finally
        systemerror(:fchdir, ccall(:fchdir,Int32,(Int32,),fd) != 0)
        systemerror(:close, ccall(:close,Int32,(Int32,),fd) != 0)
    end
end
@windows_only function cd(f::Function, dir::String, args...)
    old = pwd()
    try
        cd(dir)
        f(args...)
   finally
        cd(old)
    end
end
cd(f::Function) = cd(f, homedir())

function mkdir(path::String, mode::Unsigned=0o777)
    @unix_only ret = ccall(:mkdir, Int32, (Ptr{Uint8},Uint32), path, mode)
    @windows_only ret = ccall(:_wmkdir, Int32, (Ptr{Uint16},), utf16(path))
    systemerror(:mkdir, ret != 0)
end

function mkpath(path::String, mode::Unsigned=0o777)
    isdirpath(path) && (path = dirname(path))
    dir = dirname(path)
    (path == dir || isdir(path)) && return
    mkpath(dir, mode)
    mkdir(path, mode)
end

mkdir(path::String, mode::Signed) = error("mode must be an unsigned integer; try 0o$mode")
mkpath(path::String, mode::Signed) = error("mode must be an unsigned integer; try 0o$mode")

function rm(path::String; recursive::Bool=false)
    if islink(path) || !isdir(path)
        @windows_only if !iswritable(path); chmod(path, 0o777); end
        FS.unlink(path)
    else
        if recursive
            for p in readdir(path)
                rm(joinpath(path, p), recursive=true)
            end
        end
        @unix_only ret = ccall(:rmdir, Int32, (Ptr{Uint8},), path)
        @windows_only ret = ccall(:_wrmdir, Int32, (Ptr{Uint16},), utf16(path))
        systemerror(:rmdir, ret != 0)
    end
end


# The following use Unix command line facilites

cp(src::String, dst::String) = FS.sendfile(src, dst)
mv(src::String, dst::String) = FS.rename(src, dst)
touch(path::String) = run(`touch $path`)

# Obtain a temporary filename.
@unix_only function tempname()
    d = get(ENV, "TMPDIR", C_NULL) # tempnam ignores TMPDIR on darwin
    p = ccall(:tempnam, Ptr{Uint8}, (Ptr{Uint8},Ptr{Uint8}), d, "julia")
    systemerror(:tempnam, p == C_NULL)
    s = bytestring(p)
    c_free(p)
    return s
end

# Obtain a temporary directory's path.
@unix_only tempdir() = dirname(tempname())

# Create and return the name of a temporary file along with an IOStream
@unix_only function mktemp()
    b = joinpath(tempdir(), "tmpXXXXXX")
    p = ccall(:mkstemp, Int32, (Ptr{Uint8}, ), b) # modifies b
    return (b, fdio(p, true))
end

@windows_only begin
function tempdir()
    temppath = Array(Uint16,32767)
    lentemppath = ccall(:GetTempPathW,stdcall,Uint32,(Uint32,Ptr{Uint16}),length(temppath),temppath)
    if lentemppath >= length(temppath) || lentemppath == 0
        error("GetTempPath failed: $(FormatMessage())")
    end
    resize!(temppath,lentemppath+1)
    return utf8(UTF16String(temppath))
end
tempname(uunique::Uint32=uint32(0)) = tempname(tempdir(), uunique)
function tempname(temppath::String,uunique::Uint32)
    tname = Array(Uint16,32767)
    uunique = ccall(:GetTempFileNameW,stdcall,Uint32,(Ptr{Uint16},Ptr{Uint16},Uint32,Ptr{Uint16}),
        utf16(temppath),utf16("jul"),uunique,tname)
    lentname = findfirst(tname,0)-1
    if uunique == 0 || lentname <= 0
        error("GetTempFileName failed: $(FormatMessage())")
    end
    resize!(tname,lentname+1)
    return utf8(UTF16String(tname))
end
function mktemp()
    filename = tempname()
    return (filename, open(filename,"r+"))
end
end

# Create and return the name of a temporary directory
@unix_only function mktempdir()
    b = joinpath(tempdir(), "tmpXXXXXX")
    p = ccall(:mkdtemp, Ptr{Uint8}, (Ptr{Uint8}, ), b)
    return bytestring(p)
end

@windows_only function mktempdir()
    seed::Uint32 = rand(Uint32)
    dir = tempdir()
    while true
        if uint16(seed) == 0
            seed += 1
        end
        filename = tempname(dir, seed)
        ret = ccall(:_wmkdir, Int32, (Ptr{Uint16},), utf16(filename))
        if ret == 0
            return filename
        end
        systemerror(:mktempdir, errno()!=EEXIST)
        seed += 1
    end
end

function readdir(path::String)
    # Allocate space for uv_fs_t struct
    uv_readdir_req = zeros(Uint8, ccall(:jl_sizeof_uv_fs_t, Int32, ()))

    # defined in sys.c, to call uv_fs_readdir, which sets errno on error.
    file_count = ccall(:jl_readdir, Int32, (Ptr{Uint8}, Ptr{Uint8}),
                        path, uv_readdir_req)
    systemerror("unable to read directory $path", file_count < 0)

    # The list of dir entries is returned as a contiguous sequence of null-terminated
    # strings, the first of which is pointed to by ptr in uv_readdir_req.
    # The following lines extracts those strings into dirent
    entries = ByteString[]
    offset = 0

    for i = 1:file_count
        entry = bytestring(ccall(:jl_uv_fs_t_ptr_offset, Ptr{Uint8},
                                 (Ptr{Uint8}, Int32), uv_readdir_req, offset))
        push!(entries, entry)
        offset += sizeof(entry) + 1   # offset to the next entry
    end

    # Clean up the request string
    ccall(:jl_uv_fs_req_cleanup, Void, (Ptr{Uint8},), uv_readdir_req)

    entries
end

readdir() = readdir(".")
