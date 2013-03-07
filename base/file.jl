# get and set current directory

function pwd()
    b = Array(Uint8,1024)
    @unix_only p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    @windows_only p = ccall(:_getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    system_error(:getcwd, p == C_NULL)
    bytestring(p)
end


function cd(dir::String) 
    @windows_only system_error(:_chdir, ccall(:_chdir,Int32,(Ptr{Uint8},),dir) == -1)
    @unix_only system_error(:chdir, ccall(:chdir,Int32,(Ptr{Uint8},),dir) == -1)
end
cd() = cd(ENV["HOME"])

# do stuff in a directory, then return to current directory

@unix_only begin
function cd(f::Function, dir::String)
    fd = ccall(:open,Int32,(Ptr{Uint8},Int32),".",0)
    system_error(:open, fd == -1)
    try
        cd(dir)
        f()
    finally
        system_error(:fchdir, ccall(:fchdir,Int32,(Int32,),fd) != 0)
        system_error(:close, ccall(:close,Int32,(Int32,),fd) != 0)
    end
end
end

@windows_only begin
function cd(f::Function, dir::String)
    old = pwd()
    try
        cd(dir)
        retval = f()
        cd(old)
        retval
    catch err
        cd(old)
        rethrow(err)
    end
end
end

cd(f::Function) = cd(f, ENV["HOME"])

function mkdir(path::String, mode::Unsigned)
    @unix_only ret = ccall(:mkdir, Int32, (Ptr{Uint8},Uint32), bytestring(path), mode)
    @windows_only ret = ccall(:_mkdir, Int32, (Ptr{Uint8},), bytestring(path))
    system_error(:mkdir, ret != 0)
end
mkdir(path::String, mode::Signed) = error("mkdir: mode must be an unsigned integer -- perhaps 0o", mode, "?")
mkdir(path::String) = mkdir(path, 0o777)

function mkpath(path::String, mode)
  dparts = splitdrive(path)
	@windows_only begin 
    path = dparts[1]
    path *= "\\"
  end
  @unix_only if(path[1]=='/'); path = "/"; end
	parts = (split(dparts[2],Base.path_separator_re,false))
	for x in parts
		path=joinpath(path,x)
		if(!isdir(path))
			mkdir(path,mode)
		end
	end
end
mkpath(path::String) = mkpath(path,0o777)

function rmdir(path::String)
    @unix_only ret = ccall(:rmdir, Int32, (Ptr{Uint8},), bytestring(path))
    @windows_only ret = ccall(:_rmdir, Int32, (Ptr{Uint8},), bytestring(path))
    system_error(:rmdir, ret != 0)
end

# The following use Unix command line facilites

# list the contents of a directory
ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)

rm(path::String) = run(`rm $path`)
cp(src::String, dst::String) = run(`cp $src $dst`)
mv(src::String, dst::String) = run(`mv $src $dst`)
touch(path::String) = run(`touch $path`)

# Obtain a temporary filename.
function tempname()
  d = get(ENV, "TMPDIR", C_NULL) # tempnam ignores TMPDIR on darwin
  @unix_only p = ccall(:tempnam, Ptr{Uint8}, (Ptr{Uint8},Ptr{Uint8}), d, "julia")
  @windows_only p = ccall(:_tempnam, Ptr{Uint8}, (Ptr{Uint8},Ptr{Uint8}), d, "julia")
  s = bytestring(p)
  c_free(p)
  s
end

# Obtain a temporary directory's path.
tempdir() = dirname(tempname())

# Create and return the name of a temporary file along with an IOStream
@unix_only function mktemp()
  b = joinpath(tempdir(), "tmpXXXXXX")
  p = ccall(:mkstemp, Int32, (Ptr{Uint8}, ), b)
  return (b, fdio(p, true))
end

@windows_only begin 
function GetTempPath()
  temppath = Array(Uint8,261)
  lentemppath = ccall(:GetTempPathA,stdcall,Uint32,(Uint32,Ptr{Uint8}),length(temppath),temppath)
  if lentemppath >= length(temppath) || lentemppath == 0
      error("GetTempPath failed")
end
  resize!(temppath,lentemppath)
  return convert(ASCIIString,temppath)
end
GetTempFileName(uunique::Uint32) = GetTempFileName(GetTempPath(), uunique)
function GetTempFileName(temppath::String,uunique::Uint32)
  tname = Array(Uint8,261)
  uunique = ccall(:GetTempFileNameA,stdcall,Uint32,(Ptr{Uint8},Ptr{Uint8},Uint32,Ptr{Uint8}),temppath,"julia",uunique,tname)
  lentname = findfirst(tname,0)-1
  if uunique == 0 || lentname <= 0
      error("GetTempFileName failed")
  end
  resize!(tname,lentname)
  return convert(ASCIIString, tname)
end
function mktemp()
  filename = GetTempFileName(uint32(0))
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
  seed = rand(Uint32)
  while true
      filename = GetTempFileName(seed)
      ret = ccall(:_mkdir, Int32, (Ptr{Uint8},), filename)
      if ret == 0
          return filename
end
      system_error(:mktempdir, errno()!=EEXIST)
      seed += 1
  end
end

downloadcmd = nothing
function download(url::String, filename::String)
    global downloadcmd
    if downloadcmd === nothing
        for checkcmd in (:curl, :wget, :fetch)
            if success(`which $checkcmd` > SpawnNullStream())
                downloadcmd = checkcmd
                break
            end
        end
    end
    if downloadcmd == :wget
        run(`wget -O $filename $url`)
    elseif downloadcmd == :curl
        run(`curl -o $filename -L $url`)
    elseif downloadcmd == :fetch
        run(`fetch -f $filename $url`)
    else
        error("No download agent available; install curl, wget, or fetch.")
    end
    filename
end
function download(url::String)
  filename = tempname()
  download(url, filename)
end

function readdir(path::String)
  # Allocate space for uv_fs_t struct
  uv_readdir_req = zeros(Uint8, ccall(:jl_sizeof_uv_fs_t, Int32, ()))

  # defined in sys.c, to call uv_fs_readdir
  file_count = ccall(:jl_readdir, Int32, (Ptr{Uint8}, Ptr{Uint8}),
                     bytestring(path), uv_readdir_req)

  if file_count < 0
    error("Unable to read directory $path.")
  end

  # The list of dir entries is returned as a contiguous sequence of null-terminated
  # strings, the first of which is pointed to by ptr in uv_readdir_req.
  # The following lines extracts those strings into dirent
  entries = String[]
  offset = 0

  for i = 1:file_count
    entry = bytestring(ccall(:jl_uv_fs_t_ptr_offset, Ptr{Uint8}, 
                             (Ptr{Uint8}, Int), uv_readdir_req, offset))
    push!(entries, entry)
    offset += length(entry) + 1   # offset to the next entry
  end

  # Clean up the request string
  ccall(:jl_uv_fs_req_cleanup, Void, (Ptr{Uint8},), uv_readdir_req)

  entries
end

readdir(cmd::Cmd) = readdir(string(cmd)[2:end-1])
readdir() = readdir(".")
