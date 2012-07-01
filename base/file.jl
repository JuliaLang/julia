# get and set current directory

function cwd()
    b = Array(Uint8,1024)
    p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    system_error("getcwd", p == C_NULL)
    cstring(p)
end

cd(dir::String) = system_error("chdir", ccall(:chdir,Int32,(Ptr{Uint8},),dir) == -1)
cd() = cd(ENV["HOME"])

# do stuff in a directory, then return to current directory

function cd(f::Function, dir::String)
    fd = ccall(:open,Int32,(Ptr{Uint8},Int32),".",0)
    system_error("open", fd == -1)
    try
        cd(dir)
        retval = f()
        system_error("fchdir", ccall(:fchdir,Int32,(Int32,),fd) != 0)
        retval
    catch err
        system_error("fchdir", ccall(:fchdir,Int32,(Int32,),fd) != 0)
        throw(err)
    end
end
cd(f::Function) = cd(f, ENV["HOME"])

# list the contents of a directory

ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)

# hacks to implement R style file operations if UNIX shell commands are available

function basename(path::String)
  os_separator = "/"
  components = split(path, os_separator)
  k = length(components)
  strcat(join(components[1:(k - 1)], os_separator), os_separator)
end

function dirname(path::String)
  os_separator = "/"
  components = split(path, os_separator)
  k = length(components)
  join(components[1:(k - 1)], os_separator)
end

function file_path(components...)
  os_separator = "/"
  join(components, os_separator)
end

function path_expand(path::String)
  chomp(readlines(`bash -c "echo $path"`)[1])
end

function file_copy(source::String, destination::String)
  run(`cp $source $destination`)
end

function file_create(filename::String)
  run(`touch $filename`)
end

function file_remove(filename::String)
  run(`rm $filename`)
end

function path_rename(old_pathname::String, new_pathname::String)
  run(`mv $old_pathname $new_pathname`)
end

function dir_create(directory_name::String)
  run(`mkdir $directory_name`)
end

function dir_remove(directory_name::String)
  run(`rmdir $directory_name`)
end

function file_exists(filename::String)
  if length(readlines(`ls $filename`)) != 0
    true
  else
    false
  end
end

function tempdir()
  chomp(readall(`mktemp -d -t tmp`))
end

function tempfile()
  chomp(readall(`mktemp -t tmp`))
end

function download_file(url::String)
  filename = tempfile()
  run(`curl -o $filename $url`)
  new_filename = strcat(filename, ".tar.gz")
  path_rename(filename, new_filename)
  new_filename
end



# File information
function isfile(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return S_ISREG(stat_mode(buf))
    else
        return false
    end
end

function isdir(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return S_ISDIR(stat_mode(buf))
    else
        return false
    end
end

function islink(filename::ASCIIString)
    buf = statbuf_allocate()
    if lstat(filename, buf) == 0
        return S_ISLNK(stat_mode(buf))
    else
        return false
    end
end

function isreadable(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return stat_mode(buf) & S_IRUSR > 0
    else
        error("Error accessing file ", filename)
    end
end

function iswriteable(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return stat_mode(buf) & S_IWUSR > 0
    else
        error("Error accessing file ", filename)
    end
end

function isexecutable(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return stat_mode(buf) & S_IXUSR > 0
    else
        error("Error accessing file ", filename)
    end
end

function filesize(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return stat_size(buf)      # in bytes
    else
        error("Error accessing file ", filename)
    end
end

function mtime(filename::ASCIIString)
    buf = statbuf_allocate()
    if stat(filename, buf) == 0
        return stat_mtime(buf)
        error("Error accessing file ", filename)
    end
end

function abs_path(fname::String)
    if fname[1] == '/'
        comp = split(fname, '/')
    else
        comp = [split(cwd(), '/'), split(fname, '/')]
    end
    i = 2
    while i <= length(comp)
        if comp[i] == "."
            del(comp, i)
            continue
        elseif comp[i] == ".."
            if i <= 2
                error("invalid path")
            end
            i -= 1
            del(comp, i)
            del(comp, i)
            continue
        end
        i += 1
    end
    return join(comp, '/')
end

# Core functions: stat and friends
# Allocate a buffer for storing the results
function statbuf_allocate()
    return Array(Uint8, ccall(:jl_sizeof_stat, Int, ()))
end

function stat(pathname::ASCIIString, buf::Vector{Uint8})
    return ccall(:jl_stat, Int32, (Ptr{Uint8}, Ptr{Uint8}), pathname, buf)
end

function lstat(pathname::ASCIIString, buf::Vector{Uint8})
    return ccall(:jl_lstat, Int32, (Ptr{Uint8}, Ptr{Uint8}), pathname, buf)
end

function fstat(fd::Integer, buf::Vector{Uint8})
    return ccall(:jl_fstat, Int32, (Int, Ptr{Uint8}), fd, buf)
end

# Raw access functions
# These access the individual elements of the stat buffer
function stat_dev(buf::Vector{Uint8})
    return ccall(:jl_stat_dev, Uint, (Ptr{Uint8},), buf)
end

function stat_ino(buf::Vector{Uint8})
    return ccall(:jl_stat_ino, Uint, (Ptr{Uint8},), buf)
end

function stat_mode(buf::Vector{Uint8})
    return ccall(:jl_stat_mode, Uint, (Ptr{Uint8},), buf)
end

function stat_nlink(buf::Vector{Uint8})
    return ccall(:jl_stat_nlink, Uint, (Ptr{Uint8},), buf)
end

function stat_uid(buf::Vector{Uint8})
    return ccall(:jl_stat_uid, Uint, (Ptr{Uint8},), buf)
end

function stat_gid(buf::Vector{Uint8})
    return ccall(:jl_stat_gid, Uint, (Ptr{Uint8},), buf)
end

function stat_rdev(buf::Vector{Uint8})
    return ccall(:jl_stat_rdev, Uint, (Ptr{Uint8},), buf)
end

function stat_size(buf::Vector{Uint8})
    return ccall(:jl_stat_size, Uint, (Ptr{Uint8},), buf)
end

function stat_blksize(buf::Vector{Uint8})
    return ccall(:jl_stat_blksize, Uint, (Ptr{Uint8},), buf)
end

function stat_blocks(buf::Vector{Uint8})
    return ccall(:jl_stat_blocks, Uint, (Ptr{Uint8},), buf)
end

function stat_mtime(buf::Vector{Uint8})
    return ccall(:jl_stat_mtime, Float64, (Ptr{Uint8},), buf)
end

function stat_ctime(buf::Vector{Uint8})
    return ccall(:jl_stat_ctime, Float64, (Ptr{Uint8},), buf)
end

## Interpreting the meaning of the different fields
S_IFMT(mode) = mode & 0xf000
S_IMODE(mode) = mode & 0x0fff
# Type information
const S_IFIFO = 0x1000
const S_IFCHR = 0x2000
const S_IFDIR = 0x4000
const S_IFBLK = 0x6000
const S_IFREG = 0x8000
const S_IFLNK = 0xa000
const S_IFSOCK = 0xc000

S_ISFIFO(mode) = S_IFMT(mode) == S_IFFIFO
S_ISCHR(mode) = S_IFMT(mode) == S_IFCHR
S_ISDIR(mode) = S_IFMT(mode) == S_IFDIR
S_ISBLK(mode) = S_IFMT(mode) == S_IFBLK
S_ISREG(mode) = S_IFMT(mode) == S_IFREG
S_ISLNK(mode) = S_IFMT(mode) == S_IFLNK
S_ISSOCK(mode) = S_IFMT(mode) == S_IFSOCK

# Permission information
const S_ISUID = 0x800
const S_ISGID = 0x400
const S_ISVTX = 0x200

const S_IRWXU = 0x1c0
const S_IRUSR = 0x100
const S_IWUSR = 0x080
const S_IXUSR = 0x040

const S_IRWXG = 0x038
const S_IRGRP = 0x020
const S_IWGRP = 0x010
const S_IXGRP = 0x008

const S_IRWXO = 0x007
const S_IROTH = 0x004
const S_IWOTH = 0x002
const S_IXOTH = 0x001

const S_IREAD = S_IRUSR
const S_IWRITE = S_IWUSR
const S_IEXEC = S_IXUSR
