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


# Core functions: stat and friends
type Stat
    dev::Int64
    ino::Int64
    mode::Int64
    nlink::Int64
    uid::Int64
    gid::Int64
    rdev::Int64
    size::Int64
    blksize::Int64
    blocks::Int64
    atime_sec::Int64
    atime_nsec::Int64
    mtime_sec::Int64
    mtime_nsec::Int64
    ctime_sec::Int64
    ctime_nsec::Int64
end

stat_check(filename::ASCIIString) = stat_check(stat(filename), filename)
stat_check(s::Stat, filename::ASCIIString) = s
stat_check(s::Int64, filename::ASCIIString) = error("Error accessing file: ", filename)

function stat(pathname::ASCIIString)
    return ccall(:jl_stat, Union(Stat,Int64), (Ptr{Uint8},), pathname)
end

function lstat(pathname::ASCIIString)
    return ccall(:jl_lstat, Union(Stat,Int64), (Ptr{Uint8},), pathname)
end

function fstat(fd::Integer)
    return ccall(:jl_fstat, Union(Stat,Int64), (Int,), fd)
end

## Interpreting the meaning of the different fields
S_IFMT(s::Stat) = s.mode & 0xf000
S_IMODE(s::Stat) = s.mode & 0x0fff
# Type information
const S_IFIFO = 0x1000
const S_IFCHR = 0x2000
const S_IFDIR = 0x4000
const S_IFBLK = 0x6000
const S_IFREG = 0x8000
const S_IFLNK = 0xa000
const S_IFSOCK = 0xc000

S_ISFIFO(s::Stat) = S_IFMT(s) == S_IFFIFO
S_ISCHR(s::Stat) = S_IFMT(s) == S_IFCHR
S_ISDIR(s::Stat) = S_IFMT(s) == S_IFDIR
S_ISBLK(s::Stat) = S_IFMT(s) == S_IFBLK
S_ISREG(s::Stat) = S_IFMT(s) == S_IFREG
S_ISLNK(s::Stat) = S_IFMT(s) == S_IFLNK
S_ISSOCK(s::Stat) = S_IFMT(s) == S_IFSOCK

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

# File information
isfile(filename::ASCIIString) = isfile(stat(filename))
isfile(s::Int64) = false
isfile(s::Stat) = S_ISREG(s)
   
isdir(filename::ASCIIString) = isdir(stat(filename))
isdir(s::Int64) = false
isdir(s::Stat) = S_ISDIR(s)

islink(filename::ASCIIString) = islink(stat(filename))
islink(s::Int64) = false
islink(s::Stat) = S_ISLNK(s)

isreadable(filename::ASCIIString) = isreadable(stat_check(filename))
isreadable(s::Stat) = s.mode & S_IRUSR > 0

iswriteable(filename::ASCIIString) = iswriteable(stat_check(filename))
iswriteable(s::Stat) = s.mode & S_IWUSR > 0

isexecutable(filename::ASCIIString) = isexecutable(stat_check(filename))
isexecutable(s::Stat) = s.mode & S_IXUSR > 0

filesize(filename::ASCIIString) = filesize(stat_check(filename))
filesize(s::Stat) = s.size

mtime(filename::ASCIIString) = mtime(stat_check(filename))
mtime(s::Stat) = s.mtime_sec + s.mtime_nsec * 1e-9

## ctime is a constant from version.jl
#ctime(filename::ASCIIString) = ctime(stat_check(filename))
#ctime(s::Stat) = s.ctime_sec + s.ctime_nsec * 1e-9

