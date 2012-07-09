# Even though we provide the PATH_SEPARATOR constant, we encourage the usage of '/' as
# a path seperator on all plaform (The windows api sees '\\' and '/' as equivalent
@unix_only 		const PATH_SEPARATOR = '/'
@windows_only	const PATH_SEPARATOR = '\\'

# returns the path to the system temp directory
function systmpdir()
	@unix_only return "/tmp"
	@windows_only return ENV["TEMP"]
end

# get and set current directory

function cwd()
    b = Array(Uint8,1024)
    @unix_only p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
	@windows_only p = ccall(:_getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    system_error("cwd", p==C_NULL)
    cstring(p)
end

cd(dir::String) = system_error("chdir", ccall(:uv_chdir,Int32,(Ptr{Uint8},),dir) == -1)
cd() = cd(ENV["HOME"])

# do stuff in a directory, then return to current directory

@unix_only begin
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
end

@windows_only begin
function cd(f::Function, dir::String)
    old = cwd()
    try
        cd(dir)
        retval = f()
        cd(old)
        retval
    catch err
        cd(old)
        throw(err)
    end
end
end

cd(f::Function) = cd(f, ENV["HOME"])

# list the contents of a directory

@unix_only begin
ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)
end

@windows_only begin
ls() = run(`dir /Q /S`)
end

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

@unix_only begin
function real_path(fname::String)
    sp = ccall(:realpath, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}), fname, C_NULL)
    system_error(:real_path, sp == C_NULL)
    s = cstring(sp)
    ccall(:free, Void, (Ptr{Uint8},), sp)
    return s
end
end

@windows_only begin
const PATH_MAX=4096
function real_path(fname::String)
    path = Array(Uint8,PATH_MAX)
    size = ccall(:GetFullPathNameA,stdcall,Uint32,(Ptr{Uint8},Int32,Ptr{Uint8},Ptr{Uint8}),fname,PATH_MAX,path,0)
    if(size == 0)
        error("real_path: Failed to get real path") #TODO: better, unified (with unix) error reporting
    elseif(size < PATH_MAX)
        return convert(ASCIIString,grow(path,size-PATH_MAX)) #Shrink buffer to needed space and convert to ASCIIString
    else
        grow(path,size-PATH_MAX)
        size = ccall(:GetFullPathNameA,stdcall,Uint32,(Ptr{Uint8},Int32,Ptr{Uint8},Ptr{Uint8}),fname,PATH_MAX,path,0)
        if(size == 0)
            error("real_path: Failed to get real path (long path)")
        end
        return convert(ASCIIString,path)
    end
end
end

function abs_path(fname::String)
    if length(fname) > 0 && fname[1] == '/'
        comp = split(fname, '/')
    else
        comp = [split(cwd(), '/'), split(fname, '/')]
    end
    n = length(comp)
    pmask = trues(n)
    last_is_dir = false
    for i = 2:n
        if comp[i] == "." || comp[i] == ""
            pmask[i] = false
            last_is_dir = true
        elseif comp[i] == ".."
            pmask[i] = false
            last_is_dir = true
            for j = i-1:-1:2
                if pmask[j]
                    pmask[j] = false
                    break
                end
            end
        else
            last_is_dir = false
        end
    end
    comp = comp[pmask]
    if last_is_dir
        push(comp, "")
    end
    return join(comp, '/')
end
