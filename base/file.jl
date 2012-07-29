# File and path name manipulation
# These do not examine the filesystem at all, they just work on strings
@unix_only begin
    const os_separator = "/"
    const os_separator_match = "/"
    const os_separator_match_chars = "/"
end
@windows_only begin
    const os_separator = "\\"
    const os_separator_match = "[/\\\\]" # permit either slash type on Windows
    const os_separator_match_chars = "/\\" # to permit further concatenation
end
# Match only the final separator
const last_separator = Regex(strcat(os_separator_match, "(?!.*", os_separator_match, ")"))
# Match the "." indicating a file extension. Must satisfy the
# following requirements:
#   - It's not followed by a later "." or os_separator
#     (handles cases like myfile.txt.gz, or Mail.directory/cur)
#   - It's not the first character in a string, nor is it preceded by
#     an os_separator (handles cases like .bashrc or /home/fred/.juliarc)
const extension_separator_match = Regex(strcat("(?<!^)(?<!",
    os_separator_match, ")\\.(?!.*[", os_separator_match_chars, "\.])"))
# Match ~/filename
const plain_tilde = Regex(strcat("^~", os_separator_match))
# Match ~user/filename
const user_tilde = r"^~\w"

filesep() = os_separator

# returns the path to the system temp directory
function systmpdir()
        @unix_only return "/tmp"
        @windows_only return ENV["TEMP"]
end

function basename(path::String)
    m = match(last_separator, path)
    if m == nothing
        return path
    else
        return path[m.offset+1:end]
    end
end

function dirname(path::String)
    m = match(last_separator, path)
    if m == nothing
        return ""
    else
        return path[1:m.offset-1]
    end
end

function dirname_basename(path::String)
    m = match(last_separator, path)
    if m == nothing
        return "", path
    else
        return path[1:m.offset-1], path[m.offset+1:end]
    end
end

function split_extension(path::String)
    m = match(extension_separator_match, path)
    if m == nothing
        return path, ""
    else
        return path[1:m.offset-1], path[m.offset:end]
    end
end

split_path(path::String) = split(path, os_separator_match)

function fileparts(filename::String)
    pathname, filestr = dirname_basename(filename)
    filebase, ext = split_extension(filestr)
    return pathname, filebase, ext
end

function file_path(components...)
    join(components, os_separator)
end

function fullfile(pathname::String, basename::String, ext::String)
    if isempty(pathname)
        return basename * ext
    else
        return pathname * os_separator * basename * ext
    end
end

# Test for an absolute path
function isrooted(path::String)
    # Check whether it begins with the os_separator. On Windows, matches
    # \\servername syntax, so this is a relevant check for everyone
    m = match(Regex(strcat("^", os_separator_match)), path)
    if m != nothing
        return true
    end
    @windows_only begin
        m = match(r"^\w+:", path)
        if m != nothing
            return true
        end
    end
    false
end

global tilde_expand
function tilde_expand(path::String)
    @windows_only return path  # on windows, ~ means "temporary file"
    @unix_only begin
        m = match(user_tilde, path)
        if m != nothing
            return "/home/"*path[2:end]
        end
    end
    m = match(plain_tilde, path)
    if m != nothing
        return ENV["HOME"]*path[2:end]
    end
    path
end

# Get the absolute path to a file. Uses file system for cwd() when
# needed, the rest is all string manipulation. In particular, it
# doesn't check whether the file exists.
function abs_path_split(fname::String)
    fname = tilde_expand(fname)
    if isrooted(fname)
        comp = split(fname, os_separator_match)
    else
        comp = [split(cwd(), os_separator_match), split(fname, os_separator_match)]
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
    return comp
end
function abs_path(fname::String)
    comp = abs_path_split(fname)
    return join(comp, os_separator)
end


# The remaining commands use the file system in some way

# Get the full, real path to a file, including dereferencing
# symlinks.
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


# The following use Unix command line facilites

# list the contents of a directory
ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)

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
