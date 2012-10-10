# File and path name manipulation
# These do not examine the filesystem at all, they just work on strings
@unix_only begin
    const os_separator = "/"
    const os_separator_match = "/"
    const os_separator_match_chars = "/"
end
@windows_only begin
    const os_separator = "\\"
    const os_separator_match = "[/\\]" # permit either slash type on Windows
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

filesep() = os_separator

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

@windows_only tilde_expand(path::String) = path # on windows, ~ means "temporary file"
@unix_only function tilde_expand(path::String)
    i = start(path)
    c, i = next(path,i)
    if c != '~' return path end
    if done(path,i) return ENV["HOME"] end
    c, j = next(path,i)
    if c == '/' return ENV["HOME"]*path[i:end] end
    error("~user tilde expansion not yet implemented")
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


# Get the full, real path to a file, including dereferencing
# symlinks.
function real_path(fname::String)
    fname = tilde_expand(fname)
    sp = ccall(:realpath, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}), fname, C_NULL)
    if sp == C_NULL
        error("Cannot find ", fname)
    end
    s = bytestring(sp)
    ccall(:free, Void, (Ptr{Uint8},), sp)
    return s
end

# get and set current directory

function cwd()
    b = Array(Uint8,1024)
    p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    system_error("getcwd", p == C_NULL)
    bytestring(p)
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
  filename
end

function readdir(path::String)
  # Allocate space for uv_fs_t struct
  uv_readdir_req = zeros(Uint8, ccall(:jl_sizeof_uv_fs_t, Int, ()))

  # defined in sys.c, to call uv_fs_readdir
  file_count = ccall(:jl_readdir, Int, (Ptr{Uint8}, Ptr{Uint8}), 
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
    push(entries, entry)
    offset += length(entry) + 1   # offset to the next entry
  end

  # Clean up the request string
  ccall(:jl_uv_fs_req_cleanup, Void, (Ptr{Uint8},), uv_readdir_req)

  entries
end

readdir(cmd::Cmd) = readdir(string(cmd)[2:end-1])
readdir() = readdir(".")
