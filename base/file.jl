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

function ispath(filename::String)
  ispath(stat(filename))
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

function real_path(fname::String)
    sp = ccall(:realpath, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}), fname, C_NULL)
    system_error(:real_path, sp == C_NULL)
    s = cstring(sp)
    ccall(:free, Void, (Ptr{Uint8},), sp)
    return s
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
