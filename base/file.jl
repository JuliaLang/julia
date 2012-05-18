# get and set current directory

function cwd()
    b = Array(Uint8,1024)
    p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    if p == C_NULL
        error("current directory has been deleted (or has a really long name)")
    end
    cstring(p)
end

function cd(dir::String)
    if ccall(:chdir, Int32, (Ptr{Uint8},), dir) == -1
        throw(SystemError("cd($dir)"))
    end
    cwd()
end

# do stuff in a directory, then return to current directory

function cd(f::Function, dir::String)
    old = cwd()
    try
        cd(dir)
        res = f()
        cd(old)
        res
    catch err
        cd(old)
        throw(err)
    end
end

cd(f::Function) = cd(f, ENV["HOME"])
cd() = cd(ENV["HOME"])

macro cd(dir,ex); :(cd(()->$ex,$dir)); end

# list the contents of a directory

ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)
