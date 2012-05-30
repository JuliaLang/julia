# get and set current directory

function cwd()
    b = Array(Uint8,1024)
    p = ccall(:getcwd, Ptr{Uint8}, (Ptr{Uint8}, Uint), b, length(b))
    system_error("cwd", p==C_NULL)
    cstring(p)
end

cd(dir::String) = (system_error("cd", ccall(:chdir, Int32, (Ptr{Uint8},), dir)==-1); cwd())
cd() = cd(ENV["HOME"])

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

# list the contents of a directory

ls() = run(`ls -l`)
ls(args::Cmd) = run(`ls -l $args`)
ls(args::String...) = run(`ls -l $args`)
