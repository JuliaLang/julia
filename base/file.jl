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
