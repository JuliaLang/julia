# This file is a part of Julia. License is MIT: https://julialang.org/license

#############################################
# Create some temporary files & directories #
#############################################
starttime = time_ns() / 1e9
pwd_ = pwd()
dir = mktempdir()
file = joinpath(dir, "afile.txt")
# like touch, but lets the operating system update the timestamp
# for greater precision on some platforms (windows)
@test close(open(file,"w")) === nothing

subdir = joinpath(dir, "adir")
mkdir(subdir)
subdir2 = joinpath(dir, "adir2")
mkdir(subdir2)
@test_throws Base._UVError("mkdir($(repr(file)); mode=0o777)", Base.UV_EEXIST) mkdir(file)
let err = nothing
    try
        mkdir(file)
    catch err
        io = IOBuffer()
        showerror(io, err)
        @test startswith(String(take!(io)), "IOError: mkdir") && err.code == Base.UV_EEXIST
    end
end

if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    dirlink = joinpath(dir, "dirlink")
    symlink(subdir, dirlink)
    @test stat(dirlink) == stat(subdir)
    @test readdir(dirlink) == readdir(subdir)
    @test map(o->o.names, Base.Filesystem._readdirx(dirlink)) == map(o->o.names, Base.Filesystem._readdirx(subdir))
    @test realpath.(Base.Filesystem._readdirx(dirlink)) == realpath.(Base.Filesystem._readdirx(subdir))

    # relative link
    relsubdirlink = joinpath(subdir, "rel_subdirlink")
    reldir = joinpath("..", "adir2")
    symlink(reldir, relsubdirlink)
    @test stat(relsubdirlink) == stat(subdir2)
    @test readdir(relsubdirlink) == readdir(subdir2)
    @test Base.Filesystem._readdirx(relsubdirlink) == Base.Filesystem._readdirx(subdir2)

    # creation of symlink to directory that does not yet exist
    new_dir = joinpath(subdir, "new_dir")
    foo_file = joinpath(subdir, "new_dir", "foo")
    nedlink = joinpath(subdir, "nonexistent_dirlink")
    symlink("new_dir", nedlink; dir_target=true)
    try
        readdir(nedlink)
        @test false
    catch e
        @test isa(e, Base.IOError)
        # It's surprisingly difficult to know what numeric value this will be across platforms
        # so we'll just check the string representation instead. :(
        @test endswith(e.msg, "(ENOENT)")
    end
    mkdir(new_dir)
    touch(foo_file)
    @test readdir(new_dir) == readdir(nedlink)
    @test realpath.(Base.Filesystem._readdirx(new_dir)) == realpath.(Base.Filesystem._readdirx(nedlink))

    rm(foo_file)
    rm(new_dir)
    rm(nedlink)
end

if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    link = joinpath(dir, "afilesymlink.txt")
    symlink(file, link)
    @test stat(file) == stat(link)

    # relative link
    rellink = joinpath(subdir, "rel_afilesymlink.txt")
    relfile = joinpath("..", "afile.txt")
    symlink(relfile, rellink)
    @test stat(rellink) == stat(file)
end

@testset "hardlink" begin
    link = joinpath(dir, "afilehardlink.txt")
    hardlink(file, link)
    @test stat(file) == stat(link)

    # when the destination exists
    @test_throws Base.IOError hardlink(file, link)

    rm(link)

    # the source file does not exist
    missing_file = joinpath(dir, "for-sure-missing-file.txt")
    @test_throws Base.IOError hardlink(missing_file, link)
end

using Random

@testset "that temp names are actually unique" begin
    temps = [tempname(cleanup=false) for _ = 1:100]
    @test allunique(temps)
    temps = map(1:100) do _
        path, io = mktemp(cleanup=false)
        close(io)
        return path
    end
    @test allunique(temps)
    foreach(temps) do path
        rm(path, force=true)
    end
end

@testset "tempname with parent" begin
    t = tempname()
    @test dirname(t) == tempdir()
    mktempdir() do d
        t = tempname(d)
        @test dirname(t) == d
    end
    @test_throws ArgumentError tempname(randstring())

    # 38873: check that `TMPDIR` being set does not
    # override the parent argument to `tempname`.
    mktempdir() do d
        withenv("TMPDIR"=>tempdir()) do
            t = tempname(d)
            @test dirname(t) == d
        end
    end
    @test_throws ArgumentError tempname(randstring())
end
@testset "tempname with suffix" begin
    @test !isfile(tempname(suffix = "_foo.txt"))
end

child_eval(code::String) = eval(Meta.parse(readchomp(`$(Base.julia_cmd()) -E $code`)))

@testset "mktemp/dir basic cleanup" begin
    # mktemp without cleanup
    t = child_eval("t = mktemp(cleanup=false)[1]; @assert isfile(t); t")
    @test isfile(t)
    rm(t, force=true)
    @test !ispath(t)
    # mktemp with cleanup
    t = child_eval("t = mktemp()[1]; @assert isfile(t); t")
    @test !ispath(t)
    # mktempdir without cleanup
    t = child_eval("t = mktempdir(cleanup=false); touch(joinpath(t, \"file.txt\")); t")
    @test isfile("$t/file.txt")
    rm(t, recursive=true, force=true)
    @test !ispath(t)
    # mktempdir with cleanup
    t = child_eval("t = mktempdir(); touch(joinpath(t, \"file.txt\")); t")
    @test !ispath(t)
    # tempname without cleanup
    t = child_eval("t = tempname(cleanup=false); touch(t); t")
    @test isfile(t)
    rm(t, force=true)
    @test !ispath(t)
    # tempname with cleanup
    t = child_eval("t = tempname(); touch(t); t")
    @test !ispath(t)
end

import Base.Filesystem: TEMP_CLEANUP_MIN, TEMP_CLEANUP_MAX, TEMP_CLEANUP

function with_temp_cleanup(f::Function, n::Int)
    SAVE_TEMP_CLEANUP_MIN = TEMP_CLEANUP_MIN[]
    SAVE_TEMP_CLEANUP_MAX = TEMP_CLEANUP_MAX[]
    SAVE_TEMP_CLEANUP = copy(TEMP_CLEANUP)
    empty!(TEMP_CLEANUP)
    TEMP_CLEANUP_MIN[] = n
    TEMP_CLEANUP_MAX[] = n
    try f()
    finally
        Sys.iswindows() && GC.gc(true)
        for t in keys(TEMP_CLEANUP)
            rm(t, recursive=true, force=true)
        end
        copy!(TEMP_CLEANUP, SAVE_TEMP_CLEANUP)
        TEMP_CLEANUP_MAX[] = SAVE_TEMP_CLEANUP_MAX
        TEMP_CLEANUP_MIN[] = SAVE_TEMP_CLEANUP_MIN
    end
end

function mktempfile(; cleanup=true)
    (file, io) = mktemp(cleanup=cleanup)
    Sys.iswindows() && close(io)
    return file
end

@testset "mktemp/dir cleanup list purging" begin
    n = 12 # cleanup min & max
    @assert n % 2 == n % 3 == 0 # otherwise tests won't work
    with_temp_cleanup(n) do
        @test length(TEMP_CLEANUP) == 0
        @test TEMP_CLEANUP_MAX[] == n
        # for n mktemps, no purging is triggered
        temps = String[]
        for i = 1:n
            t = i % 2 == 0 ? mktempfile() : mktempdir()
            push!(temps, t)
            @test ispath(t)
            @test length(TEMP_CLEANUP) == i
            @test TEMP_CLEANUP_MAX[] == n
            # delete 1/3 of the temp paths
            i % 3 == 0 && rm(t, recursive=true, force=true)
        end
        # without cleanup no purge is triggered
        t = mktempdir(cleanup=false)
        @test isdir(t)
        @test length(TEMP_CLEANUP) == n
        @test TEMP_CLEANUP_MAX[] == n
        rm(t, recursive=true, force=true)
        # purge triggered by next mktemp with cleanup
        t = mktempfile()
        push!(temps, t)
        n′ = 2n÷3 + 1
        @test 2n′ > n
        @test isfile(t)
        @test length(TEMP_CLEANUP) == n′
        @test TEMP_CLEANUP_MAX[] == 2n′
        # remove all temp files
        for t in temps
            rm(t, recursive=true, force=true)
        end
        # for n′ mktemps, no purging is triggered
        for i = 1:n′
            t = i % 2 == 0 ? mktempfile() : mktempdir()
            push!(temps, t)
            @test ispath(t)
            @test length(TEMP_CLEANUP) == n′ + i
            @test TEMP_CLEANUP_MAX[] == 2n′
            # delete 2/3 of the temp paths
            i % 3 != 0 && rm(t, recursive=true, force=true)
        end
        # without cleanup no purge is triggered
        t = mktempfile(cleanup=false)
        @test isfile(t)
        @test length(TEMP_CLEANUP) == 2n′
        @test TEMP_CLEANUP_MAX[] == 2n′
        rm(t, force=true)
        # purge triggered by next mktemp
        t = mktempdir()
        push!(temps, t)
        n′′ = n′÷3 + 1
        @test 2n′′ < n
        @test isdir(t)
        @test length(TEMP_CLEANUP) == n′′
        @test TEMP_CLEANUP_MAX[] == n
    end
end

no_error_logging(f::Function) =
    Base.CoreLogging.with_logger(f, Base.CoreLogging.NullLogger())

@testset "hof mktemp/dir when cleanup is prevented" begin
    d = mktempdir()
    with_temp_cleanup(3) do
        @test length(TEMP_CLEANUP) == 0
        @test TEMP_CLEANUP_MAX[] == 3
        local t, f
        temps = String[]
        # mktemp is normally cleaned up on completion
        mktemp(d) do path, _
            @test isfile(path)
            t = path
        end
        @test !ispath(t)
        @test length(TEMP_CLEANUP) == 0
        @test TEMP_CLEANUP_MAX[] == 3
        # mktemp when cleanup is prevented
        no_error_logging() do
            mktemp(d) do path, _
                @test isfile(path)
                f = open(path) # make undeletable on Windows
                chmod(d, 0o400) # make undeletable on UNIX
                t = path
            end
        end
        chmod(d, 0o700)
        close(f)
        @test isfile(t)
        @test length(TEMP_CLEANUP) == 1
        @test TEMP_CLEANUP_MAX[] == 3
        push!(temps, t)
        # mktempdir is normally cleaned up on completion
        mktempdir(d) do path
            @test isdir(path)
            t = path
        end
        @test !ispath(t)
        @test length(TEMP_CLEANUP) == 1
        @test TEMP_CLEANUP_MAX[] == 3
        # mktempdir when cleanup is prevented
        no_error_logging() do
            mktempdir(d) do path
                @test isdir(path)
                # make undeletable on Windows:
                f = open(joinpath(path, "file.txt"), "w+")
                chmod(d, 0o400) # make undeletable on UNIX
                t = path
            end
        end
        chmod(d, 0o700)
        close(f)
        @test isdir(t)
        @test length(TEMP_CLEANUP) == 2
        @test TEMP_CLEANUP_MAX[] == 3
        push!(temps, t)
        # make one more temp file
        t = mktemp()[1]
        @test isfile(t)
        @test length(TEMP_CLEANUP) == 3
        @test TEMP_CLEANUP_MAX[] == 3
        # nothing has been deleted yet
        for t in temps
            @test ispath(t)
        end
        # another temp file triggers purge
        t = mktempdir()
        @test isdir(t)
        @test length(TEMP_CLEANUP) == 2
        @test TEMP_CLEANUP_MAX[] == 4
        # now all the temps are gone
        for t in temps
            @test !ispath(t)
        end
    end
end

@testset "mktempdir() permissions correction" begin
    # Test that `mktempdir()` with children with weird permissions get cleared
    # before we would delete the directory
    local temp_dir_path
    mktempdir() do dir
        temp_dir_path = dir
        mkdir(joinpath(dir, "foo"))
        touch(joinpath(dir, "foo", "bar"))
        chmod(joinpath(dir, "foo"), 0o444)
        @test isdir(temp_dir_path)
    end
    @test !isdir(temp_dir_path)
end

#######################################################################
# This section tests some of the features of the stat-based file info #
#######################################################################
@test !isfile(Base.Filesystem.StatStruct())
@test isdir(dir)
@test !isfile(dir)
@test !islink(dir)
@test !isdir(file)
@test isfile(file)
@test !islink(file)

@test filemode(file) & 0o444 > 0 # readable
@test filemode(file) & 0o222 > 0 # writable
@test chmod(file, filemode(file) & 0o7555) == file
@test filemode(file) & 0o222 == 0
chmod(file, filemode(file) | 0o222)
@test filemode(file) & 0o111 == 0
@test filesize(file) == 0

# issue #26685
@test !isfile("https://google.com")

if Sys.iswindows()
    permissions = 0o444
    @test filemode(dir) & 0o777 != permissions
    @test filemode(subdir) & 0o777 != permissions
    @test filemode(file) & 0o777 != permissions
    chmod(dir, permissions, recursive=true)
    @test filemode(dir) & 0o777 == permissions
    @test filemode(subdir) & 0o777 == permissions
    @test filemode(file) & 0o777 == permissions
    chmod(dir, 0o666, recursive=true)  # Reset permissions in case someone wants to use these later
else
    function get_umask()
        umask = ccall(:umask, UInt32, (UInt32,), 0)
        ccall(:umask, UInt32, (UInt32,), umask)
        return umask
    end

    mktempdir() do tmpdir
        umask = get_umask()
        tmpfile=joinpath(tmpdir, "tempfile.txt")
        tmpfile2=joinpath(tmpdir, "tempfile2.txt")
        touch(tmpfile)
        cp(tmpfile, tmpfile2)
        @test filemode(tmpfile) & (~umask) == filemode(tmpfile2)
        rm(tmpfile2)
        chmod(tmpfile, 0o777)
        cp(tmpfile, tmpfile2)
        @test filemode(tmpfile) & (~umask) == filemode(tmpfile2)
        rm(tmpfile2)
        chmod(tmpfile, 0o707)
        cp(tmpfile, tmpfile2)
        @test filemode(tmpfile) & (~umask) == filemode(tmpfile2)
        rm(tmpfile2)
        linkfile=joinpath(dir, "tempfile.txt")
        symlink(tmpfile, linkfile)
        permissions=0o776
        @test filemode(dir) & 0o777 != permissions
        @test filemode(subdir) & 0o777 != permissions
        @test filemode(file) & 0o777 != permissions
        @test filemode(linkfile) & 0o777 != permissions
        @test filemode(tmpfile) & 0o777 != permissions
        chmod(dir, permissions, recursive=true)
        @test filemode(dir) & 0o777 == permissions
        @test filemode(subdir) & 0o777 == permissions
        @test filemode(file) & 0o777 == permissions
        @test lstat(link).mode & 0o777 != permissions  # Symbolic links are not modified.
        @test filemode(linkfile) & 0o777 != permissions  # Symbolic links are not followed.
        @test filemode(tmpfile) & 0o777 != permissions
        rm(linkfile)
    end
end

function test_stat_error(stat::Function, pth)
    if stat === lstat && !(pth isa AbstractString)
        return # no lstat for fd handles
    end
    ex = try; stat(pth); false; catch ex; ex; end::Base.IOError
    @test ex.code == (pth isa AbstractString ? Base.UV_EACCES : Base.UV_EBADF)
    pth isa AbstractString || (pth = Base.INVALID_OS_HANDLE)
    @test startswith(ex.msg, "stat($(repr(pth)))")
    nothing
end
@testset "stat errors" begin # PR 32031
    mktempdir() do dir
        cd(dir) do
            touch("afile")
            try
                # remove permission to access this folder
                # to cause cause EACCESS-denied errors
                @static if Sys.iswindows()
                    @test ccall((:ImpersonateAnonymousToken, "Advapi32.dll"), stdcall, Cint, (Libc.WindowsRawSocket,),
                                ccall(:GetCurrentThread, Libc.WindowsRawSocket, ())) != 0
                else
                    chmod(dir, 0o000)
                end
                for pth in ("afile",
                            joinpath("afile", "not_file"),
                            SubString(joinpath(dir, "afile")),
                            Base.RawFD(-1))
                    test_stat_error(stat, pth)
                    test_stat_error(lstat, pth)
                end
            finally
                # restore permissions
                # to let us cleanup afile
                @static if Sys.iswindows()
                    ccall((:RevertToSelf, "advapi32.dll"), stdcall, Cint, ()) == 0 && exit(1)
                else
                    chmod(dir, 0o777)
                end
            end
        end
    end
end

# Issue #51710 and PR #54855
@test_throws MethodError stat(7)
@test_throws MethodError ispath(false)
@test_throws MethodError ispath(1)

# On windows the filesize of a folder is the accumulation of all the contained
# files and is thus zero in this case.
if Sys.iswindows()
    @test filesize(dir) == 0
else
    @test filesize(dir) > 0
end
# We need both: one to check passed time, one to comapare file's mtime()
nowtime = time_ns() / 1e9
nowwall = time()
# Allow 10s skew in addition to the time it took us to actually execute this code
let skew = 10 + (nowtime - starttime)
    mfile = mtime(file)
    mdir  = mtime(dir)
    @test abs(nowwall - mfile) <= skew && abs(nowwall - mdir) <= skew && abs(mfile - mdir) <= skew
end
#@test Int(time()) >= Int(mtime(file)) >= Int(mtime(dir)) >= 0 # 1 second accuracy should be sufficient

# test links
if Sys.isunix()
    @test islink(link) == true
    @test readlink(link) == file
end

if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    @test islink(dirlink) == true
    @test isdir(dirlink) == true
    @test readlink(dirlink) == subdir * (Sys.iswindows() ? "\\" : "")
end

# rm recursive TODO add links
newfile = joinpath(dir, "bfile.txt")
mv(file, newfile)
file = newfile
c_tmpdir = mktempdir()
c_subdir = joinpath(c_tmpdir, "c_subdir")
mkdir(c_subdir)
c_file = joinpath(c_tmpdir, "cfile.txt")
cp(newfile, c_file)

@test isdir(c_subdir)
@test isfile(c_file)
@test_throws Base.IOError rm(c_tmpdir)
@test_throws Base.IOError rm(c_tmpdir, force=true)

# create temp dir in specific directory
d_tmpdir = mktempdir(c_tmpdir)
@test isdir(d_tmpdir)
@test Base.samefile(dirname(d_tmpdir), c_tmpdir)

# create temp file in specific directory
d_tmpfile,f = mktemp(c_tmpdir)
close(f)
@test isfile(d_tmpfile)
@test Base.samefile(dirname(d_tmpfile), c_tmpdir)

rm(c_tmpdir, recursive=true)
@test !isdir(c_tmpdir)
@test_throws Base._UVError("unlink($(repr(c_tmpdir)))", Base.UV_ENOENT) rm(c_tmpdir)
@test rm(c_tmpdir, force=true) === nothing
@test_throws Base._UVError("unlink($(repr(c_tmpdir)))", Base.UV_ENOENT) rm(c_tmpdir, recursive=true)
@test rm(c_tmpdir, force=true, recursive=true) === nothing

# Some operations can return multiple different error codes depending on the system environment.
function throws_matching_exception(f::Function, acceptable_exceptions::AbstractVector)
    try
        f()
        @error "No exception was thrown."
        return false
    catch ex
        if ex in acceptable_exceptions
            return true
        else
            @error "The thrown exception is not in the list of acceptable exceptions" acceptable_exceptions exception=(ex, catch_backtrace())
            return false
        end
    end
end
function throws_matching_uv_error(f::Function, pfx::AbstractString, codes::AbstractVector{<:Integer})
    acceptable_exceptions = multiple_uv_errors(pfx, codes)
    return throws_matching_exception(f, acceptable_exceptions)
end
function multiple_uv_errors(pfx::AbstractString, codes::AbstractVector{<:Integer})
    return [Base._UVError(pfx, code) for code in codes]
end

if !Sys.iswindows()
    # chown will give an error if the user does not have permissions to change files
    uid = Libc.geteuid()
    @test stat(file).uid == uid
    @test uid == Libc.getuid()
    if uid == 0 # root user
        chown(file, -2, -1)  # Change the file owner to nobody
        @test stat(file).uid != 0
        chown(file, 0, -2)  # Change the file group to nogroup (and owner back to root)
        @test stat(file).gid != 0
        @test stat(file).uid == 0
        @test chown(file, -1, 0) == file
        @test stat(file).gid == 0
        @test stat(file).uid == 0
    else
        @test throws_matching_uv_error("chown($(repr(file)), -2, -1)", [Base.UV_EPERM, Base.UV_EINVAL]) do
            chown(file, -2, -1)  # Non-root user cannot change ownership to another user
        end
        @test throws_matching_uv_error("chown($(repr(file)), -1, -2)", [Base.UV_EPERM, Base.UV_EINVAL]) do
            chown(file, -1, -2)  # Non-root user cannot change group to a group they are not a member of (eg: nogroup)
        end
    end
else
    # test that chown doesn't cause any errors for Windows
    @test chown(file, -2, -2) == file
end

##############
# mark/reset #
##############

s = open(file, "w")
write(s, "Marked!\n")
write(s, "Hello world!\n")
write(s, "Goodbye world!\n")
close(s)
s = open(file)
mark(s)
str = readline(s)
@test startswith(str, "Marked!")
@test ismarked(s)
reset(s)
@test !ismarked(s)
str = readline(s)
@test startswith(str, "Marked!")
mark(s)
@test readline(s) == "Hello world!"
@test ismarked(s)
unmark(s)
@test !ismarked(s)
@test_throws ArgumentError reset(s)
@test !unmark(s)
@test !ismarked(s)
close(s)

#######################################################################
# This section tests temporary file and directory creation.           #
#######################################################################

@testset "invalid read/write flags" begin
    @test try
        open("this file is not expected to exist", read=false, write=false)
        false
    catch e
        isa(e, SystemError) || rethrow()
        @test endswith(sprint(showerror, e), "Invalid argument")
        true
    end
end

@testset "quoting filenames" begin
    @test try
        open("this file is not expected to exist")
        false
    catch e
        isa(e, SystemError) || rethrow()
        @test e.errnum == 2
        @test startswith(sprint(showerror, e), "SystemError: opening file \"this file is not expected to exist\"")
        true
    end
end

@testset "tempdir" begin
    my_tempdir = tempdir()
    @test isdir(my_tempdir)
    @test my_tempdir[end] != '/'
    @test my_tempdir[end] != '\\'

    var =  Sys.iswindows() ? "TMP" : "TMPDIR"
    PATH_PREFIX = Sys.iswindows() ? "C:\\" : "/tmp/" * "x"^255   # we want a long path on UNIX so that we test buffer resizing in `tempdir`
    # Warning: On Windows uv_os_tmpdir internally calls GetTempPathW. The max string length for
    # GetTempPathW is 261 (including the implied trailing backslash), not the typical length 259.
    # We thus use 260 (with implied trailing slash backlash this then gives 261 chars)
    # NOTE: not the actual max path on UNIX, but true in the Windows case for this function.
    # NOTE: we subtract 9 to account for i = 0:9.
    MAX_PATH = (Sys.iswindows() ? 260 - length(PATH_PREFIX) : 255)  - 9
    for i = 0:9
        local tmp = joinpath(PATH_PREFIX, "x"^MAX_PATH * "123456789"[1:i])
        no_error_logging() do
            @test withenv(var => tmp) do
                tempdir()
            end == tmp
        end
    end
end

let path = tempname()
    # issue #9053
    @test !ispath(path)
end

(p, f) = mktemp()
print(f, "Here is some text")
close(f)
@test isfile(p) == true
@test read(p, String) == "Here is some text"
rm(p)

let
    tmp_path = mktemp() do p, io
        @test isfile(p)
        print(io, "鴨かも？")
        p
    end
    @test tmp_path != ""
    @test !isfile(tmp_path)
end

let
    tmpdir = mktempdir() do d
        @test isdir(d)
        d
    end
    @test tmpdir != ""
    @test !isdir(tmpdir)
end

emptyfile = joinpath(dir, "empty")
touch(emptyfile)
emptyf = open(emptyfile)
@test isempty(readlines(emptyf, keep=true))
close(emptyf)
rm(emptyfile)


########################################################################################
## This section tests cp & mv(rename) files, directories, absolute and relative links. #
########################################################################################
function check_dir(orig_path::AbstractString, copied_path::AbstractString, follow_symlinks::Bool)
    isdir(orig_path) || throw(ArgumentError("'$orig_path' is not a directory."))
    # copied_path must also be a dir.
    @test isdir(copied_path)
    readir_orig = readdir(orig_path)
    readir_copied = readdir(copied_path)
    @test readir_orig == readir_copied
    # check recursive
    for name in readir_orig
        @test name in readir_copied
        check_cp(joinpath(orig_path, name), joinpath(copied_path, name), follow_symlinks)
    end
end

function check_cp(orig_path::AbstractString, copied_path::AbstractString, follow_symlinks::Bool)
    if islink(orig_path)
        if !follow_symlinks
            # copied_path must be a link
            @test islink(copied_path)
            readlink_orig = readlink(orig_path)
            # copied_path must have the same link value:
            #    this is true for absolute and relative links
            @test readlink_orig == readlink(copied_path)
            if isabspath(readlink_orig)
                @test isabspath(readlink(copied_path))
            end
        else
            # copied_path may not be a link if follow_symlinks=true
            @test islink(orig_path) == !islink(copied_path)
            if isdir(orig_path)
                check_dir(orig_path, copied_path, follow_symlinks)
            else
                # copied_path must also be a file.
                @test isfile(copied_path)
                # copied_path must have same content
                @test read(orig_path, String) == read(copied_path, String)
            end
        end
    elseif isdir(orig_path)
        check_cp_main(orig_path, copied_path, follow_symlinks)
    else
        # copied_path must also be a file.
        @test isfile(copied_path)
        # copied_path must have same content
        @test read(orig_path, String) == read(copied_path, String)
    end
end

function check_cp_main(orig::AbstractString, copied::AbstractString, follow_symlinks::Bool)
    if isdir(orig)
        check_dir(orig, copied, follow_symlinks)
    else
        check_cp(orig, copied, follow_symlinks)
    end
end

function cp_and_test(src::AbstractString, dst::AbstractString, follow_symlinks::Bool)
    cp(src, dst; follow_symlinks=follow_symlinks)
    check_cp_main(src, dst, follow_symlinks)
end

## cp ----------------------------------------------------
# issue #8698
# Test copy file
let
    afile = joinpath(dir, "a.txt")
    touch(afile)
    af = open(afile, "r+")
    write(af, "This is indeed a test")

    bfile = joinpath(dir, "b.txt")
    cp(afile, bfile)

    cfile = joinpath(dir, "c.txt")
    write(cfile, "This is longer than the contents of afile")
    cp(afile, cfile; force=true)

    a_stat = stat(afile)
    b_stat = stat(bfile)
    c_stat = stat(cfile)
    @test a_stat.mode == b_stat.mode
    @test a_stat.size == b_stat.size
    @test a_stat.size == c_stat.size

    @test parse(Int, split(sprint(show, a_stat),"mode: ")[2][1:8]) == a_stat.mode

    close(af)
    rm(afile)
    rm(bfile)
    rm(cfile)
end


## mv ----------------------------------------------------
mktempdir() do tmpdir
    # rename file
    file = joinpath(tmpdir, "afile.txt")
    close(open(file, "w")) # like touch, but lets the operating system update
    files_stat = stat(file)
    # the timestamp for greater precision on some platforms (windows)

    newfile = joinpath(tmpdir, "bfile.txt")
    mv(file, newfile)
    newfile_stat = stat(newfile)

    @test !ispath(file)
    @test isfile(newfile)
    @test Base.samefile(files_stat, newfile_stat)

    file = newfile

    # Test renaming directories
    a_tmpdir = mktempdir()
    b_tmpdir = joinpath(tmpdir, "b_tmpdir")

    # grab a_tmpdir's file info before renaming
    a_stat = stat(a_tmpdir)

    # rename, then make sure b_tmpdir does exist and a_tmpdir doesn't
    mv(a_tmpdir, b_tmpdir)
    @test isdir(b_tmpdir)
    @test !ispath(a_tmpdir)

    # get b_tmpdir's file info and compare with a_tmpdir
    b_stat = stat(b_tmpdir)
    @test Base.samefile(a_stat, b_stat)

    rm(b_tmpdir)
end

@testset "rename" begin
    # some of the windows specific behavior may be fixed in new versions of julia
    mktempdir() do dir
        # see if can make symlinks
        local can_symlink = try
            symlink("foo", joinpath(dir, "link"))
            rm(joinpath(dir, "link"))
            true
        catch
            false
        end
        local f1 = joinpath(dir, "file1")
        local f2 = joinpath(dir, "file2")
        local d1 = joinpath(dir, "dir1")
        local d2 = joinpath(dir, "dir2")
        local subd1f1 = joinpath(d1, "file1")
        local subd1f2 = joinpath(d1, "file2")
        local subd2f1 = joinpath(d2, "file1")
        local subd2f2 = joinpath(d2, "file2")
        local h1 = joinpath(dir, "hlink1")
        local h2 = joinpath(dir, "hlink2")
        local s1 = joinpath(dir, "slink1")
        local s2 = joinpath(dir, "slink2")
        @testset "renaming to non existing newpath in same directory" begin
            # file, make sure isexecutable is copied
            for mode in (0o644, 0o755)
                write(f1, b"data")
                chmod(f1, mode)
                Base.rename(f1, f2)
                @test !isfile(f1)
                @test isfile(f2)
                @test read(f2) == b"data"
                if mode == 0o644
                    @test !isexecutable(f2)
                else
                    @test isexecutable(f2)
                end
                rm(f2)
            end
            # empty directory
            mkdir(d1)
            Base.rename(d1, d2)
            @test !isdir(d1)
            @test isdir(d2)
            @test isempty(readdir(d2))
            rm(d2)
            # non empty directory
            mkdir(d1)
            write(subd1f1, b"data")
            chmod(subd1f1, 0o644)
            write(subd1f2, b"exe")
            chmod(subd1f2, 0o755)
            Base.rename(d1, d2)
            @test !isdir(d1)
            @test isdir(d2)
            @test read(subd2f1) == b"data"
            @test read(subd2f2) == b"exe"
            @test !isexecutable(subd2f1)
            @test isexecutable(subd2f2)
            rm(d2; recursive=true)
            # hardlink
            write(f1, b"data")
            hardlink(f1, h1)
            Base.rename(h1, h2)
            @test isfile(f1)
            @test !isfile(h1)
            @test isfile(h2)
            @test read(h2) == b"data"
            write(h2, b"data2")
            @test read(f1) == b"data2"
            rm(h2)
            rm(f1)
            # symlink
            if can_symlink
                symlink("foo", s1)
                Base.rename(s1, s2)
                @test !islink(s1)
                @test islink(s2)
                @test readlink(s2) == "foo"
                rm(s2)
            end
        end
        @test isempty(readdir(dir)) # make sure everything got cleaned up

        # Get the error code from failed rename, or nothing if it worked
        function rename_errorcodes(oldpath, newpath)
            try
                Base.rename(oldpath, newpath)
                nothing
            catch e
                e.code
            end
        end
        @testset "errors" begin
            # invalid paths
            @test_throws ArgumentError Base.rename(f1*"\0", "")
            @test Base.UV_ENOENT == rename_errorcodes("", "")
            write(f1, b"data")
            @test Base.UV_ENOENT == rename_errorcodes(f1, "")
            @test read(f1) == b"data"
            @test Base.UV_ENOENT == rename_errorcodes("", f1)
            @test read(f1) == b"data"
            @test Base.UV_ENOENT == rename_errorcodes(f2, f1)
            @test read(f1) == b"data"
            @test Base.UV_ENOENT == rename_errorcodes(f1, subd1f1)
            @test read(f1) == b"data"
            rm(f1)
            # attempt to make a directory a subdirectory of itself
            mkdir(d1)
            if Sys.iswindows()
                @test rename_errorcodes(d1, joinpath(d1, "subdir")) ∈ (Base.UV_EINVAL, Base.UV_EBUSY)
            else
                @test Base.UV_EINVAL == rename_errorcodes(d1, joinpath(d1, "subdir"))
            end
            rm(d1)
            # rename to child of a file
            mkdir(d1)
            write(f2, "foo")
            if Sys.iswindows()
                @test Base.UV_EINVAL == rename_errorcodes(d1, joinpath(f2, "subdir"))
            else
                @test Base.UV_ENOTDIR == rename_errorcodes(d1, joinpath(f2, "subdir"))
            end
            # replace a file with a directory
            if !Sys.iswindows()
                @test Base.UV_ENOTDIR == rename_errorcodes(d1, f2)
            else
                # this should work on windows
                Base.rename(d1, f2)
                @test isdir(f2)
                @test !ispath(d1)
            end
            rm(f2; force=true)
            rm(d1; force=true)
            # symlink loop
            if can_symlink
                symlink(s1, s2)
                symlink(s2, s1)
                @test Base.UV_ELOOP == rename_errorcodes(joinpath(s1, "foo"), f2)
                write(f2, b"data")
                @test Base.UV_ELOOP == rename_errorcodes(f2, joinpath(s1, "foo"))
                rm(s1)
                rm(s2)
                rm(f2)
            end
            # newpath is a nonempty directory
            mkdir(d1)
            mkdir(d2)
            write(subd2f1, b"data")
            write(f1, b"otherdata")
            if Sys.iswindows()
                @test Base.UV_EACCES == rename_errorcodes(f1, d1)
                @test Base.UV_EACCES == rename_errorcodes(f1, d2)
                @test Base.UV_EACCES == rename_errorcodes(d1, d2)
                @test Base.UV_EACCES == rename_errorcodes(subd2f1, d2)
            else
                @test Base.UV_EISDIR == rename_errorcodes(f1, d1)
                @test Base.UV_EISDIR == rename_errorcodes(f1, d2)
                @test rename_errorcodes(d1, d2) ∈ (Base.UV_ENOTEMPTY, Base.UV_EEXIST)
                @test rename_errorcodes(subd2f1, d2) ∈ (Base.UV_ENOTEMPTY, Base.UV_EEXIST, Base.UV_EISDIR)
            end
            rm(f1)
            rm(d1)
            rm(d2; recursive=true)
        end
        @test isempty(readdir(dir)) # make sure everything got cleaned up

        @testset "replacing existing file" begin
            write(f2, b"olddata")
            chmod(f2, 0o755)
            write(f1, b"newdata")
            chmod(f1, 0o644)
            @test isexecutable(f2)
            @test !isexecutable(f1)
            Base.rename(f1, f2)
            @test !ispath(f1)
            @test read(f2) == b"newdata"
            @test !isexecutable(f2)
            rm(f2)
        end

        @testset "replacing file with itself" begin
            write(f1, b"data")
            Base.rename(f1, f1)
            @test read(f1) == b"data"
            hardlink(f1, h1)
            Base.rename(f1, h1)
            if Sys.iswindows()
                # On Windows f1 gets deleted
                @test !ispath(f1)
            else
                @test read(f1) == b"data"
            end
            @test read(h1) == b"data"
            rm(h1)
            rm(f1; force=true)
        end

        @testset "replacing existing file in different directories" begin
            mkdir(d1)
            mkdir(d2)
            write(subd2f2, b"olddata")
            chmod(subd2f2, 0o755)
            write(subd1f1, b"newdata")
            chmod(subd1f1, 0o644)
            @test isexecutable(subd2f2)
            @test !isexecutable(subd1f1)
            Base.rename(subd1f1, subd2f2)
            @test !ispath(subd1f1)
            @test read(subd2f2) == b"newdata"
            @test !isexecutable(subd2f2)
            @test isdir(d1)
            @test isdir(d2)
            rm(d1; recursive=true)
            rm(d2; recursive=true)
        end

        @testset "rename with open files" begin
            # both open
            write(f2, b"olddata")
            write(f1, b"newdata")
            open(f1) do handle1
                open(f2) do handle2
                    if Sys.iswindows()
                        # currently this doesn't work on windows
                        @test Base.UV_EBUSY == rename_errorcodes(f1, f2)
                    else
                        Base.rename(f1, f2)
                        @test !ispath(f1)
                        @test read(f2) == b"newdata"
                    end
                    # rename doesn't break already opened files
                    @test read(handle1) == b"newdata"
                    @test read(handle2) == b"olddata"
                end
            end
            rm(f1; force=true)
            rm(f2; force=true)

            # oldpath open
            write(f2, b"olddata")
            write(f1, b"newdata")
            open(f1) do handle1
                if Sys.iswindows()
                    # currently this doesn't work on windows
                    @test Base.UV_EBUSY == rename_errorcodes(f1, f2)
                else
                    Base.rename(f1, f2)
                    @test !ispath(f1)
                    @test read(f2) == b"newdata"
                end
                # rename doesn't break already opened files
                @test read(handle1) == b"newdata"
            end
            rm(f1; force=true)
            rm(f2; force=true)

            # newpath open
            write(f2, b"olddata")
            write(f1, b"newdata")
            open(f2) do handle2
                if Sys.iswindows()
                    # currently this doesn't work on windows
                    @test Base.UV_EACCES == rename_errorcodes(f1, f2)
                else
                    Base.rename(f1, f2)
                    @test !ispath(f1)
                    @test read(f2) == b"newdata"
                end
                # rename doesn't break already opened files
                @test read(handle2) == b"olddata"
            end
            rm(f1; force=true)
            rm(f2; force=true)
        end

        @testset "replacing empty directory with directory" begin
            mkdir(d1)
            mkdir(d2)
            write(subd1f1, b"data")
            if Sys.iswindows()
                # currently this doesn't work on windows
                @test Base.UV_EACCES == rename_errorcodes(d1, d2)
                rm(d1; recursive=true)
                rm(d2)
            else
                Base.rename(d1, d2)
                @test isdir(d2)
                @test read(subd2f1) == b"data"
                @test !ispath(d1)
                rm(d2; recursive=true)
            end
        end
        @test isempty(readdir(dir)) # make sure everything got cleaned up
    end
end

# issue #10506 #10434
## Tests for directories and links to directories
if !Sys.iswindows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    function setup_dirs(tmpdir)
        srcdir = joinpath(tmpdir, "src")
        hidden_srcdir = joinpath(tmpdir, ".hidden_srcdir")
        hidden_srcsubdir = joinpath(hidden_srcdir, ".hidden_srcsubdir")
        srcdir_cp = joinpath(tmpdir, "srcdir_cp")
        mkdir(srcdir)
        mkdir(hidden_srcdir)
        mkdir(hidden_srcsubdir)
        abs_dirlink = joinpath(tmpdir, "abs_dirlink")
        symlink(abspath(srcdir), abs_dirlink)
        cd(tmpdir)
        rel_dirlink = "rel_dirlink"
        symlink("src", rel_dirlink)
        cd(pwd_)

        cfile = joinpath(srcdir, "c.txt")
        file_txt = "This is some text with unicode - 这是一个文件"
        write(cfile, file_txt)
        hidden_cfile = joinpath(hidden_srcsubdir, "c.txt")
        write(hidden_cfile, file_txt)

        abs_dirlink_cp = joinpath(tmpdir, "abs_dirlink_cp")
        hidden_srcsubdir_cp = joinpath(tmpdir, ".hidden_srcsubdir_cp")
        path_rel_dirlink = joinpath(tmpdir, rel_dirlink)
        path_rel_dirlink_cp = joinpath(tmpdir, "rel_dirlink_cp")

        test_src_paths = [srcdir, hidden_srcsubdir, abs_dirlink, path_rel_dirlink]
        test_cp_paths = [srcdir_cp, hidden_srcsubdir_cp, abs_dirlink_cp, path_rel_dirlink_cp]
        return test_src_paths, test_cp_paths
    end

    function cp_follow_symlinks_false_check(s, d; force=false)
        cp(s, d; force=force, follow_symlinks=false)
        @test isdir(s) == isdir(d)
        @test islink(s) == islink(d)
        islink(s) && @test readlink(s) == readlink(d)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d))
        # all should contain 1 file named  "c.txt"
        @test "c.txt" in readdir(d)
        @test length(readdir(d)) == 1
    end

    function mv_check(s, d, d_mv; force=true)
        # setup dest
        cp(s, d; force=true, follow_symlinks=false)
        stat_d = stat(d)
        # mv(rename) dst to dst_mv
        mv(d, d_mv; force=force)
        stat_d_mv = stat(d_mv)
        # make sure d does not exist anymore
        @test !ispath(d)
        # compare s, with d_mv
        @test isdir(s) == isdir(d_mv)
        @test islink(s) == islink(d_mv)
        islink(s) && @test readlink(s) == readlink(d_mv)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d_mv))
        # all should contain 1 file named  "c.txt"
        @test "c.txt" in readdir(d_mv)
        @test length(readdir(d_mv)) == 1
        # d => d_mv same file/dir
        @test Base.samefile(stat_d, stat_d_mv)
    end

    ## Test require `force=true` (remove destination first) for existing
    #  directories and existing links to directories
    # cp ----------------------------------------------------
    mktempdir() do tmpdir
        # Setup new copies for the test
        maindir1 = joinpath(tmpdir, "maindir1")
        maindir2 = joinpath(tmpdir, "maindir2")
        mkdir(maindir1)
        mkdir(maindir2)
        test_src_paths1, test_new_paths1 = setup_dirs(maindir1)
        test_src_paths2, test_new_paths2 = setup_dirs(maindir2)
        for (s, d) in zip(test_src_paths1, test_new_paths1)
            cp_follow_symlinks_false_check(s, d)
        end
        for (s, d) in zip(test_src_paths2, test_new_paths2)
            cp_follow_symlinks_false_check(s, d)
        end
        # Test require `force=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError cp(s, d; force=false)
                @test_throws ArgumentError cp(s, d; force=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            cp_follow_symlinks_false_check(s, d; force=true)
        end
        # Test remove the existing path first and copy an empty dir
        emptydir = joinpath(maindir1, "emptydir")
        mkdir(emptydir)
        for d in test_new_paths1
            cp(emptydir, d; force=true, follow_symlinks=false)
            # Expect no link because a dir is copied (follow_symlinks=false does not effect this)
            @test isdir(d) && !islink(d)
            # none should contain any file
            @test isempty(readdir(d))
        end
    end
    # mv ----------------------------------------------------
    mktempdir() do tmpdir
        # Setup new copies for the test
        maindir1 = joinpath(tmpdir, "maindir1")
        maindir2 = joinpath(tmpdir, "maindir2")
        mkdir(maindir1)
        mkdir(maindir2)
        test_src_paths1, test_new_paths1 = setup_dirs(maindir1)
        test_src_paths2, test_new_paths2 = setup_dirs(maindir2)
        for (s, d) in zip(test_src_paths1, test_new_paths1)
            cp_follow_symlinks_false_check(s, d; force=true)
        end
        for (s, d) in zip(test_src_paths2, test_new_paths2)
            cp_follow_symlinks_false_check(s, d; force=true)
        end

        # Test require `force=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError mv(s, d; force=false)
            end
        end
        # Test remove the existing path first and move
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv_check(s, d, d_mv; force=true)
        end
    end

    # Test full: absolute and relative directory links
    # cp / mv ----------------------------------------------------
    mktempdir() do tmpdir
        maindir = joinpath(tmpdir, "mytestdir")
        mkdir(maindir)
        targetdir = abspath(joinpath(maindir, "targetdir"))
        mkdir(targetdir)
        subdir1 = joinpath(maindir, "subdir1")
        mkdir(subdir1)

        cfile = abspath(joinpath(maindir, "c.txt"))
        write(cfile, "This is c.txt - 这是一个文件")
        write(abspath(joinpath(targetdir, "file1.txt")),
              "This is file1.txt - 这是一个文件")

        abs_dl = joinpath(maindir, "abs_linkto_targetdir")
        symlink(targetdir, abs_dl)
        # Setup relative links
        cd(subdir1)
        rel_dl = "rel_linkto_targetdir"
        rel_dir = joinpath("..", "targetdir")
        symlink(rel_dir, rel_dl)
        cd(pwd_)
        # TEST: Directory with links: Test each option
        maindir_new = joinpath(dirname(maindir),"maindir_new")
        maindir_new_keepsym = joinpath(dirname(maindir),"maindir_new_keepsym")
        cp_and_test(maindir, maindir_new, true)
        cp_and_test(maindir, maindir_new_keepsym, false)

        # mv ----------------------------------------------------
        # move the 3 maindirs
        for d in [maindir_new, maindir_new_keepsym, maindir]
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv(d, d_mv; force=true)
        end
    end

    # issue  ----------------------------------------------------
    # Check for issue when: (src == dst) or when one is a link to the other
    # https://github.com/JuliaLang/julia/pull/11172#issuecomment-100391076
    mktempdir() do tmpdir
        test_src_paths1, test_new_paths1 = setup_dirs(tmpdir)
        dirs = [joinpath(tmpdir, "src"), joinpath(tmpdir, "abs_dirlink"), joinpath(tmpdir, "rel_dirlink")]
        for src in dirs
            for dst in dirs
                # cptree
                @test_throws ArgumentError Base.cptree(src, dst; force=true, follow_symlinks=false)
                @test_throws ArgumentError Base.cptree(src, dst; force=true, follow_symlinks=true)
                # cp
                @test_throws ArgumentError cp(src, dst; force=true, follow_symlinks=false)
                @test_throws ArgumentError cp(src, dst; force=true, follow_symlinks=true)
                # mv
                @test_throws ArgumentError mv(src, dst; force=true)
            end
        end
    end
    # None existing src
    mktempdir() do tmpdir
        nonexisting_src = joinpath(tmpdir, "nonexisting_src")
        dst = joinpath(tmpdir, "dst")
        @test !ispath(nonexisting_src)
        # cptree
        @test_throws(ArgumentError("'$nonexisting_src' is not a directory. Use `cp(src, dst)`"),
                     Base.cptree(nonexisting_src, dst; force=true, follow_symlinks=false))
        @test_throws(ArgumentError("'$nonexisting_src' is not a directory. Use `cp(src, dst)`"),
                     Base.cptree(nonexisting_src, dst; force=true, follow_symlinks=true))
        # cp
        @test_throws Base._UVError("open($(repr(nonexisting_src)), $(Base.JL_O_RDONLY), 0)", Base.UV_ENOENT) cp(nonexisting_src, dst; force=true, follow_symlinks=false)
        @test_throws Base._UVError("open($(repr(nonexisting_src)), $(Base.JL_O_RDONLY), 0)", Base.UV_ENOENT) cp(nonexisting_src, dst; force=true, follow_symlinks=true)
        # mv
        @test_throws Base._UVError("rename($(repr(nonexisting_src)), $(repr(dst)))", Base.UV_ENOENT) mv(nonexisting_src, dst; force=true)
    end
end

# issue #10506 #10434
## Tests for files and links to files as well as directories and links to directories
if !Sys.iswindows()
    function setup_files(tmpdir)
        srcfile = joinpath(tmpdir, "srcfile.txt")
        hidden_srcfile = joinpath(tmpdir, ".hidden_srcfile.txt")
        srcfile_new = joinpath(tmpdir, "srcfile_new.txt")
        hidden_srcfile_new = joinpath(tmpdir, ".hidden_srcfile_new.txt")
        file_txt = "This is some text with unicode - 这是一个文件"
        write(srcfile, file_txt)
        write(hidden_srcfile, file_txt)
        abs_filelink = joinpath(tmpdir, "abs_filelink")
        symlink(abspath(srcfile), abs_filelink)
        cd(tmpdir)
        rel_filelink = "rel_filelink"
        symlink("srcfile.txt", rel_filelink)
        cd(pwd_)

        abs_filelink_new = joinpath(tmpdir, "abs_filelink_new")
        path_rel_filelink = joinpath(tmpdir, rel_filelink)
        path_rel_filelink_new = joinpath(tmpdir, "rel_filelink_new")

        test_src_paths = [srcfile, hidden_srcfile, abs_filelink, path_rel_filelink]
        test_new_paths = [srcfile_new, hidden_srcfile_new, abs_filelink_new, path_rel_filelink_new]
        return test_src_paths, test_new_paths, file_txt
    end

    function cp_follow_symlinks_false_check(s, d, file_txt; force=false)
        cp(s, d; force=force, follow_symlinks=false)
        @test isfile(s) == isfile(d)
        @test islink(s) == islink(d)
        islink(s) && @test readlink(s) == readlink(d)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d))
        # all should contain the same
        @test read(s, String) == read(d, String) == file_txt
    end

    function mv_check(s, d, d_mv, file_txt; force=true)
        # setup dest
        cp(s, d; force=true, follow_symlinks=false)
        stat_d = stat(d)
        # mv(rename) dst to dst_mv
        mv(d, d_mv; force=force)
        stat_d_mv = stat(d_mv)
        # make sure d does not exist anymore
        @test !ispath(d)
        # comare s, with d_mv
        @test isfile(s) == isfile(d_mv)
        @test islink(s) == islink(d_mv)
        islink(s) && @test readlink(s) == readlink(d_mv)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d_mv))
        # all should contain the same
        @test read(s, String) == read(d_mv, String) == file_txt
        # d => d_mv same file/dir
        @test Base.samefile(stat_d, stat_d_mv)
    end

    ## Test require `force=true` (remove destination first) for existing
    #  files and existing links to files
    # cp ----------------------------------------------------
    mktempdir() do tmpdir
        # Setup new copies for the test
        maindir1 = joinpath(tmpdir, "maindir1")
        maindir2 = joinpath(tmpdir, "maindir2")
        mkdir(maindir1)
        mkdir(maindir2)
        test_src_paths1, test_new_paths1, file_txt1 = setup_files(maindir1)
        test_src_paths2, test_new_paths2, file_txt2 = setup_files(maindir2)
        for (s, d) in zip(test_src_paths1, test_new_paths1)
            cp_follow_symlinks_false_check(s, d, file_txt1)
        end
        for (s, d) in zip(test_src_paths2, test_new_paths2)
            cp_follow_symlinks_false_check(s, d, file_txt2)
        end
        # Test require `force=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError cp(s, d; force=false)
                @test_throws ArgumentError cp(s, d; force=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            cp_follow_symlinks_false_check(s, d, file_txt2; force=true)
        end
        # Test remove the existing path first and copy an other file
        otherfile = joinpath(tmpdir, "otherfile.txt")
        otherfile_content = "This is otherfile.txt with unicode - 这是一个文件"
        write(otherfile, otherfile_content)
        for d in test_new_paths1
            cp(otherfile, d; force=true, follow_symlinks=false)
            # Expect no link because a file is copied (follow_symlinks=false does not effect this)
            @test isfile(d) && !islink(d)
            # all should contain otherfile_content
            @test read(d, String) == otherfile_content
        end
    end
    # mv ----------------------------------------------------
    mktempdir() do tmpdir
        # Setup new copies for the test
        maindir1 = joinpath(tmpdir, "maindir1")
        maindir2 = joinpath(tmpdir, "maindir2")
        mkdir(maindir1)
        mkdir(maindir2)
        test_src_paths1, test_new_paths1, file_txt1 = setup_files(maindir1)
        test_src_paths2, test_new_paths2, file_txt2 = setup_files(maindir2)
        for (s, d) in zip(test_src_paths1, test_new_paths1)
            cp_follow_symlinks_false_check(s, d, file_txt1)
        end
        for (s, d) in zip(test_src_paths2, test_new_paths2)
            cp_follow_symlinks_false_check(s, d, file_txt2)
        end
        # Test require `force=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError mv(s, d; force=false)
            end
        end
        # Test remove the existing path first and move
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv_check(s, d, d_mv, file_txt2; force=true)
        end
    end

    # Test full: absolute and relative file links and absolute and relative directory links
    # cp / mv ----------------------------------------------------
    mktempdir() do tmpdir
        maindir = joinpath(tmpdir, "mytestdir")
        mkdir(maindir)
        targetdir = abspath(joinpath(maindir, "targetdir"))
        mkdir(targetdir)
        subdir1 = joinpath(maindir, "subdir1")
        mkdir(subdir1)

        cfile = abspath(joinpath(maindir, "c.txt"))
        write(cfile, "This is c.txt - 这是一个文件")
        write(abspath(joinpath(targetdir, "file1.txt")),
                      "This is file1.txt - 这是一个文件")

        abs_fl = joinpath(maindir, "abs_linkto_c.txt")
        symlink(cfile, abs_fl)
        abs_dl = joinpath(maindir, "abs_linkto_targetdir")
        symlink(targetdir, abs_dl)
        # Setup relative links
        cd(subdir1)
        rel_fl = "rel_linkto_c.txt"
        rel_file = joinpath("..", "c.txt")
        symlink(rel_file, rel_fl)
        rel_dl = "rel_linkto_targetdir"
        rel_dir = joinpath("..", "targetdir")
        symlink(rel_dir, rel_dl)
        rel_file_read_txt = read(rel_file, String)
        cd(pwd_)
        # Setup copytodir
        copytodir = joinpath(tmpdir, "copytodir")
        mkdir(copytodir)
        cp(cfile, joinpath(copytodir, basename(cfile)))
        subdir_test = joinpath(copytodir, "subdir_test")
        mkdir(subdir_test)
        cp(targetdir, joinpath(copytodir, basename(targetdir)); follow_symlinks=false)
        # TEST: Directory with links: Test each option
        maindir_new =  joinpath(dirname(maindir),"maindir_new")
        maindir_new_keepsym =  joinpath(dirname(maindir),"maindir_new_keepsym")
        cp_and_test(maindir, maindir_new, true)
        cp_and_test(maindir, maindir_new_keepsym, false)

        ## Tests single Files, File Links
        rel_flpath = joinpath(subdir1, rel_fl)
        # `cp file`
        cp_and_test(cfile, joinpath(copytodir,"cfile_new.txt"), true)
        cp_and_test(cfile, joinpath(copytodir,"cfile_new_keepsym.txt"), false)
        # `cp absolute file link`
        cp_and_test(abs_fl, joinpath(copytodir,"abs_fl_new.txt"), true)
        cp_and_test(abs_fl, joinpath(copytodir,"abs_fl_new_keepsym.txt"), false)
        # `cp relative file link`
        cp_and_test(rel_flpath, joinpath(subdir_test,"rel_fl_new.txt"), true)
        cp_and_test(rel_flpath, joinpath(subdir_test,"rel_fl_new_keepsym.txt"), false)

        # mv ----------------------------------------------------
        # move all 4 existing dirs
        # As expected this will leave some absolute links broken #11145#issuecomment-99315168
        for d in [copytodir, maindir_new, maindir_new_keepsym, maindir]
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv(d, d_mv; force=true)
        end
    end
    # issue  ----------------------------------------------------
    # Check for issue when: (src == dst) or when one is a link to the other
    # https://github.com/JuliaLang/julia/pull/11172#issuecomment-100391076
    mktempdir() do tmpdir
        test_src_paths, test_new_paths, file_txt = setup_files(tmpdir)
        files = [joinpath(tmpdir, "srcfile.txt"), joinpath(tmpdir, "abs_filelink"), joinpath(tmpdir, "rel_filelink")]
        for src in files
            for dst in files
                # cptree
                @test_throws ArgumentError Base.cptree(src, dst; force=true, follow_symlinks=false)
                @test_throws ArgumentError Base.cptree(src, dst; force=true, follow_symlinks=true)
                # cp
                @test_throws ArgumentError cp(src, dst; force=true, follow_symlinks=false)
                @test_throws ArgumentError cp(src, dst; force=true, follow_symlinks=true)
                # mv
                @test_throws ArgumentError mv(src, dst; force=true)
            end
        end
    end
    # None existing src: not needed here as it is done above with the directories.
end


###################
# FILE* interface #
###################

function test_LibcFILE(FILEp)
    buf = Vector{UInt8}(undef, 8)
    str = ccall(:fread, Csize_t, (Ptr{Cvoid}, Csize_t, Csize_t, Ptr{Cvoid}), buf, 1, 8, FILEp)
    @test String(buf) == "Hello, w"
    @test position(FILEp) == 8
    seek(FILEp, 5)
    @test position(FILEp) == 5
    close(FILEp)
end

let f = open(file, "w")
    write(f, "Hello, world!")
    close(f)
    f = open(file, "r")
    test_LibcFILE(Libc.FILE(f))
    close(f)
    if Sys.iswindows()
        f = RawFD(ccall(:_open, Cint, (Cstring, Cint), file, Base.Filesystem.JL_O_RDONLY))
    else
        f = RawFD(ccall(:open, Cint, (Cstring, Cint, UInt32...), file, Base.Filesystem.JL_O_RDONLY))
    end
    test_LibcFILE(Libc.FILE(f, Libc.modestr(true, false)))
end

# issue #10994: pathnames cannot contain embedded NUL chars
for f in (mkdir, cd, Base.Filesystem.unlink, readlink, rm, touch, readdir, mkpath,
        stat, lstat, ctime, mtime, filemode, filesize, uperm, gperm, operm, touch,
        isblockdev, ischardev, isdir, isfifo, isfile, islink, ispath, issetgid,
        issetuid, issocket, issticky, realpath)
    local f
    @test_throws ArgumentError f("adir\0bad")
end
@test_throws ArgumentError chmod("ba\0d", 0o222)
@test_throws ArgumentError open("ba\0d", "w")
@test_throws ArgumentError cp(file, "ba\0d")
@test_throws ArgumentError mv(file, "ba\0d")
if !Sys.iswindows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    @test_throws ArgumentError symlink(file, "ba\0d")
else
    @test_throws ErrorException symlink(file, "ba\0d")
end
using Downloads: download
@test_throws ArgumentError download("good", "ba\0d")
@test_throws ArgumentError download("ba\0d", tempname())

###################
#     walkdir     #
###################

dirwalk = mktempdir()
cd(dirwalk) do
    for i=1:2
        mkdir("sub_dir$i")
        open("file$i", "w") do f end

        mkdir(joinpath("sub_dir1", "subsub_dir$i"))
        touch(joinpath("sub_dir1", "file$i"))
    end
    touch(joinpath("sub_dir2", "file_dir2"))
    has_symlinks = !Sys.iswindows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    follow_symlink_vec = has_symlinks ? [true, false] : [false]
    has_symlinks && symlink(abspath("sub_dir2"), joinpath("sub_dir1", "link"))
    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks)
        root, dirs, files = @inferred(take!(chnl))
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir1")
        if has_symlinks
            if follow_symlinks
                @test dirs ==  ["link", "subsub_dir1", "subsub_dir2"]
                @test files == ["file1", "file2"]
            else
                @test dirs ==  ["subsub_dir1", "subsub_dir2"]
                @test files == ["file1", "file2", "link"]
            end
        else
            @test dirs ==  ["subsub_dir1", "subsub_dir2"]
            @test files == ["file1", "file2"]
        end

        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end

        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]
    end

    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks, topdown=false)
        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end
        @test root == joinpath(".", "sub_dir1")
        if has_symlinks
            if follow_symlinks
                @test dirs ==  ["link", "subsub_dir1", "subsub_dir2"]
                @test files == ["file1", "file2"]
            else
                @test dirs ==  ["subsub_dir1", "subsub_dir2"]
                @test files == ["file1", "file2", "link"]
            end
        else
            @test dirs ==  ["subsub_dir1", "subsub_dir2"]
            @test files == ["file1", "file2"]
        end

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]

        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]
    end
    #test of error handling
    chnl_error = walkdir(".")
    chnl_noerror = walkdir(".", onerror=x->x)
    root, dirs, files = take!(chnl_error)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    rm(joinpath("sub_dir1"), recursive=true)
    @test_throws(Base._UVError("readdir($(repr(joinpath(".", "sub_dir1"))))", Base.UV_ENOENT),
                 take!(chnl_error)) # throws an error because sub_dir1 do not exist

    root, dirs, files = take!(chnl_noerror)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    root, dirs, files = take!(chnl_noerror) # skips sub_dir1 as it no longer exist
    @test root == joinpath(".", "sub_dir2")
    @test dirs == []
    @test files == ["file_dir2"]

    # Test that symlink loops don't cause errors
    if has_symlinks
        mkdir(joinpath(".", "sub_dir3"))
        foo = joinpath(".", "sub_dir3", "foo")
        symlink("foo", foo)

        let files = walkdir(joinpath(".", "sub_dir3"); follow_symlinks=true)
            @test_throws Base._UVError("stat($(repr(foo)))", Base.UV_ELOOP)  take!(files)
        end
        root, dirs, files = take!(walkdir(joinpath(".", "sub_dir3"); follow_symlinks=false))
        @test root == joinpath(".", "sub_dir3")
        @test dirs == []
        @test files == ["foo"]
    end

    # pwd() as default directory
    for ((r1, d1, f1), (r2, d2, f2)) in zip(walkdir(), walkdir(pwd()))
        @test r1 == r2
        @test d1 == d2
        @test f1 == f2
    end
end
rm(dirwalk, recursive=true)

###################
#     readdir     #
###################
@testset "readdir is sorted" begin
    mktempdir() do dir
        cd(dir) do
            for k in 1:10
                touch(randstring())
            end
            @test issorted(readdir())
            @test issorted(Base.Filesystem._readdirx())
            @test map(o->o.name, Base.Filesystem._readdirx()) == readdir()
            @test map(o->o.path, Base.Filesystem._readdirx()) == readdir(join=true)
            @test count(isfile, readdir(join=true)) == count(isfile, Base.Filesystem._readdirx())
        end
    end
end

############
# Clean up #
############
if !Sys.iswindows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    rm(link)
    rm(rellink)
    rm(dirlink)
    rm(relsubdirlink)
end
rm(file)
rm(subdir)
rm(subdir2)
rm(dir)

@test !ispath(file)
@test !ispath(dir)



##################
# Return values of mkpath, mkdir, cp, mv, rename and touch
####################
mktempdir() do dir
    name1 = joinpath(dir, "apples")
    name2 = joinpath(dir, "bananas")
    @test !ispath(name1)
    @test touch(name1) == name1
    @test isfile(name1)
    @test touch(name1) == name1
    @test isfile(name1)
    @test !ispath(name2)
    @test mv(name1, name2) == name2
    @test !ispath(name1)
    @test isfile(name2)
    @test cp(name2, name1) == name1
    @test isfile(name1)
    @test isfile(name2)
    @test Base.rename(name1, name2) == name2
    @test !ispath(name1)
    @test isfile(name2)
    namedir = joinpath(dir, "chalk")
    namepath = joinpath(dir, "chalk", "cheese", "fresh")
    @test !ispath(namedir)
    @test mkdir(namedir) == namedir
    @test isdir(namedir)
    @test !ispath(namepath)
    @test mkpath(namepath) == namepath
    @test isdir(namepath)
    @test mkpath(namepath) == namepath
    @test isdir(namepath)
    # issue 54826
    namepath_dirpath = joinpath(dir, "x", "y", "z", "")
    @test mkpath(namepath_dirpath) == namepath_dirpath
end
@test mkpath("") == ""
@test mkpath("/") == "/"

# issue #30588
@test realpath(".") == realpath(pwd())
mktempdir() do dir
    cd(dir) do
        path = touch("FooBar.txt")
        @test ispath(realpath(path))
        if ispath(uppercase(path)) # case-insensitive filesystem
            @test realpath(path) == realpath(uppercase(path)) == realpath(lowercase(path)) ==
                  realpath(uppercase(realpath(path))) == realpath(lowercase(realpath(path)))
            @test basename(realpath(uppercase(path))) == path
        end
        rm(path)
        @test_throws Base.IOError realpath(path)
    end
end

# issue #9687
let n = tempname()
    w = open(n, "a")
    io = open(n)
    write(w, "A"); flush(w)
    @test read(io) == UInt8[0x41]
    @test read(io) == UInt8[]
    write(w, "A"); flush(w)
    @test read(io) == UInt8[0x41]
    close(io); close(w)
    rm(n)
end

# PR #39906
if !Sys.iswindows()
    @testset "rm empty directories without read permissions" begin
        mktempdir() do d
            mkdir(joinpath(d, "nonempty"))
            touch(joinpath(d, "nonempty", "a"))
            mkdir(joinpath(d, "empty_outer"))
            mkdir(joinpath(d, "empty_outer", "empty_inner"))

            chmod(joinpath(d, "nonempty"), 0o333)
            chmod(joinpath(d, "empty_outer", "empty_inner"), 0o333)

            # Test that an empty directory, even when we can't read its contents, is deletable
            rm(joinpath(d, "empty_outer"); recursive=true)
            @test !isdir(joinpath(d, "empty_outer"))

            # But a non-empty directory is not
            @test_throws Base.IOError rm(joinpath(d, "nonempty"); recursive=true)
            chmod(joinpath(d, "nonempty"), 0o777)
            rm(joinpath(d, "nonempty"); recursive=true, force=true)
            @test !isdir(joinpath(d, "nonempty"))
        end
    end
end

@test_throws ArgumentError mkpath("fakepath", mode = -1)

@testset "mktempdir 'prefix' argument" begin
    tmpdirbase = joinpath(tempdir(), "")
    def_prefix = "jl_"
    mktempdir() do tmpdir
        @test isdir(tmpdir)
        @test startswith(tmpdir, tmpdirbase * def_prefix)
        @test sizeof(tmpdir) == sizeof(tmpdirbase) + sizeof(def_prefix) + 6
        @test sizeof(basename(tmpdir)) == sizeof(def_prefix) + 6
        cd(tmpdir) do
            Sys.iswindows() || mkdir(".\\")
            for relpath in (".", "./", ".\\", "")
                mktempdir(relpath) do tmpdir2
                    pfx = joinpath(relpath, def_prefix)
                    @test sizeof(tmpdir2) == sizeof(pfx) + 6
                    @test startswith(tmpdir2, pfx)
                end
            end
        end
    end
    # Special character prefix tests
    for tst_prefix in ("ABCDEF", "./pfx", ".\\pfx", "", "#!@%^&()-", "/", "\\", "////abc", "\\\\\\\\abc", "∃x∀y")
        mktempdir(; prefix=tst_prefix) do tmpdir
            @test isdir(tmpdir)
            @test startswith(tmpdir, tmpdirbase * tst_prefix)
            @test sizeof(basename(tmpdir)) == 6 + sizeof(basename(tst_prefix))
        end
    end

    @test_throws Base.IOError mktempdir(; prefix="dir_notexisting/bar")
    @test_throws Base.IOError mktempdir(; prefix="dir_notexisting/")
    @test_throws Base.IOError mktempdir("dir_notexisting/")

    # Behavioral differences across OS types
    if Sys.iswindows()
        # invalid file name
        @test_throws Base.IOError mktempdir(; prefix="a*b")
        @test_throws Base.IOError mktempdir("a*b")
    end

    mktempdir(""; prefix=tmpdirbase) do tmpdir
        @test startswith(tmpdir, tmpdirbase)
        @test sizeof(tmpdir) == 6 + sizeof(tmpdirbase)
        @test sizeof(basename(tmpdir)) == 6
    end
end

@testset "pwd tests" begin
    mktempdir() do dir
        cd(dir) do
            withenv("OLDPWD" => nothing) do
                io = IOBuffer()
                Base.repl_cmd(@cmd("cd"), io)
                Base.repl_cmd(@cmd("cd -"), io)
                @test realpath(pwd()) == realpath(dir)
                if !Sys.iswindows()
                    # Delete the working directory and check we can cd out of it
                    # Cannot delete the working directory on Windows
                    rm(dir)
                    @test_throws Base._UVError("pwd()", Base.UV_ENOENT) pwd()
                    Base.repl_cmd(@cmd("cd \\~"), io)
                end
            end
        end
    end
end

@testset "readdir tests" begin
    ≛(a, b) = sort(a) == sort(b)
    mktempdir() do dir
        d = cd(pwd, dir) # might resolve symlinks
        @test isempty(readdir(d))
        @test isempty(readdir(d, join=true))
        cd(d) do
            @test isempty(readdir())
            @test isempty(readdir(join=true))
        end
        touch(joinpath(d, "file"))
        mkdir(joinpath(d, "dir"))
        names = ["dir", "file"]
        paths = [joinpath(d, x) for x in names]
        @test readdir(d) ≛ names
        @test readdir(d, join=true) ≛ paths
        cd(d) do
            @test readdir() ≛ names
            @test readdir(join=true) ≛ paths
        end
        t, b = splitdir(d)
        cd(t) do
            @test readdir(b) ≛ names
            @test readdir(b, join=true) ≛ [joinpath(b, x) for x in names]
        end
    end
    if !Sys.iswindows()
        mktempdir() do dir
            cd(dir) do
                d = pwd() # might resolve symlinks
                @test isdir(d)
                @test Base.Filesystem.samefile(d, ".")
                @test isempty(readdir())
                @test isempty(readdir(d))
                @test isempty(readdir(join=true))
                rm(d, recursive=true)
                @test !ispath(d)
                @test isempty(readdir())
                @test_throws Base._UVError("readdir($(repr(d)))", Base.UV_ENOENT) readdir(d)
                @test_throws Base._UVError("pwd()", Base.UV_ENOENT) readdir(join=true)
            end
        end
    end
end

if Sys.isunix()
    @testset "mkfifo" begin
        mktempdir() do dir
            path = Libc.mkfifo(joinpath(dir, "fifo"))
            @sync begin
                @async write(path, "hello")
                cat_exec = `$(Base.julia_cmd()) --startup-file=no -e "write(stdout, read(ARGS[1]))"`
                @test read(`$cat_exec $path`, String) == "hello"
            end

            existing_file = joinpath(dir, "existing")
            write(existing_file, "")
            @test_throws SystemError Libc.mkfifo(existing_file)
        end
    end
else
    @test_throws(
        "mkfifo: Operation not supported",
        Libc.mkfifo(joinpath(pwd(), "dummy_path")),
    )
end

@testset "chmod/isexecutable/isreadable/iswritable" begin
    mktempdir() do dir
        subdir = joinpath(dir, "subdir")
        fpath = joinpath(dir, "subdir", "foo")

        @test !ispath(subdir)
        mkdir(subdir)
        @test ispath(subdir)

        @test !ispath(fpath)
        touch(fpath)
        @test ispath(fpath)

        # Test that we can actually set the executable/readable/writeable bit on all platforms.
        chmod(fpath, 0o644)
        @test !Sys.isexecutable(fpath)
        @test Sys.isreadable(fpath)
        @test Sys.iswritable(fpath) skip=Sys.iswindows()
        chmod(fpath, 0o755)
        @test Sys.isexecutable(fpath)
        @test Sys.isreadable(fpath)
        @test Sys.iswritable(fpath) skip=Sys.iswindows()
        chmod(fpath, 0o444)
        @test !Sys.isexecutable(fpath)
        @test Sys.isreadable(fpath)
        @test !Sys.iswritable(fpath)
        chmod(fpath, 0o244)
        @test !Sys.isexecutable(fpath)
        @test !Sys.isreadable(fpath) skip=Sys.iswindows()
        @test Sys.iswritable(fpath) skip=Sys.iswindows()

        # Ensure that, on Windows, where inheritance is default,
        # chmod still behaves as we expect.
        if Sys.iswindows()
            chmod(subdir, 0o666)
            @test !Sys.isexecutable(fpath)
            @test Sys.isreadable(fpath)
            @test_skip Sys.iswritable(fpath)
        end

        # Reset permissions to all at the end, so it can be deleted properly.
        chmod(dir, 0o777; recursive=true)
    end
end

if Sys.iswindows()
@testset "mkdir/rm permissions" begin
    # test delete permission in system folders (i.e. implicitly test chmod permissions)
    # issue #38433
    @test withenv("TMP" => "C:\\") do
        mktempdir() do dir end
    end === nothing
    # same as above, but test rm explicitly
    tmp = mkdir(tempname("C:\\"))
    @test rm(tmp) === nothing
end
end

# Unusually for structs, we test this explicitly because the fields of StatStruct
# is part of its documentation, and therefore cannot change.
@testset "StatStruct has promised fields" begin
    f, io = mktemp()
    s = stat(f)
    @test s isa Base.StatStruct

    @test s.desc isa Union{String, Base.OS_HANDLE}
    @test s.size isa Int64
    @test s.device isa UInt
    @test s.inode isa UInt
    @test s.mode isa UInt
    @test s.nlink isa Int
    @test s.uid isa UInt
    @test s.gid isa UInt
    @test s.rdev isa UInt
    @test s.blksize isa Int64
    @test s.blocks isa Int64
    @test s.mtime isa Float64
    @test s.ctime isa Float64

    @test s === stat((f,))
    @test s === lstat((f,))
    @test s === stat(".", f)
    @test s === lstat(".", f)
end

mutable struct URI50890; f::String; end
Base.joinpath(x::URI50890) = URI50890(x.f)
@test_throws "stat not implemented" stat(URI50890("."))
@test_throws "lstat not implemented" lstat(URI50890("."))

@testset "StatStruct show's extended details" begin
    f, io = mktemp()
    s = stat(f)
    stat_show_str = sprint(show, s)
    stat_show_str_multi = sprint(show, MIME("text/plain"), s)
    @test startswith(stat_show_str, "StatStruct(")
    @test endswith(stat_show_str, ")")
    @test startswith(stat_show_str_multi, "StatStruct for ")
    @test rstrip(stat_show_str_multi) == stat_show_str_multi # no trailing \n
    @test occursin(repr(f), stat_show_str)
    @test occursin(repr(f), stat_show_str_multi)
    if Sys.iswindows()
        @test occursin("mode: 0o100666 (-rw-rw-rw-)", stat_show_str)
        @test occursin("mode: 0o100666 (-rw-rw-rw-)\n", stat_show_str_multi)
    else
        @test occursin("mode: 0o100600 (-rw-------)", stat_show_str)
        @test occursin("mode: 0o100600 (-rw-------)\n", stat_show_str_multi)
    end
    if Sys.iswindows() == false
        @test !isnothing(Base.Filesystem.getusername(s.uid))
        @test !isnothing(Base.Filesystem.getgroupname(s.gid))
    end
    d = mktempdir()
    s = stat(d)
    stat_show_str = sprint(show, s)
    stat_show_str_multi = sprint(show, MIME("text/plain"), s)
    @test startswith(stat_show_str, "StatStruct(")
    @test endswith(stat_show_str, ")")
    @test startswith(stat_show_str_multi, "StatStruct for ")
    @test rstrip(stat_show_str_multi) == stat_show_str_multi # no trailing \n
    @test occursin(repr(d), stat_show_str)
    @test occursin(repr(d), stat_show_str_multi)
    if Sys.iswindows()
        @test occursin("mode: 0o040666 (drw-rw-rw-)", stat_show_str)
        @test occursin("mode: 0o040666 (drw-rw-rw-)\n", stat_show_str_multi)
    else
        @test occursin("mode: 0o040700 (drwx------)", stat_show_str)
        @test occursin("mode: 0o040700 (drwx------)\n", stat_show_str_multi)
    end
    if Sys.iswindows() == false
        @test !isnothing(Base.Filesystem.getusername(s.uid))
        @test !isnothing(Base.Filesystem.getgroupname(s.gid))
    end
    s = Base.Filesystem.StatStruct()
    stat_show_str = sprint(show, s)
    stat_show_str_multi = sprint(show, MIME("text/plain"), s)
    @test startswith(stat_show_str, "StatStruct(\"\" ENOENT: ") && endswith(stat_show_str, ")")
    @test startswith(stat_show_str_multi, "StatStruct for \"\"\n ENOENT: ") && !endswith(stat_show_str_multi, r"\s")
    s = Base.Filesystem.StatStruct("my/test", Ptr{UInt8}(0), Int32(Base.UV_ENOTDIR))
    stat_show_str = sprint(show, s)
    stat_show_str_multi = sprint(show, MIME("text/plain"), s)
    @test startswith(stat_show_str, "StatStruct(\"my/test\" ENOTDIR: ") && endswith(stat_show_str, ")")
    @test startswith(stat_show_str_multi, "StatStruct for \"my/test\"\n ENOTDIR: ") && !endswith(stat_show_str_multi, r"\s")
end

@testset "diskstat() works" begin
    # Sanity check assuming disk is smaller than 32PB
    PB = Int64(2)^44

    dstat = diskstat()
    @test dstat.total < 32PB
    @test dstat.used + dstat.available == dstat.total
    @test occursin(r"^DiskStat\(total=\d+, used=\d+, available=\d+\)$", sprint(show, dstat))
    # Test diskstat(::AbstractString)
    dstat = diskstat(pwd())
    @test dstat.total < 32PB
    @test dstat.used + dstat.available == dstat.total
end

@test Base.infer_return_type(stat, (String,)) == Base.Filesystem.StatStruct
