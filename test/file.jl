#############################################
# Create some temporary files & directories #
#############################################
starttime = time()
pwd_ = pwd()
dir = mktempdir()
file = joinpath(dir, "afile.txt")
close(open(file,"w")) # like touch, but lets the operating system update the timestamp for greater precision on some platforms (windows)

subdir = joinpath(dir, "adir")
mkdir(subdir)
subdir2 = joinpath(dir, "adir2")
mkdir(subdir2)

@non_windowsxp_only begin
    dirlink = joinpath(dir, "dirlink")
    symlink(subdir, dirlink)
    # relative link
    cd(subdir)
    relsubdirlink = joinpath(subdir, "rel_subdirlink")
    reldir = joinpath("..", "adir2")
    symlink(reldir, relsubdirlink)
    cd(pwd_)
end

@unix_only begin
    link = joinpath(dir, "afilelink.txt")
    symlink(file, link)
    # relative link
    cd(subdir)
    rellink = joinpath(subdir, "rel_afilelink.txt")
    relfile = joinpath("..", "afile.txt")
    symlink(relfile, rellink)
    cd(pwd_)
end


#######################################################################
# This section tests some of the features of the stat-based file info #
#######################################################################
@test isdir(dir)
@test !isfile(dir)
@test !islink(dir)
@test !isdir(file)
@test isfile(file)
@test !islink(file)
@test isreadable(file)
@test iswritable(file)
chmod(file, filemode(file) & 0o7555)
@test !iswritable(file)
chmod(file, filemode(file) | 0o222)
@test !isexecutable(file)
@test filesize(file) == 0
# On windows the filesize of a folder is the accumulation of all the contained
# files and is thus zero in this case.
@windows_only @test filesize(dir) == 0
@unix_only @test filesize(dir) > 0
now = time()
# Allow 10s skew in addition to the time it took us to actually execute this code
let skew = 10 + (now - starttime)
    mfile = mtime(file)
    mdir  = mtime(dir)
    @test abs(now - mfile) <= skew && abs(now - mdir) <= skew && abs(mfile - mdir) <= skew
end
#@test Int(time()) >= Int(mtime(file)) >= Int(mtime(dir)) >= 0 # 1 second accuracy should be sufficient

# test links
@unix_only @test islink(link) == true
@unix_only @test readlink(link) == file

@non_windowsxp_only begin
    @test islink(dirlink) == true
    @test isdir(dirlink) == true
    @test readlink(dirlink) == subdir * @windows? "\\" : ""
end

# rename file
newfile = joinpath(dir, "bfile.txt")
mv(file, newfile)
@test !ispath(file)
@test isfile(newfile)
file = newfile

# Test renaming directories
a_tmpdir = mktempdir()
b_tmpdir = joinpath(dir, "b_tmpdir")

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

# rm recursive TODO add links
c_tmpdir = mktempdir()
c_subdir = joinpath(c_tmpdir, "c_subdir")
mkdir(c_subdir)
c_file = joinpath(c_tmpdir, "cfile.txt")
cp(newfile, c_file)

@test isdir(c_subdir)
@test isfile(c_file)
@test_throws SystemError rm(c_tmpdir)

rm(c_tmpdir, recursive=true)
@test !isdir(c_tmpdir)


#######################################################################
# This section tests file watchers.                                   #
#######################################################################
function test_file_poll(channel,timeout_s)
    rc = poll_file(file, round(Int,timeout_s/10), timeout_s)
    put!(channel,rc)
end

function test_timeout(tval)
    tic()
    channel = RemoteRef()
    @async test_file_poll(channel,tval)
    tr = take!(channel)
    t_elapsed = toq()
    @test !tr
    @test tval <= t_elapsed
end

function test_touch(slval)
    tval = slval*1.1
    channel = RemoteRef()
    @async test_file_poll(channel, tval)
    sleep(tval/10)  # ~ one poll period
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)
    tr = take!(channel)
    @test tr
end


function test_monitor(slval)
    FsMonitorPassed = false
    fm = FileMonitor(file) do args...
        FsMonitorPassed = true
    end
    sleep(slval/2)
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)
    sleep(slval)
    @test FsMonitorPassed
    close(fm)
end

function test_monitor_wait(tval)
    fm = watch_file(file)
    @async begin
        sleep(tval)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    fname, events = wait(fm)
    @test fname == basename(file)
    @test events.changed
end

function test_monitor_wait_poll(tval)
    fm = watch_file(file, poll=true)
    @async begin
        sleep(tval)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    fname, events = wait(fm)
    @test fname == basename(file)
    @test events.writable
end

# Commented out the tests below due to issues 3015, 3016 and 3020
test_timeout(0.1)
test_timeout(1)
# the 0.1 second tests are too optimistic
#test_touch(0.1)
test_touch(2)
#test_monitor(0.1)
test_monitor(2)
test_monitor_wait(0.1)
test_monitor_wait_poll(0.5)

##########
#  mmap  #
##########

s = open(file, "w")
write(s, "Hello World\n")
close(s)
s = open(file, "r")
@test isreadonly(s) == true
c = mmap_array(UInt8, (11,), s)
@test c == "Hello World".data
c = mmap_array(UInt8, (UInt16(11),), s)
@test c == "Hello World".data
@test_throws ArgumentError mmap_array(UInt8, (Int16(-11),), s)
@test_throws ArgumentError mmap_array(UInt8, (typemax(UInt),), s)
close(s)
s = open(file, "r+")
@test isreadonly(s) == false
c = mmap_array(UInt8, (11,), s)
c[5] = UInt8('x')
Libc.msync(c)
close(s)
s = open(file, "r")
str = readline(s)
close(s)
@test startswith(str, "Hellx World")
c=nothing; gc(); gc(); # cause munmap finalizer to run & free resources

s = open(file, "w")
write(s, [0xffffffffffffffff,
          0xffffffffffffffff,
          0xffffffffffffffff,
          0x000000001fffffff])
close(s)
s = open(file, "r")
@test isreadonly(s)
b = mmap_bitarray((17,13), s)
@test b == trues(17,13)
@test_throws ArgumentError mmap_bitarray((7,3), s)
close(s)
s = open(file, "r+")
b = mmap_bitarray((17,19), s)
rand!(b)
Libc.msync(b)
b0 = copy(b)
close(s)
s = open(file, "r")
@test isreadonly(s)
b = mmap_bitarray((17,19), s)
@test b == b0
close(s)
b=nothing; b0=nothing; gc(); gc(); # cause munmap finalizer to run & free resources

# mmap with an offset
A = rand(1:20, 500, 300)
fname = tempname()
s = open(fname, "w+")
write(s, size(A,1))
write(s, size(A,2))
write(s, A)
close(s)
s = open(fname)
m = read(s, Int)
n = read(s, Int)
A2 = mmap_array(Int, (m,n), s)
@test A == A2
seek(s, 0)
A3 = mmap_array(Int, (m,n), s, convert(FileOffset,2*sizeof(Int)))
@test A == A3
A4 = mmap_array(Int, (m,150), s, convert(FileOffset,(2+150*m)*sizeof(Int)))
@test A[:, 151:end] == A4
close(s)
A2=nothing; A3=nothing; A4=nothing; gc(); gc(); # cause munmap finalizer to run & free resources
rm(fname)

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
@test readline(s) == "Hello world!\n"
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

my_tempdir = tempdir()
@test isdir(my_tempdir) == true

path = tempname()
# Issue #9053.
@unix_only @test ispath(path) == false
@windows_only @test ispath(path) == true

(p, f) = mktemp()
print(f, "Here is some text")
close(f)
@test isfile(p) == true
@test readall(p) == "Here is some text"
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
@test isempty(readlines(emptyf))
close(emptyf)
rm(emptyfile)


###########################################################################
## This section tests cp files, directories, absolute and relative links. #
###########################################################################
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
                @test readall(orig_path) == readall(copied_path)
            end
        end
    elseif isdir(orig_path)
        check_cp_main(orig_path, copied_path, follow_symlinks)
    else
        # copied_path must also be a file.
        @test isfile(copied_path)
        # copied_path must have same content
        @test readall(orig_path) == readall(copied_path)
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

# issue #8698
# Test copy file
afile = joinpath(dir, "a.txt")
touch(afile)
af = open(afile, "r+")
write(af, "This is indeed a test")

bfile = joinpath(dir, "b.txt")
cp(afile, bfile)

cfile = joinpath(dir, "c.txt")
open(cfile, "w") do cf
    write(cf, "This is longer than the contents of afile")
end
cp(afile, cfile; remove_destination=true)

a_stat = stat(afile)
b_stat = stat(bfile)
c_stat = stat(cfile)
@test a_stat.mode == b_stat.mode
@test a_stat.size == b_stat.size
@test a_stat.size == c_stat.size

close(af)
rm(afile)
rm(bfile)
rm(cfile)

# issue #10506 #10434
## Tests for directories and links to directories
@non_windowsxp_only begin
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
        open(cfile, "w") do cf
            write(cf, file_txt)
        end
        hidden_cfile = joinpath(hidden_srcsubdir, "c.txt")
        open(hidden_cfile, "w") do cf
            write(cf, file_txt)
        end

        abs_dirlink_cp = joinpath(tmpdir, "abs_dirlink_cp")
        hidden_srcsubdir_cp = joinpath(tmpdir, ".hidden_srcsubdir_cp")
        path_rel_dirlink = joinpath(tmpdir, rel_dirlink)
        path_rel_dirlink_cp = joinpath(tmpdir, "rel_dirlink_cp")

        test_src_paths = [srcdir, hidden_srcsubdir, abs_dirlink, path_rel_dirlink]
        test_cp_paths = [srcdir_cp, hidden_srcsubdir_cp, abs_dirlink_cp, path_rel_dirlink_cp]
        return test_src_paths, test_cp_paths
    end

    function cp_follow_symlinks_false_check(s, d; remove_destination=false)
        cp(s, d; remove_destination=remove_destination, follow_symlinks=false)
        @test isdir(s) == isdir(d)
        @test islink(s) == islink(d)
        islink(s) && @test readlink(s) == readlink(d)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d))
        # all should contain 1 file named  "c.txt"
        @test "c.txt" in readdir(d)
        @test length(readdir(d)) == 1
    end

    ## Test require `remove_destination=true` (remove destination first) for existing
    #  directories and existing links to directories
    mktempdir() do tmpdir
        # Setup new copies for the test
        test_src_paths, test_cp_paths = setup_dirs(tmpdir)
        for (s, d) in zip(test_src_paths, test_cp_paths)
            cp_follow_symlinks_false_check(s, d)
        end
        # Test require `remove_destination=true`
        for s in test_src_paths
            for d in test_cp_paths
                @test_throws ArgumentError cp(s, d; remove_destination=false)
                @test_throws ArgumentError cp(s, d; remove_destination=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy
        for (s, d) in zip(test_src_paths, test_cp_paths)
            cp_follow_symlinks_false_check(s, d; remove_destination=true)
        end
        # Test remove the existing path first and copy an empty dir
        emptydir = joinpath(tmpdir, "emptydir")
        mkdir(emptydir)
        for d in test_cp_paths
            cp(emptydir, d; remove_destination=true, follow_symlinks=false)
            # Expect no link because a dir is copied (follow_symlinks=false does not effect this)
            @test isdir(d) && !islink(d)
            # none should contain any file
            @test isempty(readdir(d))
        end
    end

    # Test full: absolute and relative directory links
    mktempdir() do tmpdir
        maindir = joinpath(tmpdir, "mytestdir")
        mkdir(maindir)
        targetdir = abspath(joinpath(maindir, "targetdir"))
        mkdir(targetdir)
        subdir1 = joinpath(maindir, "subdir1")
        mkdir(subdir1)

        cfile = abspath(joinpath(maindir, "c.txt"))
        open(cfile, "w") do cf
            write(cf, "This is c.txt - 这是一个文件")
        end
        open(abspath(joinpath(targetdir, "file1.txt")), "w") do cf
            write(cf, "This is file1.txt - 这是一个文件")
        end

        abs_dl = joinpath(maindir, "abs_linkto_targetdir")
        symlink(targetdir, abs_dl)
        # Setup relative links
        cd(subdir1)
        rel_dl = "rel_linkto_targetdir"
        rel_dir = joinpath("..", "targetdir")
        symlink(rel_dir, rel_dl)
        cd(pwd_)
        # TEST: Directory with links: Test each option
        maindir_cp = joinpath(dirname(maindir),"maindir_cp")
        maindir_cp_keepsym = joinpath(dirname(maindir),"maindir_cp_keepsym")
        cp_and_test(maindir, maindir_cp, true)
        cp_and_test(maindir, maindir_cp_keepsym, false)
    end
end

# issue #10506 #10434
## Tests for files and links to files as well as directories and links to directories
@unix_only begin
    function setup_files(tmpdir)
        srcfile = joinpath(tmpdir, "srcfile.txt")
        hidden_srcfile = joinpath(tmpdir, ".hidden_srcfile.txt")
        srcfile_cp = joinpath(tmpdir, "srcfile_cp.txt")
        hidden_srcfile_cp = joinpath(tmpdir, ".hidden_srcfile_cp.txt")
        file_txt = "This is some text with unicode - 这是一个文件"
        open(srcfile, "w") do f
            write(f, file_txt)
        end
        open(hidden_srcfile, "w") do f
            write(f, file_txt)
        end
        abs_filelink = joinpath(tmpdir, "abs_filelink")
        symlink(abspath(srcfile), abs_filelink)
        cd(tmpdir)
        rel_filelink = "rel_filelink"
        symlink("srcfile.txt", rel_filelink)
        cd(pwd_)

        abs_filelink_cp = joinpath(tmpdir, "abs_filelink_cp")
        path_rel_filelink = joinpath(tmpdir, rel_filelink)
        path_rel_filelink_cp = joinpath(tmpdir, "rel_filelink_cp")

        test_src_paths = [srcfile, hidden_srcfile, abs_filelink, path_rel_filelink]
        test_cp_paths = [srcfile_cp, hidden_srcfile_cp, abs_filelink_cp, path_rel_filelink_cp]
        return test_src_paths, test_cp_paths, file_txt
    end

    function cp_follow_symlinks_false_check(s, d, file_txt; remove_destination=false)
        cp(s, d; remove_destination=remove_destination, follow_symlinks=false)
        @test isfile(s) == isfile(d)
        @test islink(s) == islink(d)
        islink(s) && @test readlink(s) == readlink(d)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d))
        # all should contain the same
        @test readall(s) == readall(d) == file_txt
    end

    ## Test require `remove_destination=true` (remove destination first) for existing
    #  files and existing links to files
    mktempdir() do tmpdir
        # Setup new copies for the test
        test_src_paths, test_cp_paths, file_txt = setup_files(tmpdir)
        for (s, d) in zip(test_src_paths, test_cp_paths)
            cp_follow_symlinks_false_check(s, d, file_txt)
        end
        # Test require `remove_destination=true`
        for s in test_src_paths
            for d in test_cp_paths
                @test_throws ArgumentError cp(s, d; remove_destination=false)
                @test_throws ArgumentError cp(s, d; remove_destination=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy: follow_symlinks=false
        for (s, d) in zip(test_src_paths, test_cp_paths)
            cp_follow_symlinks_false_check(s, d, file_txt; remove_destination=true)
        end
        # Test remove the existing path first and copy an other file
        otherfile = joinpath(tmpdir, "otherfile.txt")
        otherfile_content = "This is otherfile.txt with unicode - 这是一个文件"
        open(otherfile, "w") do f
            write(f, otherfile_content)
        end
        for d in test_cp_paths
            cp(otherfile, d; remove_destination=true, follow_symlinks=false)
            # Expect no link because a file is copied (follow_symlinks=false does not effect this)
            @test isfile(d) && !islink(d)
            # all should contain otherfile_content
            @test readall(d) == otherfile_content
        end
    end

    # Test full: absolute and relative file links and absolute and relative directory links
    mktempdir() do tmpdir
        maindir = joinpath(tmpdir, "mytestdir")
        mkdir(maindir)
        targetdir = abspath(joinpath(maindir, "targetdir"))
        mkdir(targetdir)
        subdir1 = joinpath(maindir, "subdir1")
        mkdir(subdir1)

        cfile = abspath(joinpath(maindir, "c.txt"))
        open(cfile, "w") do cf
            write(cf, "This is c.txt - 这是一个文件")
        end
        open(abspath(joinpath(targetdir, "file1.txt")), "w") do cf
            write(cf, "This is file1.txt - 这是一个文件")
        end

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
        rel_file_read_txt = readall(rel_file)
        cd(pwd_)
        # Setup copytodir
        copytodir = joinpath(tmpdir, "copytodir")
        mkdir(copytodir)
        cp(cfile, joinpath(copytodir, basename(cfile)))
        subdir_test = joinpath(copytodir, "subdir_test")
        mkdir(subdir_test)
        cp(targetdir, joinpath(copytodir, basename(targetdir)); follow_symlinks=false)
        # TEST: Directory with links: Test each option
        maindir_cp =  joinpath(dirname(maindir),"maindir_cp")
        maindir_cp_keepsym =  joinpath(dirname(maindir),"maindir_cp_keepsym")
        cp_and_test(maindir, maindir_cp, true)
        cp_and_test(maindir, maindir_cp_keepsym, false)

        ## Tests single Files, File Links
        rel_flpath = joinpath(subdir1, rel_fl)
        # `cp file`
        cp_and_test(cfile, joinpath(copytodir,"cfile_cp.txt"), true)
        cp_and_test(cfile, joinpath(copytodir,"cfile_cp_keepsym.txt"), false)
        # `cp absolute file link`
        cp_and_test(abs_fl, joinpath(copytodir,"abs_fl_cp.txt"), true)
        cp_and_test(abs_fl, joinpath(copytodir,"abs_fl_cp_keepsym.txt"), false)
        # `cp relative file link`
        cp_and_test(rel_flpath, joinpath(subdir_test,"rel_fl_cp.txt"), true)
        cp_and_test(rel_flpath, joinpath(subdir_test,"rel_fl_cp_keepsym.txt"), false)
    end
end


###################
# FILE* interface #
###################

function test_LibcFILE(FILEp)
    buf = Array(UInt8, 8)
    str = ccall(:fread, Csize_t, (Ptr{Void}, Csize_t, Csize_t, Ptr{Void}), buf, 1, 8, FILEp)
    @test bytestring(buf) == "Hello, w"
    @test position(FILEp) == 8
    seek(FILEp, 5)
    @test position(FILEp) == 5
    close(FILEp)
end

f = open(file, "w")
write(f, "Hello, world!")
close(f)
f = open(file, "r")
test_LibcFILE(convert(Libc.FILE, f))
close(f)
@unix_only f = RawFD(ccall(:open, Cint, (Ptr{Uint8}, Cint), file, Base.FS.JL_O_RDONLY))
@windows_only f = RawFD(ccall(:_open, Cint, (Ptr{Uint8}, Cint), file, Base.FS.JL_O_RDONLY))
test_LibcFILE(Libc.FILE(f,Libc.modestr(true,false)))

############
# Clean up #
############
@unix_only begin
    rm(link)
    rm(rellink)
end
@non_windowsxp_only begin
    rm(dirlink)
    rm(relsubdirlink)
end
rm(file)
rm(subdir)
rm(subdir2)
rm(dir)

# The following fail on Windows with "stat: operation not permitted (EPERM)"
@unix_only @test !ispath(file)
@unix_only @test !ispath(dir)
