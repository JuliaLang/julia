# This file is a part of Julia. License is MIT: http://julialang.org/license

#############################################
# Create some temporary files & directories #
#############################################
starttime = time()
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
@test_throws SystemError mkdir(file)
let err = nothing
    try
        mkdir(file)
    catch err
        io = IOBuffer()
        showerror(io, err)
        @test startswith(String(take!(io)), "SystemError (with $file): mkdir:")
    end
end

if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    dirlink = joinpath(dir, "dirlink")
    symlink(subdir, dirlink)
    # relative link
    cd(subdir)
    relsubdirlink = joinpath(subdir, "rel_subdirlink")
    reldir = joinpath("..", "adir2")
    symlink(reldir, relsubdirlink)
    cd(pwd_)
end

if !is_windows()
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

@test filemode(file) & 0o444 > 0 # readable
@test filemode(file) & 0o222 > 0 # writable
chmod(file, filemode(file) & 0o7555)
@test filemode(file) & 0o222 == 0
chmod(file, filemode(file) | 0o222)
@test filemode(file) & 0o111 == 0
@test filesize(file) == 0

if is_windows()
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
    mktempdir() do tmpdir
        tmpfile=joinpath(tmpdir, "tempfile.txt")
        touch(tmpfile)
        chmod(tmpfile, 0o707)
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

# On windows the filesize of a folder is the accumulation of all the contained
# files and is thus zero in this case.
if is_windows()
    @test filesize(dir) == 0
else
    @test filesize(dir) > 0
end
nowtime = time()
# Allow 10s skew in addition to the time it took us to actually execute this code
let skew = 10 + (nowtime - starttime)
    mfile = mtime(file)
    mdir  = mtime(dir)
    @test abs(nowtime - mfile) <= skew && abs(nowtime - mdir) <= skew && abs(mfile - mdir) <= skew
end
#@test Int(time()) >= Int(mtime(file)) >= Int(mtime(dir)) >= 0 # 1 second accuracy should be sufficient

# test links
if is_unix()
    @test islink(link) == true
    @test readlink(link) == file
end

if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
    @test islink(dirlink) == true
    @test isdir(dirlink) == true
    @test readlink(dirlink) == subdir * (is_windows() ? "\\" : "")
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
@test_throws SystemError rm(c_tmpdir)
@test_throws SystemError rm(c_tmpdir, force=true)

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
@test_throws Base.UVError rm(c_tmpdir)
@test rm(c_tmpdir, force=true) === nothing
@test_throws Base.UVError rm(c_tmpdir, recursive=true)
@test rm(c_tmpdir, force=true, recursive=true) === nothing

if !is_windows()
    # chown will give an error if the user does not have permissions to change files
    if get(ENV, "USER", "") == "root" || get(ENV, "HOME", "") == "/root"
        chown(file, -2, -1)  # Change the file owner to nobody
        @test stat(file).uid !=0
        chown(file, 0, -2)  # Change the file group to nogroup (and owner back to root)
        @test stat(file).gid !=0
        @test stat(file).uid ==0
        chown(file, -1, 0)
        @test stat(file).gid ==0
        @test stat(file).uid ==0
    else
        @test_throws Base.UVError chown(file, -2, -1)  # Non-root user cannot change ownership to another user
        @test_throws Base.UVError chown(file, -1, -2)  # Non-root user cannot change group to a group they are not a member of (eg: nogroup)
    end
else
    # test that chown doesn't cause any errors for Windows
    @test chown(file, -2, -2) === nothing
end

#######################################################################
# This section tests file watchers.                                   #
#######################################################################
function test_file_poll(channel,interval,timeout_s)
    rc = poll_file(file, interval, timeout_s)
    put!(channel,rc)
end

function test_timeout(tval)
    tic()
    channel = Channel(1)
    @async test_file_poll(channel, 10, tval)
    tr = take!(channel)
    t_elapsed = toq()
    @test !ispath(tr[1]) && !ispath(tr[2])
    @test tval <= t_elapsed
end

function test_touch(slval)
    tval = slval*1.1
    channel = Channel(1)
    @async test_file_poll(channel, tval/3, tval)
    sleep(tval/3)  # one poll period
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)
    tr = take!(channel)
    @test ispath(tr[1]) && ispath(tr[2])
end

function test_watch_file_timeout(tval)
    watch = @async watch_file(file, tval)
    @test wait(watch) == Base.Filesystem.FileEvent(false, false, true)
end

function test_watch_file_change(tval)
    watch = @async watch_file(file, tval)
    sleep(tval/3)
    open(file, "a") do f
        write(f, "small change\n")
    end
    @test wait(watch) == Base.Filesystem.FileEvent(false, true, false)
end

function test_monitor_wait(tval)
    fm = FileMonitor(file)
    @async begin
        sleep(tval)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    fname, events = wait(fm)
    close(fm)
    @test fname == basename(file)
    @test events.changed
end

function test_monitor_wait_poll()
    pfw = PollingFileWatcher(file, 5.007)
    @async begin
        sleep(2.5)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    (old, new) = wait(pfw)
    close(pfw)
    @test new.mtime - old.mtime > 2.5 - 1.5 # mtime may only have second-level accuracy (plus add some hysteresis)
end

test_timeout(0.1)
test_timeout(1)
test_touch(6)
test_monitor_wait(0.1)
test_monitor_wait(0.1)
test_monitor_wait_poll()
test_monitor_wait_poll()
test_watch_file_timeout(0.1)
test_watch_file_change(6)

@test_throws Base.UVError watch_file("____nonexistent_file", 10)
@test_throws Base.UVError poll_file("____nonexistent_file", 2, 10)

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

my_tempdir = tempdir()
@test isdir(my_tempdir) == true

path = tempname()
# Issue #9053.
@test ispath(path) == is_windows()
ispath(path) && rm(path)

(p, f) = mktemp()
print(f, "Here is some text")
close(f)
@test isfile(p) == true
@test readstring(p) == "Here is some text"
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
@test isempty(readlines(emptyf, chomp=false))
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
                @test readstring(orig_path) == readstring(copied_path)
            end
        end
    elseif isdir(orig_path)
        check_cp_main(orig_path, copied_path, follow_symlinks)
    else
        # copied_path must also be a file.
        @test isfile(copied_path)
        # copied_path must have same content
        @test readstring(orig_path) == readstring(copied_path)
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
afile = joinpath(dir, "a.txt")
touch(afile)
af = open(afile, "r+")
write(af, "This is indeed a test")

bfile = joinpath(dir, "b.txt")
cp(afile, bfile)

cfile = joinpath(dir, "c.txt")
write(cfile, "This is longer than the contents of afile")
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

## mv ----------------------------------------------------
mktempdir() do tmpdir
    # rename file
    file = joinpath(tmpdir, "afile.txt")
    files_stat = stat(file)
    close(open(file,"w")) # like touch, but lets the operating system update
    # the timestamp for greater precision on some platforms (windows)

    newfile = joinpath(tmpdir, "bfile.txt")
    mv(file, newfile)
    newfile_stat = stat(file)

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

# issue #10506 #10434
## Tests for directories and links to directories
if !is_windows() || Sys.windows_version() >= Sys.WINDOWS_VISTA_VER
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

    function mv_check(s, d, d_mv; remove_destination=true)
        # setup dest
        cp(s, d; remove_destination=true, follow_symlinks=false)
        stat_d = stat(d)
        # mv(rename) dst to dst_mv
        mv(d, d_mv; remove_destination=remove_destination)
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

    ## Test require `remove_destination=true` (remove destination first) for existing
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
        # Test require `remove_destination=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError cp(s, d; remove_destination=false)
                @test_throws ArgumentError cp(s, d; remove_destination=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            cp_follow_symlinks_false_check(s, d; remove_destination=true)
        end
        # Test remove the existing path first and copy an empty dir
        emptydir = joinpath(maindir1, "emptydir")
        mkdir(emptydir)
        for d in test_new_paths1
            cp(emptydir, d; remove_destination=true, follow_symlinks=false)
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
            cp_follow_symlinks_false_check(s, d; remove_destination=true)
        end
        for (s, d) in zip(test_src_paths2, test_new_paths2)
            cp_follow_symlinks_false_check(s, d; remove_destination=true)
        end

        # Test require `remove_destination=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError mv(s, d; remove_destination=false)
            end
        end
        # Test remove the existing path first and move
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv_check(s, d, d_mv; remove_destination=true)
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
            mv(d, d_mv; remove_destination=true)
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
                @test_throws ArgumentError Base.cptree(src,dst; remove_destination=true, follow_symlinks=false)
                @test_throws ArgumentError Base.cptree(src,dst; remove_destination=true, follow_symlinks=true)
                # cp
                @test_throws ArgumentError cp(src,dst; remove_destination=true, follow_symlinks=false)
                @test_throws ArgumentError cp(src,dst; remove_destination=true, follow_symlinks=true)
                # mv
                @test_throws ArgumentError mv(src,dst; remove_destination=true)
            end
        end
    end
    # None existing src
    mktempdir() do tmpdir
        none_existing_src = joinpath(tmpdir, "none_existing_src")
        dst = joinpath(tmpdir, "dst")
        @test !ispath(none_existing_src)
        # cptree
        @test_throws ArgumentError Base.cptree(none_existing_src,dst; remove_destination=true, follow_symlinks=false)
        @test_throws ArgumentError Base.cptree(none_existing_src,dst; remove_destination=true, follow_symlinks=true)
        # cp
        @test_throws Base.UVError cp(none_existing_src,dst; remove_destination=true, follow_symlinks=false)
        @test_throws Base.UVError cp(none_existing_src,dst; remove_destination=true, follow_symlinks=true)
        # mv
        @test_throws Base.UVError mv(none_existing_src,dst; remove_destination=true)
    end
end

# issue #10506 #10434
## Tests for files and links to files as well as directories and links to directories
if !is_windows()
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

    function cp_follow_symlinks_false_check(s, d, file_txt; remove_destination=false)
        cp(s, d; remove_destination=remove_destination, follow_symlinks=false)
        @test isfile(s) == isfile(d)
        @test islink(s) == islink(d)
        islink(s) && @test readlink(s) == readlink(d)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d))
        # all should contain the same
        @test readstring(s) == readstring(d) == file_txt
    end

    function mv_check(s, d, d_mv, file_txt; remove_destination=true)
        # setup dest
        cp(s, d; remove_destination=true, follow_symlinks=false)
        stat_d = stat(d)
        # mv(rename) dst to dst_mv
        mv(d, d_mv; remove_destination=remove_destination)
        stat_d_mv = stat(d_mv)
        # make sure d does not exist anymore
        @test !ispath(d)
        # comare s, with d_mv
        @test isfile(s) == isfile(d_mv)
        @test islink(s) == islink(d_mv)
        islink(s) && @test readlink(s) == readlink(d_mv)
        islink(s) && @test isabspath(readlink(s)) == isabspath(readlink(d_mv))
        # all should contain the same
        @test readstring(s) == readstring(d_mv) == file_txt
        # d => d_mv same file/dir
        @test Base.samefile(stat_d, stat_d_mv)
    end

    ## Test require `remove_destination=true` (remove destination first) for existing
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
        # Test require `remove_destination=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError cp(s, d; remove_destination=false)
                @test_throws ArgumentError cp(s, d; remove_destination=false, follow_symlinks=true)
            end
        end
        # Test remove the existing path first and copy
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            cp_follow_symlinks_false_check(s, d, file_txt2; remove_destination=true)
        end
        # Test remove the existing path first and copy an other file
        otherfile = joinpath(tmpdir, "otherfile.txt")
        otherfile_content = "This is otherfile.txt with unicode - 这是一个文件"
        write(otherfile, otherfile_content)
        for d in test_new_paths1
            cp(otherfile, d; remove_destination=true, follow_symlinks=false)
            # Expect no link because a file is copied (follow_symlinks=false does not effect this)
            @test isfile(d) && !islink(d)
            # all should contain otherfile_content
            @test readstring(d) == otherfile_content
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
        # Test require `remove_destination=true`
        for s in test_src_paths1
            for d in test_new_paths2
                @test_throws ArgumentError mv(s, d; remove_destination=false)
            end
        end
        # Test remove the existing path first and move
        # need to use here the test_src_paths2:
        # otherwise ArgumentError: 'src' and 'dst' refer to the same file/dir.This is not supported.
        for (s, d) in zip(test_src_paths2, test_new_paths1)
            d_mv = joinpath(dirname(d), "$(basename(d))_mv")
            mv_check(s, d, d_mv, file_txt2; remove_destination=true)
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
        rel_file_read_txt = readstring(rel_file)
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
            mv(d, d_mv; remove_destination=true)
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
                @test_throws ArgumentError Base.cptree(src,dst; remove_destination=true, follow_symlinks=false)
                @test_throws ArgumentError Base.cptree(src,dst; remove_destination=true, follow_symlinks=true)
                # cp
                @test_throws ArgumentError cp(src,dst; remove_destination=true, follow_symlinks=false)
                @test_throws ArgumentError cp(src,dst; remove_destination=true, follow_symlinks=true)
                # mv
                @test_throws ArgumentError mv(src,dst; remove_destination=true)
            end
        end
    end
    # None existing src: not needed here as it is done above with the directories.
end


###################
# FILE* interface #
###################

function test_LibcFILE(FILEp)
    buf = Array{UInt8}(8)
    str = ccall(:fread, Csize_t, (Ptr{Void}, Csize_t, Csize_t, Ptr{Void}), buf, 1, 8, FILEp)
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
    test_LibcFILE(convert(Libc.FILE, f))
    close(f)
    if is_windows()
        f = RawFD(ccall(:_open, Cint, (Cstring, Cint), file, Base.Filesystem.JL_O_RDONLY))
    else
        f = RawFD(ccall(:open, Cint, (Cstring, Cint), file, Base.Filesystem.JL_O_RDONLY))
    end
    test_LibcFILE(Libc.FILE(f, Libc.modestr(true, false)))
end

# issue #10994: pathnames cannot contain embedded NUL chars
for f in (mkdir, cd, Base.Filesystem.unlink, readlink, rm, touch, readdir, mkpath,
        stat, lstat, ctime, mtime, filemode, filesize, uperm, gperm, operm, touch,
        isblockdev, ischardev, isdir, isfifo, isfile, islink, ispath, issetgid,
        issetuid, issocket, issticky, realpath, watch_file, poll_file)
    @test_throws ArgumentError f("adir\0bad")
end
@test_throws ArgumentError chmod("ba\0d", 0o222)
@test_throws ArgumentError open("ba\0d", "w")
@test_throws ArgumentError cp(file, "ba\0d")
@test_throws ArgumentError mv(file, "ba\0d")
if !is_windows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    @test_throws ArgumentError symlink(file, "ba\0d")
else
    @test_throws ErrorException symlink(file, "ba\0d")
end
@test_throws ArgumentError download("good", "ba\0d")
@test_throws ArgumentError download("ba\0d", "good")

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
    has_symlinks = !is_windows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    follow_symlink_vec = has_symlinks ? [true, false] : [false]
    has_symlinks && symlink(abspath("sub_dir2"), joinpath("sub_dir1", "link"))
    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks)
        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir1")
        @test dirs == (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

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
        @test dirs ==  (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

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
    @test_throws SystemError take!(chnl_error) # throws an error because sub_dir1 do not exist

    root, dirs, files = take!(chnl_noerror)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    root, dirs, files = take!(chnl_noerror) # skips sub_dir1 as it no longer exist
    @test root == joinpath(".", "sub_dir2")
    @test dirs == []
    @test files == ["file_dir2"]
end
rm(dirwalk, recursive=true)

############
# Clean up #
############
if !is_windows()
    rm(link)
    rm(rellink)
end
if !is_windows() || (Sys.windows_version() >= Sys.WINDOWS_VISTA_VER)
    rm(dirlink)
    rm(relsubdirlink)
end
rm(file)
rm(subdir)
rm(subdir2)
rm(dir)

@test !ispath(file)
@test !ispath(dir)

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

#issue #12992
function test_12992()
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    gc()
    gc()
end

# Make sure multiple close is fine
function test2_12992()
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    gc()
    gc()
end

test_12992()
test_12992()
test_12992()

test2_12992()
test2_12992()
test2_12992()

# issue 13559
if !is_windows()
function test_13559()
    fn = tempname()
    run(`mkfifo $fn`)
    # use subprocess to write 127 bytes to FIFO
    writer_cmds = "x=open(\"$fn\", \"w\"); for i=1:127 write(x,0xaa); flush(x); sleep(0.1) end; close(x); quit()"
    open(pipeline(`$(Base.julia_cmd()) --startup-file=no -e $writer_cmds`, stderr=STDERR))
    #quickly read FIFO, draining it and blocking but not failing with EOFError yet
    r = open(fn, "r")
    # 15 proper reads
    for i=1:15
        @test read(r, Int64) == -6148914691236517206
    end
    # last read should throw EOFError when FIFO closes, since there are only 7 bytes available.
    @test_throws EOFError read(r, Int64)
    close(r)
    rm(fn)
end
test_13559()
end
@test_throws ArgumentError mkpath("fakepath",-1)
