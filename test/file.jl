# This file is a part of Julia. License is MIT: http://julialang.org/license

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

if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
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

if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    @test islink(dirlink) == true
    @test isdir(dirlink) == true
    @test readlink(dirlink) == subdir * @windows? "\\" : ""
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

## mv ----------------------------------------------------
mktempdir() do tmpdir
    # rename file
    file = joinpath(tmpdir, "afile.txt")
    files_stat = stat(file)
    close(open(file,"w")) # like touch, but lets the operating system update the timestamp for greater precision on some platforms (windows)

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
if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
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
@unix_only begin
    function setup_files(tmpdir)
        srcfile = joinpath(tmpdir, "srcfile.txt")
        hidden_srcfile = joinpath(tmpdir, ".hidden_srcfile.txt")
        srcfile_new = joinpath(tmpdir, "srcfile_new.txt")
        hidden_srcfile_new = joinpath(tmpdir, ".hidden_srcfile_new.txt")
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
        @test readall(s) == readall(d) == file_txt
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
        @test readall(s) == readall(d_mv) == file_txt
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
        open(otherfile, "w") do f
            write(f, otherfile_content)
        end
        for d in test_new_paths1
            cp(otherfile, d; remove_destination=true, follow_symlinks=false)
            # Expect no link because a file is copied (follow_symlinks=false does not effect this)
            @test isfile(d) && !islink(d)
            # all should contain otherfile_content
            @test readall(d) == otherfile_content
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
        # As expected this will leave some absolute links brokern #11145#issuecomment-99315168
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

# issue #10994: pathnames cannot contain embedded NUL chars
for f in (mkdir, cd, Base.FS.unlink, readlink, rm, touch, readdir, mkpath, stat, lstat, ctime, mtime, filemode, filesize, uperm, gperm, operm, touch, isblockdev, ischardev, isdir, isexecutable, isfifo, isfile, islink, ispath, isreadable, issetgid, issetuid, issocket, issticky, iswritable, realpath, watch_file)
    @test_throws ArgumentError f("adir\0bad")
end
@test_throws ArgumentError chmod("ba\0d", 0o222)
@test_throws ArgumentError open("ba\0d", "w")
@test_throws ArgumentError cp(file, "ba\0d")
@test_throws ArgumentError mv(file, "ba\0d")
if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    @test_throws ArgumentError symlink(file, "ba\0d")
else
    @test_throws ErrorException symlink(file, "ba\0d")
end
@test_throws ArgumentError download("good", "ba\0d")
@test_throws ArgumentError download("ba\0d", "good")

############
# Clean up #
############
@unix_only begin
    rm(link)
    rm(rellink)
end
if @unix? true : (Base.windows_version() >= Base.WINDOWS_VISTA_VER)
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
