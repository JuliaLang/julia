#############################################
# Create some temporary files & directories #
#############################################
dir = mktempdir()
file = joinpath(dir, "afile.txt")
close(open(file,"w")) # like touch, but lets the operating system update the timestamp for greater precision on some platforms (windows)

@unix_only begin
    link = joinpath(dir, "afilelink.txt")
    symlink(file, link)
end

subdir = joinpath(dir, "adir")
mkdir(subdir)
@non_windowsxp_only begin
    dirlink = joinpath(dir, "dirlink")
    symlink(subdir, dirlink)
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
let skew = 10  # allow 10s skew
    now   = time()
    mfile = mtime(file)
    mdir  = mtime(dir)
    @test abs(now - mfile) <= skew && abs(now - mdir) <= skew && abs(mfile - mdir) <= skew
end
#@test int(time()) >= int(mtime(file)) >= int(mtime(dir)) >= 0 # 1 second accuracy should be sufficient

# test links
@unix_only @test islink(link) == true
@non_windowsxp_only begin
    @test islink(dirlink) == true
    @test isdir(dirlink) == true
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

# Commented out the tests below due to issues 3015, 3016 and 3020
test_timeout(0.1)
test_timeout(1)
# the 0.1 second tests are too optimistic
#test_touch(0.1)
test_touch(2)
#test_monitor(0.1)
test_monitor(2)
test_monitor_wait(0.1)

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
c = mmap_array(UInt8, (uint16(11),), s)
@test c == "Hello World".data
@test_throws ErrorException mmap_array(UInt8, (int16(-11),), s)
@test_throws ErrorException mmap_array(UInt8, (typemax(UInt),), s)
close(s)
s = open(file, "r+")
@test isreadonly(s) == false
c = mmap_array(UInt8, (11,), s)
c[5] = uint8('x')
msync(c)
close(s)
s = open(file, "r")
str = readline(s)
close(s)
@test beginswith(str, "Hellx World")
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
@test_throws ErrorException mmap_bitarray((7,3), s)
close(s)
s = open(file, "r+")
b = mmap_bitarray((17,19), s)
rand!(b)
msync(b)
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
@test beginswith(str, "Marked!")
@test ismarked(s)
reset(s)
@test !ismarked(s)
str = readline(s)
@test beginswith(str, "Marked!")
mark(s)
@test readline(s) == "Hello world!\n"
@test ismarked(s)
unmark(s)
@test !ismarked(s)
@test_throws ErrorException reset(s)
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

# Test copy file
afile = joinpath(dir, "a.txt")
touch(afile)
af = open(afile, "r+")
write(af, "This is indeed a test")

bfile = joinpath(dir, "b.txt")
cp(afile, bfile)

# issue #8698
cfile = joinpath(dir, "c.txt")
open(cfile, "w") do cf
    write(cf, "This is longer than the contents of afile")
end
cp(afile, cfile)

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

###################
# FILE* interface #
###################

f = open(file, "w")
write(f, "Hello, world!")
close(f)
f = open(file, "r")
FILEp = convert(CFILE, f)
buf = Array(UInt8, 8)
str = ccall(:fread, Csize_t, (Ptr{Void}, Csize_t, Csize_t, Ptr{Void}), buf, 1, 8, FILEp.ptr)
@test bytestring(buf) == "Hello, w"
@test position(FILEp) == 8
seek(FILEp, 5)
@test position(FILEp) == 5
close(f)

############
# Clean up #
############
@unix_only rm(link)
@non_windowsxp_only rm(dirlink)

rm(file)
rm(subdir)
rm(dir)

# The following fail on Windows with "stat: operation not permitted (EPERM)"
@unix_only @test !ispath(file)
@unix_only @test !ispath(dir)
