#############################################
# Create some temporary files & directories #
#############################################
dir = mktempdir()
file = joinpath(dir, "afile.txt")
close(open(file,"w")) # like touch, but lets the operating system update the timestamp for greater precision on some platforms (windows)

#######################################################################
# This section tests some of the features of the stat-based file info #
#######################################################################
@test isdir(dir) == true
@test isfile(dir) == false
@test islink(dir) == false
@test isdir(file) == false
@test isfile(file) == true
@test islink(file) == false
@test isreadable(file) == true
@test iswritable(file) == true
# Here's something else that might be UNIX-specific?
run(`chmod -w $file`)
@test iswritable(file) == false
run(`chmod +w $file`)
@test isexecutable(file) == false
@test filesize(file) == 0
# On windows the filesize of a folder is the accumulation of all the contained 
# files and is thus zero in this case. 
@windows_only begin
    @test filesize(dir) == 0
end
@unix_only begin
    @test filesize(dir) > 0
end
@test int(time()) >= int(mtime(file)) >= int(mtime(dir)) >= 0 # 1 second accuracy should be sufficient

# rename file
newfile = joinpath(dir, "bfile.txt")
mv(file, newfile)
@test ispath(file) == false
@test isfile(newfile) == true
file = newfile

#######################################################################
# This section tests file watchers.                                   #
#######################################################################
function test_file_poll(channel,timeout_s)
    rc = poll_file(file, iround(timeout_s/10), timeout_s)
    put(channel,rc)
end

function test_timeout(tval)
    tic()
    channel = RemoteRef()
    @async test_file_poll(channel,tval)
    tr = take(channel)
    t_elapsed = toq()

    @test tr == false

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

    tr = take(channel)

    @test tr == true
end


function test_monitor(slval)
    FsMonitorPassed = false
    fm = FileMonitor(file) do args...
        FsMonitorPassed = true
    end
    sleep(slval)
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
test_touch(1)
#test_monitor(0.1)
test_monitor(1)
test_monitor_wait(0.1)

##########
#  mmap  #
##########

s = open(file, "w")
write(s, "Hello World\n")
close(s)
s = open(file, "r")
@test isreadonly(s) == true
c = mmap_array(Uint8, (11,), s)
@test c == "Hello World".data
close(s)
s = open(file, "r+")
@test isreadonly(s) == false
c = mmap_array(Uint8, (11,), s)
c[5] = uint8('x')
msync(c)
close(s)
s = open(file, "r")
str = readline(s)
close(s)
@test beginswith(str, "Hellx World")
c=nothing; gc(); gc(); # cause munmap finalizer to run & free resources

#######################################################################
# This section tests temporary file and directory creation.           #
#######################################################################

# my_tempdir = tempdir()
# @test isdir(my_tempdir) == true

# path = tempname()
# @test ispath(path) == false

# (file, f) = mktemp()
# print(f, "Here is some text")
# close(f)
# @test isfile(file) == true
# @test readall(file) == "Here is some text"

emptyfile = joinpath(dir, "empty")
touch(emptyfile)
emptyf = open(emptyfile)
@test isempty(readlines(emptyf))
close(emptyf)
rm(emptyfile)

############
# Clean up #
############
rm(file)
rmdir(dir)
@test ispath(file) == false
@test ispath(dir) == false
