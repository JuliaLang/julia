#############################################
# Create some temporary files & directories #
#############################################
# This first section may not run for non-UNIX people.
# If so, create the directories and files manually, and comment out this section
# (Or fix up the code to support such operations on Windows!)
dir = mktempdir()
file = joinpath(dir, "afile.txt")
touch(file)

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
@test iswriteable(file) == true
# Here's something else that might be UNIX-specific?
run(`chmod -w $file`)
@test iswriteable(file) == false
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
@test mtime(file) >= mtime(dir)

# rename file
newfile = joinpath(dir, "bfile.txt")
mv(file, newfile)
@test ispath(file) == false
@test isfile(newfile) == true
file = newfile

#######################################################################
# This section tests file watchers.                                   #
#######################################################################
function test_file_poll(channel,timeout_ms)
    rc = poll_file(file, iround(timeout_ms/10), timeout_ms)
    put(channel,rc)
end

function test_timeout(tval)
    t1 = int64(time() * 1000)
    channel = RemoteRef()
    @async test_file_poll(channel,tval)
    tr = take(channel)
    t2 = int64(time() * 1000)

    @test tr == (:timeout, 0)

    tdiff = t2-t1
    @test tval <= tdiff
end

function test_touch(slval)
    tval = slval+100
    channel = RemoteRef()
    @async test_file_poll(channel,iround(tval))

    sleep(slval/10_000) # ~one poll period
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)

    tr = take(channel)

    @test tr[1] == :poll
    @test tr[2] == 0
end


function test_monitor(slval)
    FsMonitorPassed = false
    fm = FileMonitor(file) do args...
        FsMonitorPassed = true
    end
    sleep(slval/10_000)
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)
    sleep(9slval/10_000)
    @test FsMonitorPassed
    close(fm)
end

test_timeout(100)
test_timeout(1000)
test_touch(100)
test_touch(1000)
test_monitor(1000)
test_monitor(100)



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

############
# Clean up #
############
rm(file)
rmdir(dir)
@test ispath(file) == false
@test ispath(dir) == false
