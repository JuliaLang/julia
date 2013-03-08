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
