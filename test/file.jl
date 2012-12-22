## paths

@unix_only begin
    @test split_extension(".bashrc") == (".bashrc","")
    @test split_extension("/dir/.bashrc") == ("/dir/.bashrc","")
    @test split_extension("a.b/a") == ("a.b/a","")
    @test split_extension("a/a.b.c") == ("a/a.b",".c")

    @test split_path("a/b/c") == ["a","b","c"]
    @test split_path("a//b/c") == ["a","b","c"]
end

#############################################
# Create some temporary files & directories #
#############################################
# This first section may not run for non-UNIX people.
# If so, create the directories and files manually, and comment out this section
# (Or fix up the code to support such operations on Windows!)
dir_name = mktempdir()
filename = file_path(dir_name, "afile.txt")
file_create(filename)

#######################################################################
# This section tests some of the features of the stat-based file info #
#######################################################################
@test isdir(dir_name) == true
@test isfile(dir_name) == false
@test islink(dir_name) == false
@test isdir(filename) == false
@test isfile(filename) == true
@test islink(filename) == false
@test isreadable(filename) == true
@test iswriteable(filename) == true
# Here's something else that might be UNIX-specific?
run(`chmod -w $filename`)
@test iswriteable(filename) == false
run(`chmod +w $filename`)
@test isexecutable(filename) == false
@test filesize(filename) == 0
@test filesize(dir_name) > 0
@test mtime(filename) >= mtime(dir_name)

# rename file
newfilename = file_path(dir_name, "bfile.txt")
path_rename(filename, newfilename)
@test ispath(filename) == false
@test isfile(newfilename) == true
filename = newfilename

#######################################################################
# This section tests temporary file and directory creation.           #
#######################################################################

# my_tempdir = tempdir()
# @test isdir(my_tempdir) == true

# path = tempname()
# @test ispath(path) == false

# (filename, f) = mktemp()
# print(f, "Here is some text")
# close(f)
# @test isfile(filename) == true
# @test readall(filename) == "Here is some text"

############
# Clean up #
############
file_remove(filename)
rmdir(dir_name)
@test ispath(filename) == false
@test ispath(dir_name) == false
