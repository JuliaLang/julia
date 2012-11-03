## paths

@unix_only begin
    @assert split_extension(".bashrc") == (".bashrc","")
    @assert split_extension("/dir/.bashrc") == ("/dir/.bashrc","")
    @assert split_extension("a.b/a") == ("a.b/a","")
    @assert split_extension("a/a.b.c") == ("a/a.b",".c")

    @assert split_path("a/b/c") == ["a","b","c"]
    @assert split_path("a//b/c") == ["a","b","c"]
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
@assert isdir(dir_name) == true
@assert isfile(dir_name) == false
@assert islink(dir_name) == false
@assert isdir(filename) == false
@assert isfile(filename) == true
@assert islink(filename) == false
@assert isreadable(filename) == true
@assert iswriteable(filename) == true
# Here's something else that might be UNIX-specific?
run(`chmod -w $filename`)
@assert iswriteable(filename) == false
run(`chmod +w $filename`)
@assert isexecutable(filename) == false
@assert filesize(filename) == 0
@assert filesize(dir_name) > 0
@assert mtime(filename) >= mtime(dir_name)

# rename file
newfilename = file_path(dir_name, "bfile.txt")
path_rename(filename, newfilename)
@assert ispath(filename) == false
@assert isfile(newfilename) == true
filename = newfilename

#######################################################################
# This section tests temporary file and directory creation.           #
#######################################################################

# my_tempdir = tempdir()
# @assert isdir(my_tempdir) == true

# path = tempname()
# @assert ispath(path) == false

# (filename, f) = mktemp()
# print(f, "Here is some text")
# close(f)
# @assert isfile(filename) == true
# @assert readall(filename) == "Here is some text"

############
# Clean up #
############
file_remove(filename)
rmdir(dir_name)
@assert ispath(filename) == false
@assert ispath(dir_name) == false
