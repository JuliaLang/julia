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
# On windows the filesize of a folder is the accumulation of all the contained 
# files ans is thus zero in this case. 
@windows_only begin
	@assert filesize(dir_name) == 0
end
@unix_only begin
@assert filesize(dir_name) > 0
end
@assert mtime(filename) >= mtime(dir_name)

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

# dirname = mktempdir()
# @assert isdir(dirname)
 
############
# Clean up #
############
file_remove(filename)
rmdir(dir_name)
