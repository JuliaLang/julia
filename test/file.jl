#############################################
# Create some temporary files & directories #
#############################################
# This code assumes the availability of POSIX tools in the current PATH
# TODO: remove this dependency
dir_name = strcat("$(systmpdir())/testdir", randstring(6))
dir_create(dir_name)
filename = strcat(dir_name, "/afile.txt")
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

############
# Clean up #
############
file_remove(filename)
dir_remove(dir_name)
