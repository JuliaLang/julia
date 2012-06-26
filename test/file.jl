#############################################
# Create some temporary files & directories #
#############################################
# This first section may not run for non-UNIX people.
# If so, create the directories and files manually, and comment out this section
# (Or fix up the code to support such operations on Windows!)
dir_name = strcat("/tmp/testdir", randstring(6))
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
@assert filesize(dir_name) > 0
@assert mtime(filename) >= mtime(dir_name)

############
# Clean up #
############
file_remove(filename)
dir_remove(dir_name)
