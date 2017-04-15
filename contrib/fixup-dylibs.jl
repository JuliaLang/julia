cd(string(JULIA_HOME, "/../lib"))
alllibs = Dict{String,Bool}()

# Find all the installed dynamic libraries 
for file in readdir(string(JULIA_HOME, "/../lib")) 
    (isdylib, _) = search(file, ".dylib")
    if isdylib > 0 && isfile(file)
        alllibs[file] = true
    end
end

# Find the dependencies and rewrite
for file in keys(alllibs) # for all dynamic libraries
    println(file)
    deps = split(readall(`otool -L $file`), "\n") # find all the dependencies
    ndeps = length(deps)
    for d = 2:ndeps # process each dependency
        depname = match(r"lib[a-z,A-Z,\d]*.\d*.dylib", deps[d])
        if depname != nothing
            if has(alllibs, depname.match) # rewrite relative paths if dependency is available
                depname_fullpath = match(r"\t.*.dylib", deps[d])
                if depname_fullpath != nothing
                    println("\t install_name_tool -id @rpath/$(depname.match) $file")
                    run(`install_name_tool -id @rpath/$(depname.match) $file`)
                end
            end
        end
    end
end
