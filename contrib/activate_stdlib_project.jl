# This file is a part of Julia. License is MIT: https://julialang.org/license

# Write a project listing every installed stdlib into the directory given as the second
# argument, and activate it. The cache file names depend on this path, so it must not
# change between builds.
let dir = ARGS[2], path = joinpath(dir, "Project.toml")
    stdlibs = filter(name -> isfile(joinpath(Sys.STDLIB, name, "Project.toml")), readdir(Sys.STDLIB))
    project = sprint() do io
        println(io, "[deps]")
        for name in stdlibs
            uuid = Base.parsed_toml(joinpath(Sys.STDLIB, name, "Project.toml"))["uuid"]
            println(io, name, " = \"", uuid, "\"")
        end
    end
    # two builds can run this at once, so never leave the file half written
    if !isfile(path) || read(path, String) != project
        tmp, io = mktemp(dir)
        write(io, project)
        close(io)
        Base.rename(tmp, path)
    end
    # a manifest here would be used in place of each stdlib's own dependency list
    manifest = Base.project_file_manifest_path(path)
    manifest === nothing || rm(manifest; force=true)
    Base.set_active_project(dir)
end
