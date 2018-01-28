# Write the sys source cache in format readable by Base._read_dependency_src
cachefile = ARGS[1]
open(cachefile, "w") do io
    for (_, filename) in Base._included_files
        src = read(filename, String)
        write(io, hton(Int32(sizeof(filename))))
        write(io, filename)
        write(io, hton(UInt64(sizeof(src))))
        write(io, src)
    end
    write(io, Int32(0))
end
