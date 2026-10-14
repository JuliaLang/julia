# Write the sys source cache in format readable by Base._read_dependency_src
cachefile, source_paths = ARGS

open(source_paths) do paths
    open(cachefile, "w") do io
        while !eof(paths)
            filename = readuntil(paths, '\0')
            original = readuntil(paths, '\0')
            src = read(original, String)
            write(io, Int32(sizeof(filename)))
            write(io, filename)
            write(io, UInt64(sizeof(src)))
            write(io, src)
        end
        write(io, Int32(0))
    end
end
