# Write the sys source cache in format readable by Base._read_dependency_src
cachefile, source_paths = ARGS

# (cache key, file to read) pairs. The manifest is missing when the system image base
# was built elsewhere (CROSS_BOOTSTRAP_SYSBASE); then the keys are the files themselves.
sources = if isfile(source_paths)
    open(source_paths) do paths
        pairs = Pair{String,String}[]
        while !eof(paths)
            push!(pairs, readuntil(paths, '\0') => readuntil(paths, '\0'))
        end
        pairs
    end
else
    [filename => filename for (_, filename) in Base._included_files]
end

open(cachefile, "w") do io
    for (filename, original) in sources
        src = read(original, String)
        write(io, Int32(sizeof(filename)))
        write(io, filename)
        write(io, UInt64(sizeof(src)))
        write(io, src)
    end
    write(io, Int32(0))
end
