using Tokenize

nt = @timed @eval(collect(Tokenize.tokenize("foo + bar", Tokens.RawToken)))
println("First run took $(nt.time) seconds with $(nt.bytes/1e6) MB allocated")

srcdir = joinpath(Sys.BINDIR, Base.DATAROOTDIR, "..")

allfiles = []
for (root, dirs, files) in walkdir(srcdir, follow_symlinks = true)
    for file in files
        splitext(file)[2] == ".jl" || continue
        push!(allfiles, joinpath(root, file))
    end
end

# warmup
let time_taken = 0.0, allocated = 0.0
    for file in allfiles
        content = IOBuffer(read(file, String))
        nt = @timed for t in Tokenize.tokenize(content, Tokens.RawToken) end
        time_taken += nt.time
        allocated += nt.bytes
    end
end

# actual run
let time_taken = 0.0, allocated = 0.0
    for file in allfiles
        content = IOBuffer(read(file, String))
        nt = @timed for t in Tokenize.tokenize(content, Tokens.RawToken) end
        time_taken += nt.time
        allocated += nt.bytes
    end
    println("Tokenized $(length(allfiles)) files in $(time_taken) seconds with $(allocated/1e6) MB allocated")
end

isempty(ARGS) && exit(0)

using PProf, Profile

# warm up profiler
let content = read(first(allfiles), String)
    @profile collect(Tokenize.tokenize(content, Tokens.RawToken))
end

Profile.clear()
for file in allfiles
    content = read(file, String)
    @profile collect(Tokenize.tokenize(content, Tokens.RawToken))
end
pprof()

println("Press any key to exit...")
readline()
