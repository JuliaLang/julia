module Pkg2
import Base: Git, isequal, isless, hash

@unix_only    const DIR_NAME = ".julia"
@windows_only const DIR_NAME = "packages"

function dir()
    d = abspath(get(ENV, "JULIA_PKGDIR", joinpath(ENV["HOME"], DIR_NAME)))
    x, y = VERSION.major, VERSION.minor
    dxy = joinpath(d, "v$x.$y")
    isdir(dxy) && return dxy
    dx = joinpath(d, "v$x")
    isdir(dx) && return dx
    return d
end
dir(pkg::String...) = joinpath(dir(), pkg...)

function cd_pkgdir(f::Function)
    d = dir()
    if !isdir(d)
        if has(ENV, "JULIA_PKGDIR")
            error("Package directory $d doesn't exist; run Pkg.init() to create it.")
        else
            info("Auto-initializing default package repository $d.")
            init()
        end
    end
    cd(f,d)
end

each_installed() = @task begin
    for line in eachline(`ls`)
        name = chomp(line)
        name == "METADATA" && continue
        name == "REQUIRE" && continue
        isfile(name, "src", "$name.jl") && produce(name)
    end
end

function isfixed(pkg::String)
    isfile("METDATA", pkg, "url") || return true
    ispath(pkg, ".git") || return true
    cd(pkg) do
        Git.dirty() && return true
        Git.attached() && return true
        for line in eachline(`git branch -r --contains`)
            return false
        end
        return true
    end
end

function requirements()
    reqs = parse_requires("REQUIRE")
    for pkg in each_installed()
        isfixed(pkg) || continue
        isfile(pkg, "REQUIRE") || continue
        append!(reqs, parse_requires(joinpath(pkg, "REQUIRE")))
    end
    sort!(reqs)
end

function parse_requires(readable)
    reqs = VersionSet[]
    for line in eachline(readable)
        if ismatch(r"^\s*(?:#|$)", line) continue end
        line = replace(line, r"#.*$", "")
        fields = split(line)
        pkg = shift!(fields)
        vers = [ convert(VersionNumber,x) for x in fields ]
        if !issorted(vers)
            error("invalid requires entry for $pkg: $vers")
        end
        # TODO: merge version sets instead of appending?
        push!(reqs,VersionSet(pkg,vers))
    end
    sort!(reqs)
end
parse_requires(file::String) = open(parse_requires,file)

### VersionSet ###

immutable VersionSet
    package::ByteString
    versions::Vector{VersionNumber}

    function VersionSet(pkg::ByteString, vers::Vector{VersionNumber})
        if !issorted(vers)
            error("version numbers must be sorted")
        end
        new(pkg, vers)
    end
end
VersionSet(pkg::ByteString) = VersionSet(pkg, VersionNumber[])

isequal(a::VersionSet, b::VersionSet) =
    a.package == b.package && a.versions == b.versions
isless(a::VersionSet, b::VersionSet) = a.package < b.package
hash(s::VersionSet) = hash([s.(n) for n in VersionSet.names])

end # module
