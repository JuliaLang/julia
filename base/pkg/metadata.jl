module Metadata

import Git
import Base.isequal, Base.isless, Base.contains, Base.hash

export parse_requires, Version, VersionSet

function gen_versions(pkg::String)
    for (ver,sha1) in Git.each_tagged_version(pkg)
        dir = "METADATA/$pkg/versions/$ver"
        run(`mkdir -p $dir`)
        open("$dir/sha1","w") do io
            println(io,sha1)
        end
        if isfile("$pkg/REQUIRE")
            run(`cp $pkg/REQUIRE $dir/requires`)
        end
    end
end
gen_versions() = for pkg in each_package() gen_versions(pkg) end

function gen_hashes(pkg::String)
    for (ver,dir) in each_tagged_version(pkg)
        sha1 = readchomp("$dir/sha1")
        run(`mkdir -p METADATA/$pkg/hashes`)
        open("METADATA/$pkg/hashes/$sha1","w") do io
            println(io,ver)
        end
    end
end
gen_hashes() = for pkg in each_package() gen_hashes(pkg) end

function pkg_url(pkg::String)
    path = "METADATA/$pkg/url"
    isfile(path) ? readchomp(path) : nothing
end

function version(pkg::String, sha1::String)
    path = "METADATA/$pkg/hashes/$sha1"
    isfile(path) || Metadata.gen_hashes(pkg)
    isfile(path) ? convert(VersionNumber,readchomp(path)) : sha1
end

each_package() = @task begin
    for line in each_line(`ls -1 METADATA`)
        line = chomp(line)
        # stat() chokes if we try to check if the subdirectory of a non-directory exists
        if isdir(joinpath("METADATA", line)) && isdir(joinpath("METADATA", line, "versions"))
            produce(line)
        end
    end
end

each_tagged_version(pkg::String) = @task begin
    for line in each_line(`ls -1 $(joinpath("METADATA", pkg, "versions"))`)
        line = chomp(line)
        if isdir(joinpath("METADATA", pkg, "versions", line)) && ismatch(Base.VERSION_REGEX, line)
            ver = convert(VersionNumber,line)
            dir = "METADATA/$pkg/versions/$(line)"
            if isfile("$dir/sha1")
                produce((ver,dir))
            end
        end
    end
end

function packages()
    pkgs = String[]
    for pkg in each_package()
        push!(pkgs,pkg)
    end
    sort!(pkgs)
end

type Version
    package::ByteString
    version::VersionNumber
end

isequal(a::Version, b::Version) =
    a.package == b.package && a.version == b.version
function isless(a::Version, b::Version)
    (a.package < b.package) && return true
    (a.package > b.package) && return false
    return a.version < b.version
end

function versions(pkgs)
    vers = Version[]
    for pkg in pkgs
        for (ver,dir) in each_tagged_version(pkg)
            push!(vers,Version(pkg,ver))
        end
    end
    sort!(vers)
end
versions() = versions(packages())

hash(v::Version) = hash([v.(n) for n in Version.names])

type VersionSet
    package::ByteString
    versions::Vector{VersionNumber}

    function VersionSet(pkg::ByteString, vers::Vector{VersionNumber})
        if !issorted(vers)
            error("version numbers must be sorted")
        end
        new(pkg,vers)
    end
end
VersionSet(pkg::ByteString) = VersionSet(pkg, VersionNumber[])

isequal(a::VersionSet, b::VersionSet) =
    a.package == b.package && a.versions == b.versions
isless(a::VersionSet, b::VersionSet) = a.package < b.package

function contains(s::VersionSet, v::Version)
    (s.package != v.package) && return false
    for i in length(s.versions):-1:1
        (v.version >= s.versions[i]) && return isodd(i)
    end
    return isempty(s.versions)
end

hash(s::VersionSet) = hash([s.(n) for n in VersionSet.names])

function parse_requires(readable)
    reqs = VersionSet[]
    for line in each_line(readable)
        if ismatch(r"^\s*(?:#|$)", line) continue end
        line = replace(line, r"#.*$", "")
        fields = split(line)
        pkg = shift!(fields)
        vers = [ convert(VersionNumber,x) for x=fields ]
        if !issorted(vers)
            error("invalid requires entry for $pkg: $vers")
        end
        # TODO: merge version sets instead of appending?
        push!(reqs,VersionSet(pkg,vers))
    end
    sort!(reqs)
end
parse_requires(file::String) = open(parse_requires,file)

function dependencies(pkgs)
    deps = Array((Version,VersionSet),0)
    for pkg in each_package()
        for (ver,dir) in each_tagged_version(pkg)
            v = Version(pkg,ver)
            file = "$dir/requires"
            if isfile(file)
                for d in parse_requires("$dir/requires")
                    if !contains(pkgs,d.package)
                        error("unknown dependency for $pkg: $(d.package)")
                    end
                    push!(deps,(v,d))
                end
            end
        end
    end
    sort!(deps)
end

older(a::Version, b::Version) = a.package == b.package && a.version < b.version

end # module
