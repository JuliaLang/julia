require("linprog")

module Metadata

using LinProgGLPK

import Git
import GLPK
import Base.isequal, Base.isless, Base.contains

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
        if isdir(file_path("METADATA", line)) && isdir(file_path("METADATA", line, "versions"))
            produce(line)
        end
    end
end

each_tagged_version(pkg::String) = @task begin
    for line in each_line(`ls -1 $(file_path("METADATA", pkg, "versions"))`)
        line = chomp(line)
        if isdir(file_path("METADATA", pkg, "versions", line)) && ismatch(Base.VERSION_REGEX, line)
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
        push(pkgs,pkg)
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
            push(vers,Version(pkg,ver))
        end
    end
    sort!(vers)
end
versions() = versions(packages())

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
isless(a::VersionSet, b::VersionSet) = a.package < b.package

function contains(s::VersionSet, v::Version)
    (s.package != v.package) && return false
    for i in length(s.versions):-1:1
        (v.version >= s.versions[i]) && return isodd(i)
    end
    return isempty(s.versions)
end

function parse_requires(file::String)
    reqs = VersionSet[]
    open(file) do io
        for line in each_line(io)
            if ismatch(r"^\s*(?:#|$)", line) continue end
            line = replace(line, r"#.*$", "")
            fields = split(line)
            pkg = shift(fields)
            vers = [ convert(VersionNumber,x) for x=fields ]
            if !issorted(vers)
                error("invalid requires entry for $pkg in $file: $vers")
            end
            # TODO: merge version sets instead of appending?
            push(reqs,VersionSet(pkg,vers))
        end
    end
    sort!(reqs)
end

function dependencies(pkgs,vers)
    deps = Array((Version,VersionSet),0)
    for pkg in each_package()
        for (ver,dir) in each_tagged_version(pkg)
            v = Version(pkg,ver)
            file = "$dir/requires"
            if isfile(file)
                for d in parse_requires("$dir/requires")
                    if !contains(pkgs,d.package)
                        error("Unknown dependency for $pkg: $(d.package)")
                    end
                    push(deps,(v,d))
                end
            end
        end
    end
    sort!(deps)
end

older(a::Version, b::Version) = a.package == b.package && a.version < b.version

function resolve(reqs::Vector{VersionSet})
    pkgs = packages()
    vers = versions(pkgs)
    deps = dependencies(pkgs,vers)

    n = length(vers)
    z = zeros(Int,n)
    u = ones(Int,n)

    G  = [ v == d[1]        ? 1 : 0  for v=vers, d=deps ]
    G *= [ contains(d[2],v) ? 1 : 0  for d=deps, v=vers ]
    G += [ older(a,b)       ? 2 : 0  for a=vers, b=vers ]
    I = find(G)
    W = zeros(Int,length(I),n)
    for (r,i) in enumerate(I)
        W[r,rem(i-1,n)+1] = -1
        W[r,div(i-1,n)+1] = G[i]
    end
    mipopts = GLPK.IntoptParam()
    mipopts["msg_lev"] = GLPK.MSG_ERR
    mipopts["presolve"] = GLPK.ON
    _, ws, flag, _ = mixintprog(u,W,-ones(Int,length(I)),nothing,nothing,u,nothing,nothing,mipopts)
    if flag != 0
        msg = sprint(print_linprog_flag, flag)
        error("resolve() failed: $msg.")
    end
    w = iround(ws)

    V = [ p == v.package ? 1 : 0                     for p=pkgs, v=vers ]
    R = [ contains(r,v) ? -1 : 0                     for r=reqs, v=vers ]
    D = [ d[1] == v ? 1 : contains(d[2],v) ? -1 : 0  for d=deps, v=vers ]
    b = [  ones(Int,length(pkgs))
          -ones(Int,length(reqs))
          zeros(Int,length(deps)) ]

    _, xs, flag, _ = mixintprog(w,[V;R;D],b,nothing,nothing,z,u,nothing,mipopts)
    if flag != 0
        msg = sprint(print_linprog_flag, flag)
        error("resolve() failed: $msg.")
    end
    x = bool(xs)
    h = (String=>ASCIIString)[]
    for v in vers[x]
        h[v.package] = readchomp("METADATA/$(v.package)/versions/$(v.version)/sha1")
    end
    return h
end

end # module
