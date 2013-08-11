## semantic version numbers (http://semver.org)

immutable VersionNumber
    major::Int
    minor::Int
    patch::Int
    prerelease::(Union(Int,ASCIIString)...)
    build::(Union(Int,ASCIIString)...)

    function VersionNumber(major::Integer, minor::Integer, patch::Integer, pre::(Union(Int,ASCIIString)...), bld::(Union(Int,ASCIIString)...))
        major >= 0 || error("invalid negative major version: $major")
        minor >= 0 || error("invalid negative minor version: $minor")
        patch >= 0 || error("invalid negative patch version: $patch")
        for ident in pre
            if isa(ident,Int)
                ident >= 0 || error("invalid negative pre-release identifier: $ident")
            else
                if !ismatch(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && !(length(pre)==1 && isempty(bld))
                    error("invalid pre-release identifier: ", repr(ident))
                end
            end
        end
        for ident in bld
            if isa(ident,Int)
                ident >= 0 || error("invalid negative build identifier: $ident")
            else
                if !ismatch(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && length(bld)!=1
                    error("invalid build identifier: ", repr(ident))
                end
            end
        end
        new(major, minor, patch, pre, bld)
    end
end
VersionNumber(x::Integer, y::Integer, z::Integer) = VersionNumber(x, y, z, (), ())
VersionNumber(x::Integer, y::Integer)             = VersionNumber(x, y, 0, (), ())
VersionNumber(x::Integer)                         = VersionNumber(x, 0, 0, (), ())

function print(io::IO, v::VersionNumber)
    v == typemax(VersionNumber) && return print(io, "∞")
    print(io, v.major)
    print(io, '.')
    print(io, v.minor)
    print(io, '.')
    print(io, v.patch)
    if !isempty(v.prerelease)
        print(io, '-')
        print_joined(io, v.prerelease,'.')
    end
    if !isempty(v.build)
        print(io, '+')
        print_joined(io, v.build,'.')
    end
end
show(io::IO, v::VersionNumber) = print(io, "v\"", v, "\"")

convert(::Type{VersionNumber}, v::Integer) = VersionNumber(v)
convert(::Type{VersionNumber}, v::Tuple) = VersionNumber(v...)

const VERSION_REGEX = r"^
    v?                                      # prefix        (optional)
    (\d+)                                   # major         (required)
    (?:\.(\d+))?                            # minor         (optional)
    (?:\.(\d+))?                            # patch         (optional)
    (?:(-)|
    (?:-((?:[0-9a-z-]+\.)*[0-9a-z-]+))?     # pre-release   (optional)
    (?:(\+)|
    (?:\+((?:[0-9a-z-]+\.)*[0-9a-z-]+))?    # build         (optional)
    ))
$"ix

function split_idents(s::String)
    idents = split(s, '.')
    ntuple(length(idents)) do i
        ident = idents[i]
        ismatch(r"^\d+$", ident) ? parseint(ident) : bytestring(ident)
    end
end

function convert(::Type{VersionNumber}, v::String)
    m = match(VERSION_REGEX, v)
    if m == nothing error("invalid version string: $v") end
    major, minor, patch, minus, prerl, plus, build = m.captures
    major = int(major)
    minor = minor != nothing ? int(minor) : 0
    patch = patch != nothing ? int(patch) : 0
    prerl = prerl != nothing ? split_idents(prerl) : minus == "-" ? ("",) : ()
    build = build != nothing ? split_idents(build) : plus  == "+" ? ("",) : ()
    VersionNumber(major, minor, patch, prerl, build)
end

macro v_str(v); convert(VersionNumber, v); end

typemin(::Type{VersionNumber}) = v"0-"
typemax(::Type{VersionNumber}) = VersionNumber(typemax(Int),typemax(Int),typemax(Int),(),("",))

ident_cmp(a::Int, b::Int) = cmp(a,b)
ident_cmp(a::Int, b::ASCIIString) = isempty(b) ? +1 : -1
ident_cmp(a::ASCIIString, b::Int) = isempty(a) ? -1 : +1
ident_cmp(a::ASCIIString, b::ASCIIString) = cmp(a,b)

function ident_cmp(A::(Union(Int,ASCIIString)...),
                   B::(Union(Int,ASCIIString)...))
    i = start(A)
    j = start(B)
    while !done(A,i) && !done(B,i)
       a,i = next(A,i)
       b,j = next(B,j)
       c = ident_cmp(a,b)
       (c != 0) && return c
    end
    done(A,i) && !done(B,j) ? -1 :
    !done(A,i) && done(B,j) ? +1 : 0
end

function isequal(a::VersionNumber, b::VersionNumber)
    (a.major != b.major) && return false
    (a.minor != b.minor) && return false
    (a.patch != b.patch) && return false
    (ident_cmp(a.prerelease,b.prerelease) != 0) && return false
    (ident_cmp(a.build,b.build) != 0) && return false
    return true
end

issupbuild(v::VersionNumber) = length(v.build)==1 && isempty(v.build[1])

function isless(a::VersionNumber, b::VersionNumber)
    (a.major < b.major) && return true
    (a.major > b.major) && return false
    (a.minor < b.minor) && return true
    (a.minor > b.minor) && return false
    (a.patch < b.patch) && return true
    (a.patch > b.patch) && return false
    (!isempty(a.prerelease) && isempty(b.prerelease)) && return true
    (isempty(a.prerelease) && !isempty(b.prerelease)) && return false
    c = ident_cmp(a.prerelease,b.prerelease)
    (c < 0) && return true
    (c > 0) && return false
    (!issupbuild(a) && issupbuild(b)) && return true
    (issupbuild(a) && !issupbuild(b)) && return false
    c = ident_cmp(a.build,b.build)
    (c < 0) && return true
    return false
end

hash(v::VersionNumber) = hash([v.(n) for n in VersionNumber.names])

## julia version info

begin
# Include build number if we've got at least some distance from a tag (e.g. a release)
prerelease = BUILD_INFO.prerelease ? "-prerelease" : ""
build_number = BUILD_INFO.build_number != 0 ? "+$(BUILD_INFO.build_number)" : ""
try
    global const VERSION = convert( VersionNumber, "$(BUILD_INFO.version_string)$(prerelease)$(build_number)")
catch e
    println("while creating Base.VERSION, ignoring error $e")
    global const VERSION = VersionNumber(0)
end
branch_prefix = (BUILD_INFO.branch == "master") ? "" : "$(BUILD_INFO.branch)/"
dirty_suffix = BUILD_INFO.dirty ? "*" : ""
global const commit_string = (BUILD_INFO.commit == "") ? "" : "Commit $(branch_prefix)$(BUILD_INFO.commit_short)$(dirty_suffix) $(BUILD_INFO.date_string)"

const banner_plain =
"""
               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type "help()" to list help topics
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version $VERSION
 _/ |\\__'_|_|_|\\__'_|  |  "$commit_string"
|__/                   |  $(Sys.MACHINE)

"""
local tx = "\033[0m\033[1m" # text
local jl = "\033[0m\033[1m" # julia
local d1 = "\033[34m" # first dot
local d2 = "\033[31m" # second dot
local d3 = "\033[32m" # third dot
local d4 = "\033[35m" # fourth dot
const banner_color =
"\033[1m               $(d3)_
   $(d1)_       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |  A fresh approach to technical computing
  $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  Documentation: http://docs.julialang.org
   $(jl)_ _   _| |_  __ _$(tx)   |  Type \"help()\" to list help topics
  $(jl)| | | | | | |/ _` |$(tx)  |
  $(jl)| | |_| | | | (_| |$(tx)  |  Version $VERSION
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |  $commit_string
$(jl)|__/$(tx)                   |  $(Sys.MACHINE)

\033[0m"
end # begin
