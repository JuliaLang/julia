## semantic version numbers (http://semver.org)

immutable VersionNumber
    major::Int
    minor::Int
    patch::Int
    prerelease::Tuple{Union(Int,ASCIIString), ...}
    build::Tuple{Union(Int,ASCIIString), ...}

    function VersionNumber(major::Integer, minor::Integer, patch::Integer, pre::Tuple{Union(Int,ASCIIString), ...}, bld::Tuple{Union(Int,ASCIIString), ...})
        major >= 0 || throw(ArgumentError("invalid negative major version: $major"))
        minor >= 0 || throw(ArgumentError("invalid negative minor version: $minor"))
        patch >= 0 || throw(ArgumentError("invalid negative patch version: $patch"))
        for ident in pre
            if isa(ident,Int)
                ident >= 0 || throw(ArgumentError("invalid negative pre-release identifier: $ident"))
            else
                if !ismatch(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && !(length(pre)==1 && isempty(bld))
                    throw(ArgumentError("invalid pre-release identifier: $(repr(ident))"))
                end
            end
        end
        for ident in bld
            if isa(ident,Int)
                ident >= 0 || throw(ArgumentError("invalid negative build identifier: $ident"))
            else
                if !ismatch(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && length(bld)!=1
                    throw(ArgumentError("invalid build identifier: $(repr(ident))"))
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
    (?:(-)|                                 # pre-release   (optional)
    ([a-z][0-9a-z-]*(?:\.[0-9a-z-]+)*|-(?:[0-9a-z-]+\.)*[0-9a-z-]+)?
    (?:(\+)|
    (?:\+((?:[0-9a-z-]+\.)*[0-9a-z-]+))?    # build         (optional)
    ))
$"ix

function split_idents(s::AbstractString)
    idents = split(s, '.')
    ntuple(length(idents)) do i
        ident = idents[i]
        ismatch(r"^\d+$", ident) ? parse(Int, ident) : bytestring(ident)
    end
end

VersionNumber(v::AbstractString) = begin
    m = match(VERSION_REGEX, v)
    m == nothing && throw(ArgumentError("invalid version string: $v"))
    major, minor, patch, minus, prerl, plus, build = m.captures
    major = parse(Int, major)
    minor = minor != nothing ? parse(Int, minor) : 0
    patch = patch != nothing ? parse(Int, patch) : 0
    if prerl != nothing && !isempty(prerl) && prerl[1] == '-'
        prerl = prerl[2:end] # strip leading '-'
    end
    prerl = prerl != nothing ? split_idents(prerl) : minus == "-" ? ("",) : ()
    build = build != nothing ? split_idents(build) : plus  == "+" ? ("",) : ()
    VersionNumber(major, minor, patch, prerl, build)
end

convert(::Type{VersionNumber}, v::AbstractString) = VersionNumber(v)

macro v_str(v); VersionNumber(v); end

typemin(::Type{VersionNumber}) = v"0-"
typemax(::Type{VersionNumber}) = VersionNumber(typemax(Int),typemax(Int),typemax(Int),(),("",))

ident_cmp(a::Int, b::Int) = cmp(a,b)
ident_cmp(a::Int, b::ASCIIString) = isempty(b) ? +1 : -1
ident_cmp(a::ASCIIString, b::Int) = isempty(a) ? -1 : +1
ident_cmp(a::ASCIIString, b::ASCIIString) = cmp(a,b)

function ident_cmp(A::Tuple{Union(Int,ASCIIString), ...},
                   B::Tuple{Union(Int,ASCIIString), ...})
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

function ==(a::VersionNumber, b::VersionNumber)
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

function hash(v::VersionNumber, h::UInt)
    h += 0x8ff4ffdb75f9fede % UInt
    h = hash(v.major, h)
    h = hash(v.minor, h)
    h = hash(v.patch, h)
    h = hash(v.prerelease, ~h)
    h = hash(v.build, ~h)
end

lowerbound(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch, ("",), ())
upperbound(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch, (), ("",))

thispatch(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch)
thisminor(v::VersionNumber) = VersionNumber(v.major, v.minor, 0)
thismajor(v::VersionNumber) = VersionNumber(v.major, 0, 0)

nextpatch(v::VersionNumber) = v < thispatch(v) ? thispatch(v) : VersionNumber(v.major, v.minor, v.patch+1)
nextminor(v::VersionNumber) = v < thisminor(v) ? thisminor(v) : VersionNumber(v.major, v.minor+1, 0)
nextmajor(v::VersionNumber) = v < thismajor(v) ? thismajor(v) : VersionNumber(v.major+1, 0, 0)

function check_new_version(existing::Vector{VersionNumber}, ver::VersionNumber)
    @assert issorted(existing)
    if isempty(existing)
        for v in [v"0", v"0.0.1", v"0.1", v"1"]
            lowerbound(v) <= ver <= v && return
        end
        error("$ver is not a valid initial version (try 0.0.0, 0.0.1, 0.1 or 1.0)")
    end
    idx = searchsortedlast(existing,ver)
    prv = existing[idx]
    ver == prv && error("version $ver already exists")
    nxt = thismajor(ver) != thismajor(prv) ? nextmajor(prv) :
          thisminor(ver) != thisminor(prv) ? nextminor(prv) : nextpatch(prv)
    ver <= nxt || error("$ver skips over $nxt")
    thispatch(ver) <= ver && return # regular or build release
    idx < length(existing) && thispatch(existing[idx+1]) <= nxt &&
        error("$ver is a pre-release of existing version $(existing[idx+1])")
    return # acceptable new version
end

## julia version info

# Include build number if we've got at least some distance from a tag (e.g. a release)
try
    build_number = GIT_VERSION_INFO.build_number != 0 ? "+$(GIT_VERSION_INFO.build_number)" : ""
    global const VERSION = convert(VersionNumber, "$(VERSION_STRING)$(build_number)")
catch e
    println("while creating Base.VERSION, ignoring error $e")
    global const VERSION = VersionNumber(0)
end

function banner(io::IO = STDOUT)
    if GIT_VERSION_INFO.tagged_commit
        commit_string = TAGGED_RELEASE_BANNER
    elseif GIT_VERSION_INFO.commit == ""
        commit_string = ""
    else
        days = Int(floor((ccall(:clock_now, Float64, ()) - GIT_VERSION_INFO.fork_master_timestamp) / (60 * 60 * 24)))
        unit = days == 1 ? "day" : "days"
        distance = GIT_VERSION_INFO.fork_master_distance
        commit = GIT_VERSION_INFO.commit_short

        if distance == 0
            commit_string = "Commit $(commit) ($(days) $(unit) old master)"
        else
            branch = GIT_VERSION_INFO.branch
            commit_string = "$(branch)/$(commit) (fork: $(distance) commits, $(days) $(unit))"
        end
    end
    commit_date = GIT_VERSION_INFO.date_string != "" ? " ($(GIT_VERSION_INFO.date_string))": ""

    if have_color
        tx = "\033[0m\033[1m" # text
        jl = "\033[0m\033[1m" # julia
        d1 = "\033[34m" # first dot
        d2 = "\033[31m" # second dot
        d3 = "\033[32m" # third dot
        d4 = "\033[35m" # fourth dot

        print(io,"""\033[1m               $(d3)_$(tx)
           $(d1)_$(tx)       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |  A fresh approach to technical computing
          $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  Documentation: http://docs.julialang.org
           $(jl)_ _   _| |_  __ _$(tx)   |  Type \"help()\" for help.
          $(jl)| | | | | | |/ _` |$(tx)  |
          $(jl)| | |_| | | | (_| |$(tx)  |  Version $(VERSION)$(commit_date)
         $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |  $(commit_string)
        $(jl)|__/$(tx)                   |  $(Sys.MACHINE)

        \033[0m""")
    else
        print(io,"""
                       _
           _       _ _(_)_     |  A fresh approach to technical computing
          (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
           _ _   _| |_  __ _   |  Type \"help()\" for help.
          | | | | | | |/ _` |  |
          | | |_| | | | (_| |  |  Version $(VERSION)$(commit_date)
         _/ |\\__'_|_|_|\\__'_|  |  $(commit_string)
        |__/                   |  $(Sys.MACHINE)

        """)
    end
end
