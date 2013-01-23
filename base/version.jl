## semantic version numbers (http://semver.org)

type VersionNumber
    major::Int
    minor::Int
    patch::Int
    prerelease::Vector{Union(Int,ASCIIString)}
    build::Vector{Union(Int,ASCIIString)}

    function VersionNumber(major::Int, minor::Int, patch::Int, pre::Vector, bld::Vector)
        if major < 0 error("invalid major version: $major") end
        if minor < 0 error("invalid minor version: $minor") end
        if patch < 0 error("invalid patch version: $patch") end
        prerelease = Array(Union(Int,ASCIIString),length(pre))
        for i in 1:length(pre)
            ident = ascii(string(pre[i]))
            if isempty(ident) && !(length(pre)==1 && isempty(bld))
                error("invalid pre-release identifier: empty string")
            end
            if !ismatch(r"^(?:[0-9a-z-]*)?$"i, ident)
                error("invalid pre-release identifier: $ident")
            end
            if ismatch(r"^\d+$", ident)
                ident = parse_int(ident)
            end
            prerelease[i] = ident
        end
        build = Array(Union(Int,ASCIIString),length(bld))
        for i in 1:length(bld)
            ident = ascii(string(bld[i]))
            if !ismatch(r"^(?:[0-9a-z-]+)?$"i, ident)
                error("invalid build identifier: $ident")
            end
            if ismatch(r"^\d+$", ident)
                ident = parse_int(ident)
            end
            build[i] = ident
        end
        new(major, minor, patch, prerelease, build)
    end
end
VersionNumber(x::Integer, y::Integer, z::Integer) = VersionNumber(x, y, z, [], [])
VersionNumber(x::Integer, y::Integer)             = VersionNumber(x, y, 0, [], [])
VersionNumber(x::Integer)                         = VersionNumber(x, 0, 0, [], [])

function print(io::IO, v::VersionNumber)
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
    (?:\+((?:[0-9a-z-]+\.)*[0-9a-z-]+))?    # build         (optional)
    )
$"ix

function convert(::Type{VersionNumber}, v::String)
    m = match(VERSION_REGEX, v)
    if m == nothing error("invalid version string: $v") end
    major, minor, patch, minus, prerl, build = m.captures
    major = int(major)
    minor = minor != nothing ? int(minor) : 0
    patch = patch != nothing ? int(patch) : 0
    prerl = prerl != nothing ? split(prerl,'.') : minus != nothing ? [""] : []
    build = build != nothing ? split(build,'.') : []
    VersionNumber(major, minor, patch, prerl, build)
end

macro v_str(v); convert(VersionNumber, v); end

ident_cmp(a::Int, b::Int) = cmp(a,b)
ident_cmp(a::Int, b::ASCIIString) = isempty(b) ? +1 : -1
ident_cmp(a::ASCIIString, b::Int) = isempty(a) ? -1 : +1
ident_cmp(a::ASCIIString, b::ASCIIString) = cmp(a,b)

function ident_cmp(A::Vector{Union(Int,ASCIIString)},
                       B::Vector{Union(Int,ASCIIString)})
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
    c = ident_cmp(a.build,b.build)
    (c < 0) && return true
    return false
end

hash(v::VersionNumber) = hash([v.(n) for n in VersionNumber.names])

## julia version info

if(isfile("$JULIA_HOME/../../VERSION"))
const VERSION = convert(VersionNumber,readchomp("$JULIA_HOME/../../VERSION"))
elseif(isfile("$JULIA_HOME/../share/julia/VERSION"))
	const VERSION = convert(VersionNumber,readchomp("$JULIA_HOME/../share/julia/VERSION"))
else
	const VERSION = convert(VersionNumber,"0.0.0")
end
if(isfile("$JULIA_HOME/../../COMMIT"))
    const VERSION_COMMIT = ""
    const commit_string = readchomp("$JULIA_HOME/../../COMMIT")
elseif(isfile("$JULIA_HOME/../share/julia/COMMIT"))
    const VERSION_COMMIT = ""
    const commit_string = readchomp("$JULIA_HOME/../share/julia/COMMIT")
else

let
    expected = ErrorException("error: don't copy this code, for breaking out of uv_run during boot-strapping only")
    acceptable = ErrorException(expected.msg) # we would like to update the error msg for this later, but at
                                              # this point in the bootstrapping, conflicts between old and new
                                              # defintions for write, TTY, ASCIIString, and STDOUT make it fail
    ver = string(VERSION)
    (outver,ps) = read_from(`git rev-parse HEAD`)
    ps.closecb = function(proc)
        if !success(proc)
            #acceptable.msg = "failed process: $proc [$(proc.exit_code)]"
            error(acceptable)
        end
        commit = readchomp(proc.out.buffer)
        (outtag,ps) = read_from(`git rev-parse --verify --quiet v$ver`)
        ps.closecb = function(proc)
            tagged = if success(proc)
                readchomp(proc.out.buffer)
            elseif proc.exit_code == 1 && proc.term_signal == 0
                "doesn't reference a commit"
            else
                #acceptable.msg = "failed process: $proc [$(proc.exit_code)]"
                error(acceptable)
            end
            tagged = success(proc) ? readchomp(proc.out.buffer) : "doesn't reference a commit"
            (outctim,ps) = read_from(`git log -1 --pretty=format:%ct`)
            ps.closecb = function(proc)
                if !success(proc)
                    #acceptable.msg = string("failed process: ",proc," [",proc.exit_code,"]")
                    error(acceptable)
                end
                ctim = int(readall(proc.out.buffer))
                if commit != tagged
                    # 1250998746: ctime of first commit (Sat Aug 23 3:39:06 2009 UTC)
                    push!(VERSION.build, ctim - 1250998746)
                    push!(VERSION.build, "r$(commit[1:4])")
                end
                ps = spawn(`git diff --quiet HEAD`)
                ps.closecb = function(proc)
                    clean = if success(proc)
                        ""
                    elseif proc.exit_code == 1 && proc.term_signal == 0
                        push!(VERSION.build, "dirty")
                        "*" 
                    else
                        #acceptable.msg = string("failed process: ",proc," [",proc.exit_code,"]")
                        error(acceptable)
                    end
                    isotime = strftime("%Y-%m-%d %H:%M:%S", ctim)
                    global const commit_string = "Commit $(commit[1:10]) ($isotime)$clean"
                    global const VERSION_COMMIT = commit[1:10]
                    error(expected)
                end
            end
        end
    end
    try
        run_event_loop() # equivalent to wait_exit() on a more sane version of the previous
                         # block of code, but Scheduler doesn't exist during bootstrapping
                         # so we do what we must, but don't do this in user-land code or you'll regret it
    catch err
        if err != expected
            global const commit_string = ""
            global const VERSION_COMMIT = ""
            if err == acceptable
                println("Warning: git failed in version.jl")
                #println(err) # not a useful error msg currently
            else
                rethrow(err)
            end
        end
    end
end
end
begin
const version_string = "Version $VERSION"
const banner_plain =
I"               _
   _       _ _(_)_     |  A fresh approach to technical computing
  (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
   _ _   _| |_  __ _   |  Type \"help()\" to list help topics
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  $version_string
 _/ |\__'_|_|_|\__'_|  |  $commit_string
|__/                   |

"
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
  $(jl)| | |_| | | | (_| |$(tx)  |  $version_string
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |  $commit_string
$(jl)|__/$(tx)                   |

\033[0m"
end # begin
