## semantic version numbers (http://semver.org)

type VersionNumber
    major::Uint16
    minor::Uint16
    patch::Uint16
    suffix::String

    function VersionNumber(major::Int, minor::Int, patch::Int, suffix::String)
        if major < 0; error("invalid major version: $major"); end
        if minor < 0; error("invalid minor version: $minor"); end
        if patch < 0; error("invalid patch version: $patch"); end
        if !matches(ri"^(?:[a-z-][0-9a-z-]*)?$", suffix)
            error("invalid version suffix: $suffix")
        end
        new(major, minor, patch, suffix)
    end
end
VersionNumber(x::Int, y::Int, s::String) = VersionNumber(x, y, 0, s )
VersionNumber(x::Int, s::String)         = VersionNumber(x, 0, 0, s )
VersionNumber(x::Int, y::Int, z::Int)    = VersionNumber(x, y, z, "")
VersionNumber(x::Int, y::Int)            = VersionNumber(x, y, 0, "")
VersionNumber(x::Int)                    = VersionNumber(x, 0, 0, "")

print(v::VersionNumber) = print("$(v.major).$(v.minor).$(v.patch)$(v.suffix)")
show(v::VersionNumber) = print("v\"", v, "\"")

convert(::Type{VersionNumber}, v::Int) = VersionNumber(v)
convert(::Type{VersionNumber}, v::Tuple) = VersionNumber(v...)

const VERSION_REGEX = ri"^v?(\d+)(?:\.(\d+)(?:\.(\d+))?)?((?:[a-z-][0-9a-z-]*)?)$"

function convert(::Type{VersionNumber}, v::String)
    m = match(VERSION_REGEX, v)
    if m == nothing; error("invalid version string: $v"); end
    major, minor, patch, suffix = m.captures
    major = parse_dec(major)
    minor = minor == nothing ? 0 : parse_dec(minor)
    patch = patch == nothing ? 0 : parse_dec(patch)
    VersionNumber(major, minor, patch, suffix)
end

macro v_str(v); convert(VersionNumber, v); end

<(a::VersionNumber, b::VersionNumber) =
    a.major < b.major || a.major == b.major &&
    (a.minor < b.minor || a.minor == b.minor &&
     (a.patch < b.patch || a.patch == b.patch &&
      (!isempty(a.suffix) && (isempty(b.suffix) || a.suffix < b.suffix))))

==(a::VersionNumber, b::VersionNumber) =
    a.major == b.major && a.minor == b.minor &&
    a.patch == b.patch && a.suffix == b.suffix

<(a::VersionNumber, b) = a < convert(VersionNumber,b)
<(a, b::VersionNumber) = convert(VersionNumber,a) < b
==(a::VersionNumber, b) = a == convert(VersionNumber,b)
==(a, b::VersionNumber) = convert(VersionNumber,a) == b

## julia version info

const VERSION = convert(VersionNumber,readall(`cat $JULIA_HOME/VERSION`)[1:end-1])
const VERSION_COMMIT = readall(`git rev-parse HEAD`)[1:end-1]
const VERSION_CLEAN = success(`git diff --quiet`)
const VERSION_TIME = readall(
    `git log -1 --pretty=format:%ct` |
    `perl -MPOSIX=strftime -e 'print strftime "%F %T", gmtime <>'`
)

begin

const jl_version_string = "Version $VERSION"
local jl_version_clean = VERSION_CLEAN ? "" : "*"
const jl_commit_string = "Commit $(VERSION_COMMIT[1:10]) ($VERSION_TIME)$jl_version_clean"

const jl_banner_plain =
I"               _
   _       _ _(_)_     |
  (_)     | (_) (_)    |  A fresh approach to technical computing
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |  $jl_version_string
  | | |_| | | | (_| |  |  $jl_commit_string
 _/ |\__'_|_|_|\__'_|  |
|__/                   |

"

local tx = "\033[0m\033[1m" # text
local jl = "\033[0m\033[1m" # julia
local d1 = "\033[34m" # first dot
local d2 = "\033[31m" # second dot
local d3 = "\033[32m" # third dot
local d4 = "\033[35m" # fourth dot
const jl_banner_color =
"\033[1m               $(d3)_
   $(d1)_       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |
  $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  A fresh approach to technical computing
   $(jl)_ _   _| |_  __ _$(tx)   |
  $(jl)| | | | | | |/ _` |$(tx)  |  $jl_version_string
  $(jl)| | |_| | | | (_| |$(tx)  |  $jl_commit_string
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |
$(jl)|__/$(tx)                   |

\033[0m"

color_available() =
    success(`tput setaf 0`) || has(ENV, "TERM") && matches(r"^xterm", ENV["TERM"])

banner() = print(color_available() ? jl_banner_color : jl_banner_plain)

end # begin
