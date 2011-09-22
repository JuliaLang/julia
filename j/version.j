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
        # TODO: use compile-time regex, pending bugfix
        if !matches(Regex(L"^(?:[a-z-][0-9a-z-]*)?$"), suffix)
            error("invalid version suffix: $suffix")
        end
        new(major, minor, patch, suffix)
    end
end
VersionNumber(major, minor, suffix::String) = VersionNumber(major, minor, 0,     suffix)
VersionNumber(major, suffix::String)        = VersionNumber(major, 0,     0,     suffix)
VersionNumber(major, minor, patch)          = VersionNumber(major, minor, patch, "")
VersionNumber(major, minor)                 = VersionNumber(major, minor, 0,     "")
VersionNumber(major)                        = VersionNumber(major, 0,     0,     "")

show(v::VersionNumber) = print("$(v.major).$(v.minor).$(v.patch)$(v.suffix)")

convert(::Type{VersionNumber}, v::Int) = VersionNumber(v)
convert(::Type{VersionNumber}, v::Tuple) = VersionNumber(v...)

function convert(::Type{VersionNumber}, v::String)
    # TODO: move outside of function when #187 is fixed
    VERSION_REGEX = Regex(L"^v?(\d+)(?:\.(\d+)(?:\.(\d+))?)?((?:[a-z-][0-9a-z-]*)?)$")
    m = match(VERSION_REGEX, v)
    if m == nothing; error("invalid version string: $v"); end
    major, minor, patch, suffix = m.captures
    major = parse_dec(major)
    minor = minor == nothing ? 0 : parse_dec(minor)
    patch = patch == nothing ? 0 : parse_dec(patch)
    VersionNumber(major, minor, patch, suffix)
end

<(a::VersionNumber, b::VersionNumber) =
    a.major < b.major || a.major == b.major &&
    (a.minor < b.minor || a.minor == b.minor &&
     (a.patch < b.patch || a.patch == b.patch && a.suffix < b.suffix))

==(a::VersionNumber, b::VersionNumber) =
    a.major == b.major && a.minor == b.minor &&
    a.patch == b.patch && a.suffix == b.suffix

<(a::VersionNumber, b) = a < convert(VersionNumber,b)
<(a, b::VersionNumber) = convert(VersionNumber,a) < b
==(a::VersionNumber, b) = a == convert(VersionNumber,b)
==(a, b::VersionNumber) = convert(VersionNumber,a) == b

## julia version info

VERSION = convert(VersionNumber,readall(`cat $JULIA_HOME/VERSION`)[1:end-1])
VERSION_COMMIT = readall(`git rev-parse HEAD`)[1:end-1]
VERSION_CLEAN = run(`git diff --quiet`)
VERSION_TIME = readall(
    `git log -1 --pretty=format:%ct` |
    `perl -MPOSIX=strftime -e 'print strftime "%F %T", gmtime <>'`
)

jl_version_string = "Version $VERSION"
jl_version_clean = VERSION_CLEAN ? "" : "*"
jl_commit_string = "Commit $(VERSION_COMMIT[1:10]) ($VERSION_TIME)$jl_version_clean"

jl_banner_plain =
I"               _
   _       _ _(_)_     |
  (_)     | (_) (_)    |  A fresh approach to technical computing
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |  $jl_version_string
  | | |_| | | | (_| |  |  $jl_commit_string
 _/ |\__'_|_|_|\__'_|  |
|__/                   |

"

begin
local tx = "\033[0m\033[1m" # text
local jl = "\033[0m\033[1m" # julia
local d1 = "\033[34m" # first dot
local d2 = "\033[31m" # second dot
local d3 = "\033[32m" # third dot
local d4 = "\033[35m" # fourth dot
jl_banner_color =
"\033[1m               $(d3)_
   $(d1)_       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |
  $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  A fresh approach to technical computing
   $(jl)_ _   _| |_  __ _$(tx)   |
  $(jl)| | | | | | |/ _` |$(tx)  |  $jl_version_string
  $(jl)| | |_| | | | (_| |$(tx)  |  $jl_commit_string
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |
$(jl)|__/$(tx)                   |

\033[0m"
end

function color_available()
    if run(`tput setaf 0`)
        return true
    end
    if has(ENV, "TERM")
        term = ENV["TERM"]
        return term=="xterm" || term=="xterm-color"
    end
    false
end

function banner()
    if color_available()
        print(jl_banner_color)
    else
        print(jl_banner_plain)
    end
end
