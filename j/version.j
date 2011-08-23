VERSION_STRING = readall(`cat $JULIA_HOME/VERSION`)[1:end-1]
VERSION_COMMIT = readall(`git rev-parse HEAD`)[1:end-1]
VERSION_CLEAN = run(`git diff --quiet`)
VERSION_TIME = readall(
    `git log -1 --pretty=format:%ct` |
    `perl -MPOSIX=strftime -e 'print strftime "%F %T", gmtime <>'`
)

jl_version_string = "Version $VERSION_STRING"
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
