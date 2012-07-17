##################################
# Cross Plaform tests for spawn. #
##################################

# Assumes the following are available in the
#  - GNU coreutils (or equivalent)
#  - perl

#TODO:
# - Windows:
#   - Add a test whether coreutils are available and skip tests if not

#### Examples used in the manual ####

show(readall(`echo hello | sort`))
@assert readall(`echo hello | sort`) == "hello | sort\n"
@assert readall(`echo hello`|`sort`) == "hello\n"
out = readall(`echo hello` & `echo world`)
@assert search(out,"world") != (0,0)
@assert search(out,"hello") != (0,0)
@assert readall((`echo hello` & `echo world`)|`sort`)=="hello\nworld\n"

## THIS requires visual inspection
@assert run(`echo stdio passthrough OK`)

prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`
@assert run(`perl -le '$|=1; for(0..9){ print; sleep 1 }'` | prefixer("A",2) & prefixer("B",2))

@assert run(`perl -le '$|=1; for(0..9){ print; sleep 1 }'` |
    prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3) |
    prefixer("A",2) & prefixer("B",2))

@assert_fails run(`false`)
@assert run(ignorestatus(`false`))
@assert run(ignorestatus(`false`)|`true`)
@assert_fails run(ignorestatus(`false`)|`false`)
@assert_fails run(ignorestatus(`false`)&`false`)
@assert run(ignorestatus(`false`|`false`))
@assert run(ignorestatus(`false`&`false`))
