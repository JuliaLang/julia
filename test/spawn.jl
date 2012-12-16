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
@test readall(`echo hello | sort`) == "hello | sort\n"
@test readall(`echo hello`|`sort`) == "hello\n"
out = readall(`echo hello` & `echo world`)
@test search(out,"world") != (0,0)
@test search(out,"hello") != (0,0)
@test readall((`echo hello` & `echo world`)|`sort`)=="hello\nworld\n"

## THIS requires visual inspection
@test run(`echo stdio passthrough OK`)

prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`
@test run(`perl -le '$|=1; for(0..9){ print; sleep 1 }'` | prefixer("A",2) & prefixer("B",2))

@test run(`perl -le '$|=1; for(0..9){ print; sleep 1 }'` |
    prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3) |
    prefixer("A",2) & prefixer("B",2))

@test_fails run(`false`)
@test run(ignorestatus(`false`))
@test run(ignorestatus(`false`)|`true`)
@test_fails run(ignorestatus(`false`)|`false`)
@test_fails run(ignorestatus(`false`)&`false`)
@test run(ignorestatus(`false`|`false`))
@test run(ignorestatus(`false`&`false`))
