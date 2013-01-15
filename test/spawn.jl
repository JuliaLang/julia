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

@test readall(`echo hello | sort`) == "hello | sort\n"
@test readall(`echo hello`|`sort`) == "hello\n"

out = readall(`echo hello` & `echo world`)
@test search(out,"world") != (0,0)
@test search(out,"hello") != (0,0)
@test readall((`echo hello` & `echo world`)|`sort`)=="hello\nworld\n"

if false
    @test (run(`echo hello`); true)

    prefixer(prefix, sleep) = `perl -nle '$|=1; print "'$prefix' ", $_; sleep '$sleep';'`
    @test success(`perl -le '$|=1; for(0..2){ print; sleep 1 }'` |
                  prefixer("A",2) & prefixer("B",2))
    @test success(`perl -le '$|=1; for(0..2){ print; sleep 1 }'` |
                  prefixer("X",3) & prefixer("Y",3) & prefixer("Z",3) |
                  prefixer("A",2) & prefixer("B",2))
end

@test  success(`true`)
@test !success(`false`)
if false
    @test  success(ignorestatus(`false`))
    @test  success(ignorestatus(`false`) | `true`)
    @test !success(ignorestatus(`false`) | `false`)
    @test !success(ignorestatus(`false`) & `false`)
    @test  success(ignorestatus(`false` | `false`))
    @test  success(ignorestatus(`false` & `false`))
end
