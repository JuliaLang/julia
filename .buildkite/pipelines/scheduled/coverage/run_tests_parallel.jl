# Important note: even if one or more tests fail, we will still exit with status code 0.
#
# The reason for this is that we always want to upload code coverage, even if some of the
# tests fail. Therefore, even if the `coverage_linux64` builder passes, you should not
# assume that all of the tests passed. If you want to know if all of the tests are passing,
# please look at the status of the `tester_*` builders (e.g. `tester_linux64`).

const ncores = Sys.CPU_THREADS
@info "" Sys.CPU_THREADS
@info "" ncores

script_native_yes = """
    Base.runtests(["cmdlineargs"]; ncores = $(ncores))
"""
script_native_no = """
    Base.runtests(["all", "--skip", "cmdlineargs"]; ncores = $(ncores))
"""

base_cmd       = `$(Base.julia_cmd()) --code-coverage=all`
cmd_native_yes = `$(base_cmd) --sysimage-native-code=yes -e $(script_native_yes)`
cmd_native_no  = `$(base_cmd) --sysimage-native-code=no  -e $(script_native_no)`

@info "Running command" cmd_native_yes
p1 = run(pipeline(cmd_native_yes; stdin, stdout, stderr); wait = false)
wait(p1)

@info "Running command" cmd_native_no
p2 = run(pipeline(cmd_native_no; stdin, stdout, stderr); wait = false)
wait(p2)
