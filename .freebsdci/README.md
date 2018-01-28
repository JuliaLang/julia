# FreeBSD CI Build Scripts

The flow of a [FreeBSD CI](https://freebsdci.julialang.org) build:

1. `compile.sh`
2. `build-state.sh`
3. `runtests.sh`
4. `test-embedding.sh`

Detail of flow is control by the variable `factory`
[here](https://github.com/iblis17/julia-fbsd-buildbot/blob/master/master/master.cfg).
