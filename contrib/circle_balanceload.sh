#!/bin/bash
# Balance the testing load between 2 CircleCI parallel containers

tests=($(/tmp/julia/bin/julia -e 'include("choosetests.jl"); t = choosetests(["all"])[1]; print(join(t, " "))'))

# Command to run tests -- append names for specific tests
jlcmd="/tmp/julia/bin/julia --check-bounds=yes runtests.jl"

# Cut my build into pieces, this is my last resort
cutoff=29

case $CIRCLE_NODE_INDEX in
  0) $jlcmd ${tests[@]:0:${cutoff}} libgit2-online ;;
  1) $jlcmd ${tests[@]:${cutoff}+1} pkg ;;
esac
