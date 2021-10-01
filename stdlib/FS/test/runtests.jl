# This file is a part of Julia. License is MIT: https://julialang.org/license

using FS
using Test

# just test that we can import this, since the tests are generally already part
# of Base's suite
@test FS === Base.Filesystem
