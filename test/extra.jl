runtests("suitesparse")
runtests("arpack")
runtests("bigfloat")
runtests("file")
runtests("zlib")
# runtests("options")
runtests("image")
# runtests("iterators")
@unix_only runtests("gzip")

runtests("perf")
