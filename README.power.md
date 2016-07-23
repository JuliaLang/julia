# Julia Binaries for Power8

Experimental support is now available for Julia on power8 little
endian architectures running linux - ppc64le.

[Precompiled
binaries](https://drive.google.com/drive/u/0/folders/0B0rXlkvSbIfhR1RsbUV2VkpFMFk)
for Julia 0.5-pre are available. Make sure to download `libtatlas.so`
from there and put it on your `LD_LIBRARY_PATH`.

The latest release of ATLAS on most linux distributions is buggy on
Power and using the provided binary is preferable. The next stable
release of ATLAS (3.10.3) should fix all known issues, but it may be a
while before an update is available on popular linux distros.

Note that these are unofficial binaries and contributed strictly for
convenience. They may or may not work for you, and have not gone
through the same rigour as official binaries.

# Building Julia on Power8

Since OpenBLAS power support is still in the works, we use ATLAS as a
system provided BLAS for now through a custom Make.user file:

````
override USE_SYSTEM_BLAS = 1
override USE_BLAS64 = 0
override LIBBLAS = -L/usr/lib64/atlas -ltatlas
override LIBBLASNAME = libtatlas
````
