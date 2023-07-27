# Insert LazyLibrary for `libpcre`, although at this time this gets lazily
# on startup because `include()` calls `isdirpath()` which has a regex in it.
using Base.JLLAdapters: libpcre2_8, libpcre2_8_name
using Base.Libc.Libdl: BundledLazyLibraryPath, LazyLibrary
libpcre2_8[] = LazyLibrary(BundledLazyLibraryPath(libpcre2_8_name))


# Insert LazyLibrary for `libgmp`
using Base.JLLAdapters: libgmp, libgmp_name
using Base.Libc.Libdl: BundledLazyLibraryPath, LazyLibrary
using Base.GMP: libgmp_init
libgmp[] = LazyLibrary(BundledLazyLibraryPath(libgmp_name); on_load_callback=libgmp_init)

# Insert LazyLibrary for `libmpfr`
using Base.JLLAdapters: libgmp, libmpfr, libmpfr_name
using Base.Libc.Libdl: BundledLazyLibraryPath, LazyLibrary
using Base.MPFR: libmpfr_init
libmpfr[] = LazyLibrary(BundledLazyLibraryPath(libmpfr_name); dependencies=[libgmp[]], on_load_callback=libmpfr_init)
