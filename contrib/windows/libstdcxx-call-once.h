// Make libstdc++'s headers compile std::call_once for the libstdc++ that Julia
// ships on Windows, BinaryBuilder's (from CompilerSupportLibraries_jll), when
// the headers themselves come from a differently configured toolchain such as
// MSYS2's. Pass it with -include, so that it runs before any other header.
//
// BinaryBuilder's libstdc++ is configured without thread-local storage, and
// implements std::call_once through a global functor and mutex
// (std::__once_functor, std::__get_once_mutex(), ...). MSYS2's is configured
// with TLS, and its headers instead call accessors for thread-local state
// (std::__get_once_callable() and std::__get_once_call() since GCC 16), which
// BinaryBuilder's libstdc++ does not export. Nor does MSYS2's libstdc++ export
// the former, so shipping it instead would break the BinaryBuilder-built C++
// libraries that packages load. _GLIBCXX_HAVE_TLS only selects between these
// two implementations in <mutex>.
#if defined(__cplusplus) && defined(__has_include)
#if __has_include(<bits/c++config.h>)
#include <bits/c++config.h>
#undef _GLIBCXX_HAVE_TLS
#endif
#endif
