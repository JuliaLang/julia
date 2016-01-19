// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef DIRPATH_H
#define DIRPATH_H

#ifdef _OS_WINDOWS_
#define PATHSEPSTRING "\\"
#define PATHLISTSEPSTRING ";"
#if defined(_MSC_VER) || defined(_COMPILER_CLANG_)
#define PATH_MAX MAX_PATH
#endif
#else
#define PATHSEPSTRING "/"
#define PATHLISTSEPSTRING ":"
#ifndef PATH_MAX // many platforms don't have a max path, we define one anyways
#define PATH_MAX 1024
#endif
#endif

#endif
