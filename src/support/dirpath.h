// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_DIRPATH_H
#define JL_DIRPATH_H

#ifdef _OS_WINDOWS_
#define PATHSEPSTRING "\\"
#define PATHLISTSEPSTRING ";"
#define JL_PATH_MAX PATH_MAX
#if defined(_COMPILER_CLANG_)
#define JL_PATH_MAX MAX_PATH
#endif
#else
#define PATHSEPSTRING "/"
#define PATHLISTSEPSTRING ":"
#ifndef JL_PATH_MAX // many platforms don't have a max path, we define one anyways
#define JL_PATH_MAX 1024
#endif
#endif

#endif
