// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_DIRPATH_H
#define JL_DIRPATH_H

#ifdef _OS_WINDOWS_
#define PATHSEPSTRING "\\"
#define PATHLISTSEPSTRING ";"
#if defined(PATH_MAX)
#define JL_PATH_MAX PATH_MAX
#else // _COMPILER_CLANG_ may have the name reversed
#define JL_PATH_MAX MAX_PATH
#endif
#else
#define PATHSEPSTRING "/"
#define PATHLISTSEPSTRING ":"
#if defined(PATH_MAX)
#define JL_PATH_MAX PATH_MAX
#else // many platforms don't have a max path, we define one anyways
#define JL_PATH_MAX 1024
#endif
#endif

#endif
