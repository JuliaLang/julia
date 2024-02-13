// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
const JL_O_WRONLY      = O_WRONLY
const JL_O_RDONLY      = O_RDONLY
const JL_O_RDWR        = O_RDWR
const JL_O_APPEND      = O_APPEND
const JL_O_CREAT       = O_CREAT
const JL_O_EXCL        = O_EXCL
const JL_O_TRUNC       = O_TRUNC
#ifdef O_TEMPORARY
const JL_O_TEMPORARY = O_TEMPORARY
#else
const JL_O_TEMPORARY = 0x0000
#endif
#ifdef O_SHORT_LIVED
const JL_O_SHORT_LIVED = O_SHORT_LIVED
#else
const JL_O_SHORT_LIVED = 0x0000
#endif
#ifdef O_SEQUENTIAL
const JL_O_SEQUENTIAL = O_SEQUENTIAL
#else
const JL_O_SEQUENTIAL = 0x0000
#endif
#ifdef O_RANDOM
const JL_O_RANDOM = O_RANDOM
#else
const JL_O_RANDOM = 0x0000
#endif
#ifdef O_NOCTTY
const JL_O_NOCTTY = O_NOCTTY
#else
const JL_O_NOCTTY = 0x0000
#endif
#ifdef O_NONBLOCK
const JL_O_NONBLOCK = O_NONBLOCK
#else
const JL_O_NONBLOCK = 0x0000
#endif
#ifdef O_NDELAY
const JL_O_NDELAY = O_NDELAY
#else
const JL_O_NDELAY = 0x0000
#endif
#ifdef O_SYNC
const JL_O_SYNC = O_SYNC
#else
const JL_O_SYNC = 0x0000
#endif
#ifdef O_FSYNC
const JL_O_FSYNC = O_FSYNC
#else
const JL_O_FSYNC = 0x0000
#endif
#ifdef O_ASYNC
const JL_O_ASYNC = O_ASYNC
#else
const JL_O_ASYNC = 0x0000
#endif
#ifdef O_LARGEFILE
const JL_O_LARGEFILE = O_LARGEFILE
#else
const JL_O_LARGEFILE = 0x0000
#endif
#ifdef O_DIRECTORY
const JL_O_DIRECTORY = O_DIRECTORY
#else
const JL_O_DIRECTORY = 0x0000
#endif
#ifdef O_NOFOLLOW
const JL_O_NOFOLLOW = O_NOFOLLOW
#else
const JL_O_NOFOLLOW = 0x0000
#endif
#ifdef O_CLOEXEC
const JL_O_CLOEXEC = O_CLOEXEC
#else
const JL_O_CLOEXEC = 0x0000
#endif
#ifdef O_DIRECT
const JL_O_DIRECT = O_DIRECT
#else
const JL_O_DIRECT = 0x0000
#endif
#ifdef O_NOATIME
const JL_O_NOATIME = O_NOATIME
#else
const JL_O_NOATIME = 0x0000
#endif
#ifdef O_PATH
const JL_O_PATH = O_PATH
#else
const JL_O_PATH = 0x0000
#endif
#ifdef O_TMPFILE
const JL_O_TMPFILE = O_TMPFILE
#else
const JL_O_TMPFILE = 0x0000
#endif
#ifdef O_DSYNC
const JL_O_DSYNC = O_DSYNC
#else
const JL_O_DSYNC = 0x0000
#endif
#ifdef O_RSYNC
const JL_O_RSYNC = O_RSYNC
#else
const JL_O_RSYNC = 0x0000
#endif
