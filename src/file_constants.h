#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
const JL_DUMMY = 0
const JL_O_WRONLY      = O_WRONLY
const JL_O_RDONLY      = O_RDONLY
const JL_O_RDWR        = O_RDWR
const JL_O_APPEND      = O_APPEND
const JL_O_CREAT       = O_CREAT
const JL_O_EXCL        = O_EXCL
const JL_O_TRUNC       = O_TRUNC
#ifdef O_TEMPORARY
const JL_O_TEMPORARY   = O_TEMPORARY
#endif
#ifdef O_SHORT_LIVED
const JL_O_SHORT_LIVED  = O_SHORT_LIVED
#endif
#ifdef O_SEQUENTIAL
const JL_O_SEQUENTIAL   = O_SEQUENTIAL
#endif
#ifdef O_RANDOM
const JL_O_RANDOM       = O_RANDOM
#endif
#ifdef O_NOCTTY
const JL_O_NOCTTY       = O_NOCTTY
#endif
