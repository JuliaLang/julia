#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "dtypes.h"

#ifdef WIN32
#include <malloc.h>
#include <sys/timeb.h>
#include <windows.h>
#undef NO_ERROR
#undef MOD_SHIFT
#undef TRUE
#undef FALSE
#undef VOID
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

#include "dirpath.h"

void get_cwd(char *buf, size_t size)
{
#ifndef WIN32
    getcwd(buf, size);
#else
    GetCurrentDirectory(size, buf);
#endif
}

int set_cwd(char *buf)
{
#ifndef WIN32
    if (chdir(buf) == -1)
        return 1;
#else
    if (SetCurrentDirectory(buf) == 0)
        return 1;
#endif
    return 0;
}

// destructively convert path to directory part
void path_to_dirname(char *path)
{
    char *sep = strrchr(path, PATHSEP);
    if (sep != NULL) {
        *sep = '\0';
    }
    else {
        path[0] = '\0';
    }
}

#ifdef LINUX
char *get_exename(char *buf, size_t size)
{
    char linkname[64]; /* /proc/<pid>/exe */
    pid_t pid;
    ssize_t ret;

    /* Get our PID and build the name of the link in /proc */
    pid = getpid();

    if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
        return NULL;

    /* Now read the symbolic link */
    ret = readlink(linkname, buf, size);

    /* In case of an error, leave the handling up to the caller */
    if (ret == -1)
        return NULL;

    /* Report insufficient buffer size */
    if ((size_t)ret >= size)
        return NULL;

    /* Ensure proper NUL termination */
    buf[ret] = 0;

    return buf;
}
#elif defined(WIN32)
char *get_exename(char *buf, size_t size)
{
    if (GetModuleFileName(NULL, buf, size) == 0)
        return NULL;

    return buf;
}
#elif defined(MACOSX)
#include "/Developer/Headers/FlatCarbon/Processes.h"
#include "/Developer/Headers/FlatCarbon/Files.h"
char *get_exename(char *buf, size_t size)
{
    ProcessSerialNumber PSN;
    FSRef ref;

    if (GetCurrentProcess(&PSN) < 0 ||
        GetProcessBundleLocation(&PSN, &ref) < 0 ||
        FSRefMakePath(&ref, buf, size) < 0)
        return NULL;

    return buf;
}
#endif
