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
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

#include "timefuncs.h"

#ifdef WIN32
/*
double tvals2float(struct tm *t, struct timeb *tstruct)
{
    return (double)t->tm_hour * 3600 + (double)t->tm_min * 60 +
        (double)t->tm_sec + (double)tstruct->millitm/1.0e3;
}
*/
double floattime()
{
    struct timeb tstruct;

    ftime(&tstruct);
    return (double)tstruct.time + (double)tstruct.millitm/1.0e3;
}
#else
double tv2float(struct timeval *tv)
{
    return (double)tv->tv_sec + (double)tv->tv_usec/1.0e6;
}

double diff_time(struct timeval *tv1, struct timeval *tv2)
{
    return tv2float(tv1) - tv2float(tv2);
}
#endif

// return as many bits of system randomness as we can get our hands on
u_int64_t i64time()
{
    u_int64_t a;
#ifdef WIN32
    struct timeb tstruct;
    ftime(&tstruct);
    a = (((u_int64_t)tstruct.time)<<32) + (u_int64_t)tstruct.millitm;
#else
    struct timeval now;
    gettimeofday(&now, NULL);
    a = (((u_int64_t)now.tv_sec)<<32) + (u_int64_t)now.tv_usec;
#endif

    return a;
}

double clock_now()
{
#ifdef WIN32
    return floattime();
#else
    struct timeval now;

    gettimeofday(&now, NULL);
    return tv2float(&now);
#endif
}

void timestring(double seconds, char *buffer, size_t len)
{
    time_t tme = (time_t)seconds;
    char *fmt = "%c"; /* needed to suppress GCC warning */

#ifdef LINUX
    struct tm tm;

    localtime_r(&tme, &tm);
    strftime(buffer, len, fmt, &tm);
#else
    static char *wdaystr[] = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
    static char *monthstr[] = {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                               "Sep","Oct","Nov","Dec"};
    struct tm *tm;
    int hr;

    tm = localtime(&tme);
    hr = tm->tm_hour;
    if (hr > 12) hr -= 12;
    if (hr == 0) hr = 12;
    snprintf(buffer, len, "%s %02d %s %d %02d:%02d:%02d %s %s",
             wdaystr[tm->tm_wday], tm->tm_mday, monthstr[tm->tm_mon],
             tm->tm_year+1900, hr, tm->tm_min, tm->tm_sec,
             tm->tm_hour>11 ? "PM" : "AM", "");
#endif
}

#if defined(LINUX) || defined(MACOSX)
extern char *strptime(const char *s, const char *format, struct tm *tm);
double parsetime(char *str)
{
    char *fmt = "%c"; /* needed to suppress GCC warning */
    char *res;
    time_t t;
    struct tm tm;

    res = strptime(str, fmt, &tm);
    if (res != NULL) {
        t = mktime(&tm);
        if (t == ((time_t)-1))
            return -1;
        return (double)t;
    }
    return -1;
}
#else
// TODO
#endif

void sleep_ms(int ms)
{
    if (ms == 0)
        return;

#ifdef WIN32
    Sleep(ms);
#else
    struct timeval timeout;

    timeout.tv_sec = ms/1000;
    timeout.tv_usec = (ms % 1000) * 1000;
    select(0, NULL, NULL, NULL, &timeout);
#endif
}

void timeparts(int32_t *buf, double t)
{
    time_t tme = (time_t)t;

#ifndef WIN32
    struct tm tm;
    localtime_r(&tme, &tm);
    tm.tm_year += 1900;
    memcpy(buf, (char*)&tm, sizeof(struct tm));
#else
    struct tm *tm;

    tm = localtime(&tme);
    tm->tm_year += 1900;
    memcpy(buf, (char*)tm, sizeof(struct tm));
#endif
}
