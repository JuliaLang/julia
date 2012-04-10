#ifndef TIMEFUNCS_H
#define TIMEFUNCS_H



u_int64_t i64time(void);
DLLEXPORT double clock_now(void);
void sleep_ms(int ms);

DLLEXPORT int read_tm_year(struct tm *t); 
DLLEXPORT int read_tm_mon(struct tm *t); 
DLLEXPORT int read_tm_mday(struct tm *t); 
DLLEXPORT int read_tm_hour(struct tm *t); 
DLLEXPORT int read_tm_min(struct tm *t); 
DLLEXPORT int read_tm_sec(struct tm *t); 
DLLEXPORT int read_tm_isdst(struct tm *t); 
DLLEXPORT long int read_tm_gmtoff(struct tm *t); 
DLLEXPORT char *read_tm_zone(struct tm *t); 
DLLEXPORT char *default_tzone();
DLLEXPORT long int default_gmtoff();
DLLEXPORT int default_isdst();

#endif
