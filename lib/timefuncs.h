#ifndef __TIMEFUNCS_H_
#define __TIMEFUNCS_H_

u_int64_t i64time();
double clock_now();
void timestring(double seconds, char *buffer, size_t len);
double parsetime(const char *str);
void sleep_ms(int ms);
void timeparts(int32_t *buf, double t);

#endif
