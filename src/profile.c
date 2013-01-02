#include <signal.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include "julia.h"

#if defined(__APPLE__) || defined(__WIN32__)
#define MAX_BT_SIZE 1023
#else
#define MAX_BT_SIZE 80000
#endif

extern void rec_backtrace(size_t *bt_size_p, ptrint_t *bt_data);

static timer_t timerprof;
static struct sigevent sigprof;
static struct itimerspec itsprof;

static ptrint_t bt_data_prof[MAX_BT_SIZE+1];
static size_t bt_size_prof = 0;
static int bt_skip = 0;
static uint64_t nsecprof;

jl_array_t *profdata;

void profile_bt(union sigval v)
{
  size_t i; //,j;  
  
  // Get backtrace data
  rec_backtrace(&bt_size_prof, bt_data_prof);
  // Copy backtrace data to buffer
  int n = (bt_size_prof - bt_skip)*sizeof(ptrint_t);
  if (n > 0) {
    i = jl_array_len(profdata);
//     for (j = 0; j < n/sizeof(ptrint_t); j++)
//       ios_printf(JL_STDERR, "%llx ", bt_data_prof[j]);
//     ios_printf(JL_STDERR, "\n");
    jl_array_grow_end(profdata, n+sizeof(ptrint_t));
    memcpy(profdata->data+i, (const void*) bt_data_prof, n);
    memset(profdata->data+i+n, 0, sizeof(ptrint_t));  // mark end
  }
  // Re-arm the  timer
  itsprof.it_value.tv_nsec = nsecprof;
  timer_settime(timerprof, 0, &itsprof, NULL);
}

DLLEXPORT void profile_init(uint64_t nsec, int skip)
{
  nsecprof = nsec;
  bt_skip = skip;
  
  profdata = jl_alloc_array_1d(jl_array_uint8_type, 0);
  jl_gc_preserve((jl_value_t*) profdata);
}

DLLEXPORT void profile_start_timer(void)
{
  sigprof.sigev_notify = SIGEV_THREAD;
  sigprof.sigev_signo = 1;
  sigprof.sigev_notify_function = profile_bt;
  sigprof.sigev_notify_attributes = NULL;
  itsprof.it_value.tv_sec = 0;
  itsprof.it_interval.tv_sec = 0;       // make it fire once
  itsprof.it_interval.tv_nsec = 0;
  
  timer_create(CLOCK_REALTIME, &sigprof, &timerprof);
  itsprof.it_value.tv_nsec = nsecprof;
  timer_settime(timerprof, 0, &itsprof, NULL);
}

DLLEXPORT void profile_stop_timer(void)
{
  timer_delete(timerprof);
}

DLLEXPORT uint8_t* get_profile_data(void)
{
  return profdata->data;
}

DLLEXPORT int get_profile_data_len(void)
{
  return jl_array_len(profdata)/sizeof(ptrint_t);
}

DLLEXPORT void clear_profile_data(void)
{
  size_t n;
  n = jl_array_len(profdata);
  jl_array_del_end(profdata, n);
}
