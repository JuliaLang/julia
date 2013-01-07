#include <signal.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include "julia.h"

extern size_t rec_backtrace(ptrint_t *bt_data, size_t maxsize);

static timer_t timerprof;
static struct itimerspec itsprof;

static ptrint_t* bt_data_prof = NULL;
static size_t bt_size_max = 0;
static size_t bt_size_cur = 0;
static uint64_t nsecprof;

// The handler function, called whenever the profiling timer elapses
static void profile_bt(int signal, siginfo_t *si, void *uc)
{
  if (si->si_value.sival_ptr == &timerprof && bt_size_cur < bt_size_max) {
    // Get backtrace data
    bt_size_cur += rec_backtrace(bt_data_prof+bt_size_cur, bt_size_max-bt_size_cur-1);
    // Mark the end of this block with 0
    bt_data_prof[bt_size_cur] = 0;
    bt_size_cur++;
    // Re-arm the  timer
    if (bt_size_cur < bt_size_max) {
      itsprof.it_value.tv_nsec = nsecprof;
      timer_settime(timerprof, 0, &itsprof, NULL);
    }
  }
}

DLLEXPORT void profile_init(size_t maxsize, uint64_t nsec)
{
  if (bt_data_prof != NULL)
    free(bt_data_prof);
  bt_data_prof = (ptrint_t*) malloc(maxsize*sizeof(ptrint_t));
  if (bt_data_prof == NULL && maxsize > 0)
    jl_errorf("Could not allocate space for %d profiling samples", maxsize);
  bt_size_max = maxsize;
  nsecprof = nsec;
}

DLLEXPORT void profile_start_timer(void)
{
  struct sigevent sigprof;
  struct sigaction sa;
  
  // Establish the signal handler
  memset(&sa, 0, sizeof(struct sigaction));
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = profile_bt;
  sigemptyset(&sa.sa_mask);
  if (sigaction(SIGUSR1, &sa, NULL) == -1)
    jl_error("Cannot specify signal action for profiling");

  // Establish the signal event
  memset(&sigprof, 0, sizeof(struct sigevent));
  sigprof.sigev_notify = SIGEV_SIGNAL;
  sigprof.sigev_signo = SIGUSR1;
  sigprof.sigev_value.sival_ptr = &timerprof;
//   sigprof.sigev_notify_function = profile_bt;
//   sigprof.sigev_notify_attributes = NULL;
  if (timer_create(CLOCK_REALTIME, &sigprof, &timerprof) == -1)
    jl_error("Cannot create the timer for profiling");

  // Start the timer
  itsprof.it_value.tv_sec = 0;
  itsprof.it_interval.tv_sec = 0;       // make it fire once
  itsprof.it_interval.tv_nsec = 0;
  itsprof.it_value.tv_nsec = nsecprof;
  if (timer_settime(timerprof, 0, &itsprof, NULL) == -1)
    jl_error("Cannot start the timer for profiling");
}

DLLEXPORT void profile_stop_timer(void)
{
  timer_delete(timerprof);
}

DLLEXPORT uint8_t* get_profile_data(void)
{
  if (bt_size_cur == bt_size_max)
    ios_printf(JL_STDERR, "Warning: profile buffer is full, profiling may have stopped before your program completed. Consider initializing with a larger buffer and re-running.");
  return (uint8_t*) bt_data_prof;
}

DLLEXPORT size_t get_profile_data_len(void)
{
  return bt_size_cur;
}

DLLEXPORT void clear_profile_data(void)
{
  bt_size_cur = 0;
}
