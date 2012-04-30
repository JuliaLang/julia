#ifdef TIME_WITH_SYS_TIME
//Standard test for TIME_WITH_SYS_TIME

#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

int
main ()
{
if ((struct tm *) 0)
return 0;
  ;
  return 0;
}

#endif
