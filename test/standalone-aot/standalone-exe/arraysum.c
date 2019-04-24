#include <stdio.h> 
#include <julia.h> 

extern long arraysum(long);
extern void init_lib(void);

int main()
{
   init_lib();
   printf("arraysum: %ld\n", arraysum(5));
   jl_atexit_hook(0);
   return 0;
}