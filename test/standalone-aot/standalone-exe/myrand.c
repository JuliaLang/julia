#include <stdio.h> 
#include <julia.h> 

extern double myrand();
extern void init_lib(void);

int main()
{
   init_lib();
   printf("rand: %f\n", myrand());
   jl_atexit_hook(0);
   return 0;
}