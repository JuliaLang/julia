#include<stdio.h> 

extern long twox(long);
// extern void jl_init_basics(void);

int main()
{
   // jl_init_basics();
   printf("twox: %ld\n", twox(2));
   return 0;
}