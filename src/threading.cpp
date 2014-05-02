#include "julia.h"
#include <stdio.h>
#include <math.h>
#include <thread>
#include <iostream>

extern "C" {

void do_work(jl_function_t * func, jl_value_t* args, size_t start, size_t step, size_t length)
{
    printf("do_work(start=%ld,stop=%ld) \n", start, start+length*step);
    for(size_t i=start; i<start+length*step; i+=step)
    {
        jl_value_t* arg[2];
        arg[0]=args;
        arg[1]= jl_box_int64(i);
        jl_apply(func,arg,2);
    }
}

void jl_par_apply(jl_function_t * func, jl_value_t* args, size_t num_threads, size_t start, size_t step, size_t length)
{
    jl_gc_disable();

    // precompilation TODO: Don't compute the first time
    jl_value_t* idx = jl_box_int64(start);
    jl_call2(func, args, idx);

    std::thread* t = new std::thread[num_threads];

    size_t N = length - 1;
    size_t chunk = N / num_threads;
    size_t rem = N;
    for(size_t i=0; i < num_threads - 1; i++)
    {
      t[i] = std::thread(do_work,func,args,start+1+i*chunk*step, step, chunk  );
      rem -= chunk;
    }
    t[num_threads-1] = std::thread(do_work,func,args,start+1+(num_threads-1)*chunk*step, step, rem );

    // std::cout << "Launched from the main thread \n";

    for(size_t i=0; i < num_threads; i++)
    {
      t[i].join();
    }

    delete[] t;
    jl_gc_enable();
}


void run_thread(jl_function_t* f, jl_tuple_t* targs)
{
  jl_value_t** args;
  JL_GC_PUSHARGS(args, jl_tuple_len(targs));
  for(int l=0; l<jl_tuple_len(targs); l++)
    args[l] = jl_tupleref(targs,l);

  jl_apply(f,args,jl_tuple_len(targs));

  JL_GC_POP();
}

void* jl_create_thread(jl_function_t* f, jl_tuple_t* targs)
{
  std::thread* t = new std::thread[1];
  t[0] = std::thread(run_thread,f,targs);
  return (void*) t;
}

void jl_join_thread(void* t)
{
  ((std::thread*)t)->join();
}

void jl_destroy_thread(void* t)
{
  delete[] ((std::thread*)t);
}


}
