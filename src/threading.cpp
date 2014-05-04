#include "julia.h"
#include <stdio.h>
#include <math.h>
#include <thread>
#include <iostream>
#include <mutex>

extern "C" {

extern jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);
extern jl_tuple_t *arg_type_tuple(jl_value_t **args, size_t nargs);

static std::mutex _global_lock;

void jl_global_lock()
{
  _global_lock.lock();
}

void jl_global_unlock()
{
  _global_lock.unlock();
}

std::mutex _ct_mutex;
void run_thread(jl_function_t* f, jl_tuple_t* targs)
{
  _ct_mutex.lock();
  jl_value_t** args = new jl_value_t*[jl_tuple_len(targs)];
  for(int l=0; l<jl_tuple_len(targs); l++)
    args[l] = jl_tupleref(targs,l);
  _ct_mutex.unlock();

  jl_apply(f,args,jl_tuple_len(targs));

  delete[] args;
}

void* jl_create_thread(jl_function_t* f, jl_tuple_t* targs)
{
  _ct_mutex.lock();
  
  int nargs = jl_tuple_len(targs);
            
  jl_tuple_t* argtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs);
  jl_function_t *mfunc = jl_get_specialization(f, argtypes);

  _ct_mutex.unlock();

  std::thread* t = new std::thread[1];
  t[0] = std::thread(run_thread,mfunc,targs);
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

static std::mutex _do_work_mutex;

void do_work(jl_function_t * func, jl_value_t** args, int nargs, size_t start, size_t step, size_t length)
{
    //printf("do_work(start=%ld,stop=%ld) \n", start, start+length*step);
    
    jl_value_t** args2 = new jl_value_t*[nargs];
    for(int i=0; i< nargs -1; i++)
      args2[i] = args[i];
    
    for(size_t i=start; i<start+length*step; i+=step)
    {
        _do_work_mutex.lock();
        args2[nargs-1] = jl_box_int64(i);
        _do_work_mutex.unlock();
        
        jl_apply(func,args2,nargs);
        
    }
    
    delete[] args2;
}

void jl_par_apply(jl_function_t * func, jl_value_t* targs, size_t num_threads, size_t start, size_t step, size_t length)
{
    jl_gc_disable();

    int nargs = jl_tuple_len(targs)+1;
    
    // convert tuple into array
    jl_value_t** args;    
    JL_GC_PUSHARGS(args, nargs);
    for(int l=0; l<jl_tuple_len(targs); l++)
      args[l] = jl_tupleref(targs,l);

    //jl_methtable_t *mt = jl_gf_mtable(func);
    //jl_function_t *mfunc = jl_method_table_assoc_exact(mt, args, nargs);
    
    jl_tuple_t* firstargtypes = arg_type_tuple(&jl_tupleref(targs,0), nargs-1);
    jl_tuple_t* argtypes = jl_alloc_tuple(nargs);
    for(int l=0; l<nargs-1; l++)
      jl_tupleset(argtypes, l, jl_tupleref(firstargtypes,l));
    jl_tupleset(argtypes, nargs-1, jl_int64_type);
    jl_function_t *mfunc = jl_get_specialization(func, argtypes);

    std::thread* t = new std::thread[num_threads];

    size_t chunk = length / num_threads;
    size_t rem = length;
    for(size_t i=0; i < num_threads - 1; i++)
    {
      t[i] = std::thread(do_work,mfunc,args,nargs,start+i*chunk*step, step, chunk  );
      rem -= chunk;
    }
    t[num_threads-1] = std::thread(do_work,mfunc,args,nargs,start+(num_threads-1)*chunk*step, step, rem );

    // std::cout << "Launched from the main thread \n";

    for(size_t i=0; i < num_threads; i++)
    {
      t[i].join();
    }

    delete[] t;
    
    JL_GC_POP();
    jl_gc_enable();
}




}
