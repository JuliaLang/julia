all: _laplace.so laplace_for_update1.out laplace_for_update2.out for_laplace.so c_laplace.out c_laplace_parallel_update.out c_laplace_parallel_update_pointer.out
intel: intel_laplace_for_update1.out intel_laplace_for_update2.out intel_for_laplace.so intel_c_laplace.out intel_c_laplace_parallel_update.out intel_c_laplace_parallel_update_pointer.out cilk_laplace.out
#all: _laplace.so intel_laplace_for_update1.out laplace_for_update1.out intel_laplace_for_update2.out laplace_for_update2.out for_laplace.so c_laplace.out intel_c_laplace.out c_laplace_parallel_update.out intel_c_laplace_parallel_update.out intel_c_laplace_parallel_update_pointer.out c_laplace_parallel_update_pointer.out

#_laplace.so: _laplace.pyx
#	sage -python setup.py build_ext --inplace

#_laplace.so: _laplace.o
#	gcc -pthread -shared _laplace.o -L$(SAGE_ROOT)/local/lib -lpython2.6 -o _laplace.so
#
#_laplace.o: _laplace.c __laplace.c
#	gcc -fno-strict-aliasing -DNDEBUG -O3 -Wall -Wstrict-prototypes -fPIC -I$(SAGE_ROOT)/local/lib/python2.6/site-packages/numpy/core/include -I$(SAGE_ROOT)/local/include/python2.6 -c _laplace.c -o _laplace.o
#	gcc -fno-strict-aliasing -g -O2 -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I$(SAGE_ROOT)/local/lib/python2.6/site-packages/numpy/core/include -I$(SAGE_ROOT)/local/include/python2.6 -c _laplace.c -o _laplace.o

_laplace.so: _laplace.pyx parameter.h
	sage -c "from sage.all import cython_create_local_so; cython_create_local_so('_laplace.pyx')"
#	sage -cython _laplace.pyx

for_laplace.so: for_laplace.f90
	sage -python -c "import numpy.f2py as f2py2e;f2py2e.main()" -c -m for_laplace for_laplace.f90 --quiet --f77exec=$(SAGE_ROOT)/local/bin/sage-g77_shared --f90exec=$(SAGE_ROOT)/local/bin/sage-g77_shared

intel_for_laplace.so: for_laplace.f90
	f2py --fcompiler=intelem -c for_laplace.f90 -m intel_for_laplace

laplace_for_update1.out: laplace_for_update1.f90 for_laplace.f90
	gfortran -O3 -ffast-math -funroll-loops -o laplace_for_update1.out  for_laplace.f90 laplace_for_update1.f90

intel_laplace_for_update1.out: laplace_for_update1.f90 for_laplace.f90
	ifort -O3 -funroll-loops -o intel_laplace_for_update1.out  for_laplace.f90 laplace_for_update1.f90

laplace_for_update2.out: laplace_for_update2.f90 for_laplace.f90
	gfortran -O3 -ffast-math -funroll-loops -o laplace_for_update2.out  for_laplace.f90 laplace_for_update2.f90

intel_laplace_for_update2.out: laplace_for_update2.f90 for_laplace.f90
	ifort -O3 -funroll-loops -o intel_laplace_for_update2.out  for_laplace.f90 laplace_for_update2.f90

c_laplace.out: c_laplace.c parameter.h
	gcc -O3 c_laplace.c -o c_laplace.out

intel_c_laplace.out: c_laplace.c parameter.h
	icc -O3 c_laplace.c -o intel_c_laplace.out

c_laplace_parallel_update.out: c_laplace_parallel_update.c parameter.h
	gcc -O3 c_laplace_parallel_update.c -o c_laplace_parallel_update.out

intel_c_laplace_parallel_update.out: c_laplace_parallel_update.c parameter.h
	icc -O3 c_laplace_parallel_update.c -o intel_c_laplace_parallel_update.out

intel_c_laplace_parallel_update_pointer.out: c_laplace_parallel_update_pointer.c parameter.h
	icc -O3 c_laplace_parallel_update_pointer.c -o intel_c_laplace_parallel_update_pointer.out

c_laplace_parallel_update_pointer.out: c_laplace_parallel_update_pointer.c parameter.h
	gcc -O3 c_laplace_parallel_update_pointer.c -o c_laplace_parallel_update_pointer.out

cilk_laplace.out: cilk_laplace.c parameter.h
	icc -O3 cilk_laplace.c -o cilk_laplace.out

clean:
	rm *.so *.out data*
