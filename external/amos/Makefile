FC = gfortran

FFLAGS = -O2 -fPIC

src = $(shell ls *.f)

obj = 	$(src:%.f=%.o)

all: $(obj)

clean: 
	/bin/rm -f $(obj)
