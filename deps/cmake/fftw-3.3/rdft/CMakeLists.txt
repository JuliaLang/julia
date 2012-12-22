include_directories ("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft")

add_subdirectory(scalar)
add_subdirectory(simd)


########### next target ###############
set(RDFT2 
buffered2.c 
direct2.c 
nop2.c 
rank0-rdft2.c 
rank-geq2-rdft2.c
plan2.c 
problem2.c 
solve2.c 
vrank-geq1-rdft2.c 
rdft2-rdft.c	
rdft2-tensor-max-index.c 
rdft2-inplace-strides.c 
rdft2-strides.c
khc2c.c 
ct-hc2c.h
ct-hc2c.c
ct-hc2c-direct.c)

set(rdft_STAT_SRCS
    #,hc2hc.h
    hc2hc.c
    dft-r2hc.c
    dht-r2hc.c
    dht-rader.c
    buffered.c
    #codelet-rdft.h
    conf.c
    direct-r2r.c
    direct-r2c.c
    generic.c
    hc2hc-direct.c
    hc2hc-generic.c
    khc2hc.c
    kr2c.c
    kr2r.c
    indirect.c
    nop.c
    plan.c
    problem.c
    rank0.c
    rank-geq2.c
    #rdft.h
    rdft-dht.c
    solve.c
    vrank-geq1.c
    vrank3-transpose.c
)

list(APPEND rdft_STAT_SRCS ${RDFT2})

include_directories("${top_srcdir}/kernel" "${top_srcdir}/dft")
addObjects(rdft  ${rdft_STAT_SRCS})
#target_link_libraries(rdft rdft_scalar rdft_scalar_r2cb rdft_scalar_r2cf rdft_scalar_r2r)

########### install files ###############




#original Makefile.am contents follow:

#AM_CPPFLAGS = -I$(top_srcdir)/kernel -I$(top_srcdir)/dft
#SUBDIRS = scalar simd
#
#noinst_LTLIBRARIES = librdft.la
#
## pkgincludedir = $(includedir)/fftw3@PREC_SUFFIX@
## pkginclude_HEADERS = codelet-rdft.h rdft.h
#
#RDFT2 = buffered2.c direct2.c nop2.c rank0-rdft2.c rank-geq2-rdft2.c	\
#plan2.c problem2.c solve2.c vrank-geq1-rdft2.c rdft2-rdft.c		\
#rdft2-tensor-max-index.c rdft2-inplace-strides.c rdft2-strides.c	\
#khc2c.c ct-hc2c.h ct-hc2c.c ct-hc2c-direct.c
#
#librdft_la_SOURCES = hc2hc.h hc2hc.c dft-r2hc.c dht-r2hc.c dht-rader.c	\
#buffered.c codelet-rdft.h conf.c direct-r2r.c direct-r2c.c generic.c	\
#hc2hc-direct.c hc2hc-generic.c khc2hc.c kr2c.c kr2r.c indirect.c nop.c	\
#plan.c problem.c rank0.c rank-geq2.c rdft.h rdft-dht.c solve.c		\
#vrank-geq1.c vrank3-transpose.c $(RDFT2)
