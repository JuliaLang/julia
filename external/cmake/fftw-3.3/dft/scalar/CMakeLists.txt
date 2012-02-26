add_subdirectory(codelets)

include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft")

########### next target ###############

set(dft_scalar_STAT_SRCS
    n.c
    t.c
    f.h
    n.h
    q.h
    t.h)

addObjects(dft_scalar ${dft_scalar_STAT_SRCS})
#target_link_libraries(dft_scalar dft dft_scalar_codelets)


########### install files ###############
