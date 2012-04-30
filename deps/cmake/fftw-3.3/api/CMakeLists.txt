########### next target ###############

set(api_STAT_SRCS
    apiplan.c
    configure.c
    execute-dft-c2r.c
    execute-dft-r2c.c
    execute-dft.c
    execute-r2r.c
    execute-split-dft-c2r.c
    execute-split-dft-r2c.c
    execute-split-dft.c
    execute.c
    export-wisdom-to-file.c
    export-wisdom-to-string.c
    export-wisdom.c
    f77api.c
    flops.c
    forget-wisdom.c
    import-system-wisdom.c
    import-wisdom-from-file.c
    import-wisdom-from-string.c
    import-wisdom.c
    malloc.c
    map-r2r-kind.c
    mapflags.c
    mkprinter-file.c
    mktensor-iodims.c
    mktensor-rowmajor.c
    plan-dft-1d.c
    plan-dft-2d.c
    plan-dft-3d.c
    plan-dft-c2r-1d.c
    plan-dft-c2r-2d.c
    plan-dft-c2r-3d.c
    plan-dft-c2r.c
    plan-dft-r2c-1d.c
    plan-dft-r2c-2d.c
    plan-dft-r2c-3d.c
    plan-dft-r2c.c
    plan-dft.c
    plan-guru-dft-c2r.c
    plan-guru-dft-r2c.c
    plan-guru-dft.c
    plan-guru-r2r.c
    plan-guru-split-dft-c2r.c
    plan-guru-split-dft-r2c.c
    plan-guru-split-dft.c
    plan-many-dft-c2r.c
    plan-many-dft-r2c.c
    plan-many-dft.c
    plan-many-r2r.c
    plan-r2r-1d.c
    plan-r2r-2d.c
    plan-r2r-3d.c
    plan-r2r.c
    print-plan.c
    rdft2-pad.c
    the-planner.c
    version.c
    api.h
    f77funcs.h
    fftw3.h
    x77.h
    guru.h
    guru64.h
    mktensor-iodims.h
    plan-guru-dft-c2r.h
    plan-guru-dft-r2c.h
    plan-guru-dft.h
    plan-guru-r2r.h
    plan-guru-split-dft-c2r.h
    plan-guru-split-dft-r2c.h
    plan-guru-split-dft.h
    plan-guru64-dft-c2r.c
    plan-guru64-dft-r2c.c
    plan-guru64-dft.c
    plan-guru64-r2r.c
    plan-guru64-split-dft-c2r.c
    plan-guru64-split-dft-r2c.c
    plan-guru64-split-dft.c
    mktensor-iodims64.c)

addObjects(api ${api_STAT_SRCS})


########### install files ###############

install(FILES  ${TOP_SRCDIR}/api/fftw3.h ${TOP_SRCDIR}/api/fftw3.f DESTINATION include)
configure_file(${TOP_SRCDIR}/api/fftw3.f03.in ${CMAKE_CURRENT_BINARY_DIR}/fftw3.f03)
install(FILES  ${CMAKE_CURRENT_BINARY_DIR}/fftw3.f03 DESTINATION lib/pkgconfig)

include_directories("${TOP_SRCDIR}/kernel" "${TOP_SRCDIR}/dft" "${TOP_SRCDIR}/rdft" "${TOP_SRCDIR}/reodft")

IF(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/fftw.f03)
## Manually Configre to prepend ##
	file(READ ${TOP_SRCDIR}/api/fftw3.f03.in contents)
	set(contents "! Generated automatically.  DO NOT EDIT!\n\n integer, parameter :: C_FFTW_R2R_KIND = @C_FFTW_R2R_KIND@\n${contents}")
	string(CONFIGURE "${contents}" contents)
	file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/fftw.f03 "${contents}")
ENDIF()
