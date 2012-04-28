cmake_minimum_required(VERSION 2.8)
project(arpack)

enable_language(Fortran)

function(prefixlist list_name prefix)
    set(${list_name}_TMP)
    foreach(l ${${list_name}})
      list(APPEND ${list_name}_TMP ${prefix}${l} )
    endforeach()
    set(${list_name} "${${list_name}_TMP}" PARENT_SCOPE)
endfunction(prefixlist)

set(arpacksrc_DIR ${EXTERNAL_TOP_SRCDIR}/arpack-${arpack_VER}/SRC/)
set(arpackutil_DIR ${EXTERNAL_TOP_SRCDIR}/arpack-${arpack_VER}/UTIL/)

set(arpacksrc_STAT_SRCS
    sgetv0.f
    slaqrb.f
    sstqrb.f
    ssortc.f
    ssortr.f
    sstatn.f
    sstats.f
    snaitr.f
    snapps.f
    snaup2.f
    snaupd.f
    snconv.f
    sneigh.f
    sngets.f
    ssaitr.f
    ssapps.f
    ssaup2.f
    ssaupd.f
    ssconv.f
    sseigt.f
    ssgets.f
    sneupd.f
    sseupd.f
    ssesrt.f
    dgetv0.f
    dlaqrb.f
    dstqrb.f
    dsortc.f
    dsortr.f
    dstatn.f
    dstats.f
    dnaitr.f
    dnapps.f
    dnaup2.f
    dnaupd.f
    dnconv.f
    dneigh.f
    dngets.f
    dsaitr.f
    dsapps.f
    dsaup2.f
    dsaupd.f
    dsconv.f
    dseigt.f
    dsgets.f
    dneupd.f
    dseupd.f
    dsesrt.f
    cnaitr.f
    cnapps.f
    cnaup2.f
    cnaupd.f
    cneigh.f
    cneupd.f
    cngets.f
    cgetv0.f
    csortc.f
    cstatn.f
    znaitr.f
    znapps.f
    znaup2.f
    znaupd.f
    zneigh.f
    zneupd.f
    zngets.f
    zgetv0.f
    zsortc.f
    zstatn.f)

prefixlist(arpacksrc_STAT_SRCS "${arpacksrc_DIR}")

set(arpackutil_STAT_SRCS
    icnteq.f
    icopy.f
    iset.f
    iswap.f
    ivout.f
    svout.f
    smout.f
    dvout.f
    dmout.f
    cvout.f
    cmout.f
    zvout.f
    zmout.f)

prefixlist(arpackutil_STAT_SRCS "${arpackutil_DIR}")


########### next target ###############
add_library(arpack SHARED ${arpacksrc_STAT_SRCS} ${arpackutil_STAT_SRCS})
target_link_libraries(arpack ${LapackLIB} ${BLAS_LIBRARIES})
set_target_properties(arpack PROPERTIES VERSION 2.0.0 SOVERSION 2)
