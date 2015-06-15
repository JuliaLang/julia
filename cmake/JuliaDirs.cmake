#

jl_set_make_flag(JULIAHOME "${CMAKE_SOURCE_DIR}")

if(NOT DEFINED CMAKE_INSTALL_DATAROOTDIR)
  set(CMAKE_INSTALL_DATAROOTDIR share)
endif()

if(NOT DEFINED CMAKE_INSTALL_DOCDIR)
  set(CMAKE_INSTALL_DOCDIR "${CMAKE_INSTALL_DATAROOTDIR}/doc")
endif()

include(GNUInstallDirs)
function(set_install_dir name val)
  set("${name}" "${val}" PARENT_SCOPE)
  jl_set_make_flag("${name}" "${val}")
endfunction()
set_install_dir(prefix "${CMAKE_INSTALL_PREFIX}")
set_install_dir(bindir "${CMAKE_INSTALL_FULL_BINDIR}")
set_install_dir(libdir "${CMAKE_INSTALL_FULL_LIBDIR}")
set_install_dir(private_libdir "${CMAKE_INSTALL_FULL_LIBDIR}/julia")
set_install_dir(libexecdir "${CMAKE_INSTALL_FULL_LIBEXECDIR}")
set_install_dir(datarootdir "${CMAKE_INSTALL_FULL_DATAROOTDIR}")
set_install_dir(docdir "${CMAKE_INSTALL_FULL_DOCDIR}")
set_install_dir(mandir "${CMAKE_INSTALL_FULL_MANDIR}")
set_install_dir(man1dir "${CMAKE_INSTALL_FULL_MANDIR}/man1")
set_install_dir(includedir "${CMAKE_INSTALL_FULL_INCLUDEDIR}")
set_install_dir(sysconfdir "${CMAKE_INSTALL_FULL_SYSCONFDIR}")

function(set_build_dir name val)
  set("build_${name}" "${val}" PARENT_SCOPE)
  jl_set_make_flag("build_${name}" "${val}")
  file(MAKE_DIRECTORY "${val}")
endfunction()

jl_set_make_flag(JULIAHOME "${CMAKE_SOURCE_DIR}")

# Directories where things get built into
set_build_dir(prefix "${CMAKE_BINARY_DIR}/usr")
set_build_dir(staging "${CMAKE_BINARY_DIR}/usr-staging")
set_build_dir(bindir "${build_prefix}/bin")
set_build_dir(libdir "${build_prefix}/lib")
set_build_dir(private_libdir "${build_prefix}/lib/julia")
set_build_dir(libexecdir "${build_prefix}/libexec")
set_build_dir(datarootdir "${build_prefix}/share")
set_build_dir(docdir "${build_prefix}/doc/julia")
set_build_dir(mandir "${build_prefix}/man")
set_build_dir(man1dir "${build_mandir}/man1")
set_build_dir(includedir "${build_prefix}/include")
set_build_dir(sysconfdir "${build_prefix}/etc")
# On Windows, we want shared library files to end up in ${build_bindir},
# instead of ${build_libdir}
if(WIN32)
  set_build_dir(shlibdir "${build_bindir}")
else()
  set_build_dir(shlibdir "${build_libdir}")
endif()

file(MAKE_DIRECTORY "${build_sysconfdir}/julia")
file(MAKE_DIRECTORY "${build_datarootdir}/julia/base")
file(MAKE_DIRECTORY "${build_datarootdir}/julia/test")

function(set_relative_dir name from to)
  file(RELATIVE_PATH rel "${from}" "${to}")
  set("${name}" "${rel}" PARENT_SCOPE)
  jl_set_make_flag("${name}" "${rel}")
endfunction()

set_relative_dir(build_libdir_rel "${build_bindir}" "${build_libdir}")
set_relative_dir(libdir_rel "${bindir}" "${libdir}")
set_relative_dir(build_private_libdir_rel "${build_bindir}"
  "${build_private_libdir}")
set_relative_dir(private_libdir_rel "${bindir}" "${private_libdir}")
set_relative_dir(datarootdir_rel "${bindir}" "${datarootdir}")
set_relative_dir(docdir_rel "${bindir}" "${docdir}")
set_relative_dir(sysconfdir_rel "${bindir}" "${sysconfdir}")

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/include/julia")

jl_set_make_flag(CMAKE_BINARY_DIR "${CMAKE_BINARY_DIR}")
