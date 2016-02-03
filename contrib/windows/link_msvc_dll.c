int __stdcall _DllMainCRTStartup(void* hinstance, unsigned int fdwReason, void* lpvReserved)
{
    return 1;
}

/*
strip -g -o sys-debug-msvc.o sys-debug.o

x86_64-w64-mingw32-gcc -c -o link_msvc_dll.o link_msvc_dll.c

link /dll sys-debug.o link_msvc_dll.o /out:sys-debug-msvc.dll ../libjulia-debug.dll.a ../libopenlibm.a /nodefaultlib:libcmt /nodefaultlib:libcpmt /nodefaultlib:oldnames /debug C:/mingw-w64/x86_64-5.1.0-win32-seh-rt_v4-rev0/mingw64/lib/gcc/x86_64-w64-mingw32/5.1.0/libssp.dll.a C:/mingw-w64/x86_64-5.1.0-win32-seh-rt_v4-rev0/mingw64/lib/gcc/x86_64-w64-mingw32/5.1.0/libgcc.a C:/mingw-w64/x86_64-5.1.0-win32-seh-rt_v4-rev0/mingw64/x86_64-w64-mingw32/lib/libmsvcrt.a dll.o /export:jl_sysimg_cpu_target /export:jl_sysimg_gvars /export:jl_sysimg_fvars /export:jl_globalUnique /export:jl_sysimg_cpu_target /export:jl_sysimg_cpu_cpuid /export:jl_system_image_data /export:jl_system_image_size /debug
 */
