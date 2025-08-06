#include <dlfcn.h>
#include <stdio.h>
#include <signal.h>

int main(int argc, char *argv[])
{
    // This test doesn't do much yet, except check
    // https://github.com/JuliaLang/julia/issues/57240
    signal(SIGCHLD, SIG_IGN);
    void *handle = dlopen(LIBJULIA_PATH, RTLD_LAZY);
    return 0;
}
