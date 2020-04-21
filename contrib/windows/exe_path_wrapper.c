#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <shlwapi.h>

/* The maximum path length we allow the environment to impose upon us */
#define MAX_PATH_LEN 1920

/* PATH_ENTRIES is our simulated RPATH, usually of the form 'TEXT("../path1"), TEXT("../path2"), ...' */
#ifndef PATH_ENTRIES
#define PATH_ENTRIES  TEXT("")
#endif

LPWSTR pathEntries[] = {
    PATH_ENTRIES
};

/* JULIA_EXE_PATH is the relative path to julia.exe, usually of the form "../path/to/julia.exe" */
#ifndef JULIA_EXE_PATH
#define JULIA_EXE_PATH "../libexec/julia.exe"
#endif

int wmain(int argc, wchar_t *argv[], wchar_t *envp[]) {
    // Determine absolute path to true julia.exe sitting in `libexec/`
    WCHAR currFileDir[MAX_PATH_LEN];
    WCHAR juliaPath[MAX_PATH_LEN];
    if (!GetModuleFileName(NULL, currFileDir, MAX_PATH_LEN)) {
        fprintf(stderr, "ERROR: GetModuleFileName() failed with code %lu\n", GetLastError());
        exit(1);
    }
    PathRemoveFileSpec(currFileDir);
    PathCombine(juliaPath, currFileDir, TEXT(JULIA_EXE_PATH));

    // On windows, we simulate RPATH by generating a new PATH, and hiding the original
    // PATH into a sidechannel variable, JULIA_ORIGINAL_PATH, which will be restored
    // within the callee.
    LPWSTR pathVal = (LPWSTR) malloc(MAX_PATH_LEN*sizeof(WCHAR));
    DWORD dwRet = GetEnvironmentVariable(TEXT("PATH"), pathVal, MAX_PATH_LEN);
    if (dwRet > 0 && dwRet < MAX_PATH_LEN) {
        SetEnvironmentVariable(TEXT("JULIA_ORIGINAL_PATH"), pathVal);
    } else {
        SetEnvironmentVariable(TEXT("JULIA_ORIGINAL_PATH"), TEXT(""));
    }

    // We also set JULIA_BINDIR to the directory holding this file, as otherwise it
    // auto picks up `libexec` instead of `bin`, but only if it is not already set.
    if (GetEnvironmentVariable(TEXT("JULIA_BINDIR"), pathVal, MAX_PATH_LEN) == 0)
        SetEnvironmentVariable(TEXT("JULIA_BINDIR"), currFileDir);

    // Next, we construct a new PATH variable specifically for launching the inner `julia.exe`
    DWORD numPathEntries = sizeof(pathEntries)/sizeof(pathEntries[0]);
    pathVal[0] = '\0';
    // We always add the current directory (e.g. `bin/`) to PATH so that we can find e.g. libjulia.dll
    lstrcat(pathVal, currFileDir);

    // For each entry in PATH_ENTRIES, tack it on to the end, relative to the current directory:
    int env_idx;
    for (env_idx = 0; env_idx < numPathEntries; ++env_idx) {
        lstrcat(pathVal, TEXT(";"));
        lstrcat(pathVal, currFileDir);
        lstrcat(pathVal, TEXT("\\"));
        lstrcat(pathVal, pathEntries[env_idx]);
    }
    SetEnvironmentVariable(TEXT("PATH"), pathVal);
    free(pathVal);

    STARTUPINFO info;
    PROCESS_INFORMATION processInfo;
    DWORD exit_code = 1;
    GetStartupInfo(&info);
    if (CreateProcess(juliaPath, GetCommandLine(), NULL, NULL, TRUE, 0, NULL, NULL, &info, &processInfo)) {
        WaitForSingleObject(processInfo.hProcess, INFINITE);
        GetExitCodeProcess(processInfo.hProcess, &exit_code);
        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);
    }
    return exit_code;
}
