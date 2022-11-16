// This file is a part of Julia. License is MIT: https://julialang.org/license

// Workarounds for compiling via mingw without using libgcc_s
typedef struct {
    HANDLE fd;
    BOOL isconsole;
} FILE;

static FILE _stdout = { INVALID_HANDLE_VALUE };
static FILE _stderr = { INVALID_HANDLE_VALUE };

FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

int loader_fwrite(const char *str, size_t nchars, FILE *out) {
    DWORD written;
    if (out->isconsole) {
        // Windows consoles do not support UTF-8 (for reading input, though new Windows Terminal does for writing), only UTF-16.
        wchar_t* wstr = utf8_to_wchar(str);
        if (!wstr)
            return -1;
        if (WriteConsoleW(out->fd, wstr, wcslen(wstr), &written, NULL)) {
            loader_free(wstr);
            return written;
        }
        loader_free(wstr);
    } else {
        // However, we want to print UTF-8 if the output is a file.
        if (WriteFile(out->fd, str, nchars, &written, NULL))
            return written;
    }
    return -1;
}

int loader_fputs(const char *str, FILE *out) {
    return loader_fwrite(str, loader_strlen(str), out);
}

void * loader_malloc(const size_t size) {
    return HeapAlloc(GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS, size);
}

void * loader_realloc(void * mem, const size_t size) {
    return HeapReAlloc(GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS, mem, size);
}

void loader_free(void* mem) {
    HeapFree(GetProcessHeap(), 0, mem);
}

LPWSTR *CommandLineToArgv(LPWSTR lpCmdLine, int *pNumArgs) {
    LPWSTR out = lpCmdLine;
    LPWSTR cmd = out;
    unsigned MaxEntries = 4;
    unsigned backslashes = 0;
    int in_quotes = 0;
    int empty = 1;
    LPWSTR *cmds;
    *pNumArgs = 0;
    cmds = (LPWSTR*)malloc(sizeof(LPWSTR) * MaxEntries);
    while (1) {
        WCHAR c = *lpCmdLine++;
        switch (c) {
        case 0:
            if (!empty) {
                *out++ = '\0';
                cmds[(*pNumArgs)++] = cmd;
            }
            cmds[*pNumArgs] = NULL;
            return cmds;
        default:
            *out++ = c;
            empty = 0;
            break;
        case '"':
            out -= backslashes / 2; // remove half of the backslashes
            if (backslashes % 2)
                *(out - 1) = '"'; // replace \ with "
            else
                in_quotes = !in_quotes; // treat as quote delimater
            empty = 0;
            break;
        case '\t':
        case ' ':
            if (in_quotes) {
                *out++ = c;
            } else if (!empty) {
                *out++ = '\0';
                cmds[(*pNumArgs)++] = cmd;
                cmd = out;
                empty = 1;
                if (*pNumArgs >= MaxEntries - 1) {
                    MaxEntries *= 2;
                    cmds = (LPWSTR*)realloc(cmds, sizeof(LPWSTR) * MaxEntries);
                }
            }
        }
        if (c == '\\')
            backslashes++;
        else
            backslashes = 0;
    }
}

void setup_stdio() {
    DWORD mode = 0;
    _stdout.fd = GetStdHandle(STD_OUTPUT_HANDLE);
    _stdout.isconsole = GetConsoleMode(_stdout.fd, &mode);
    _stderr.fd = GetStdHandle(STD_ERROR_HANDLE);
    _stderr.isconsole = GetConsoleMode(_stderr.fd, &mode);
}

void loader_exit(int code) {
    ExitProcess(code);
}


/* Utilities to convert from Windows' wchar_t stuff to UTF-8 */
char *wchar_to_utf8(const wchar_t * wstr) {
    /* Fast-path empty strings, as WideCharToMultiByte() returns zero for them. */
    if (wstr[0] == L'\0') {
        char *str = malloc(1);
        str[0] = '\0';
        return str;
    }
    size_t len = WideCharToMultiByte(CP_UTF8, 0, wstr, -1, NULL, 0, NULL, NULL);
    if (!len)
        return NULL;
    char *str = (char *)malloc(len);
    if (!WideCharToMultiByte(CP_UTF8, 0, wstr, -1, str, len, NULL, NULL))
        return NULL;
    return str;
}

wchar_t *utf8_to_wchar(const char * str) {
    /* Fast-path empty strings, as MultiByteToWideChar() returns zero for them. */
    if (str[0] == '\0') {
        wchar_t *wstr = malloc(sizeof(wchar_t));
        wstr[0] = L'\0';
        return wstr;
    }
    size_t len = MultiByteToWideChar(CP_UTF8, 0, str, -1, NULL, 0);
    if (!len)
        return NULL;
    wchar_t *wstr = (wchar_t *)malloc(len * sizeof(wchar_t));
    if (!MultiByteToWideChar(CP_UTF8, 0, str, -1, wstr, len))
        return NULL;
    return wstr;
}

size_t loader_strlen(const char * x) {
    int idx = 0;
    while (x[idx] != 0)
        idx++;
    return idx;
}

size_t loader_wcslen(const wchar_t * x) {
    int idx = 0;
    while (x[idx] != 0)
        idx++;
    return idx;
}

char * loader_strncat(char * base, const char * tail, size_t maxlen) {
    int base_len = strlen(base);
    int tail_len = strlen(tail);
    for (int idx=base_len; idx<min(maxlen, base_len + tail_len); ++idx) {
        base[idx] = tail[idx - base_len];
    }
    return base;
}

void * loader_memcpy(void * dest, const void * src, size_t len) {
    for (int idx=0; idx<len; ++idx) {
        ((char *)dest)[idx] = ((const char *)src)[idx];
    }
    return dest;
}

char * loader_dirname(char * x) {
    int idx = strlen(x);
    while (idx > 0 && x[idx] != PATHSEPSTRING[0]) {
        idx -= 1;
    }
    if (x[idx] == PATHSEPSTRING[0]) {
        // Special-case x == "/"
        if (idx == 0) {
            x[1] = '\0';
            return x;
        } else {
            x[idx] = '\0';
            return x;
        }
    }
    x[0] = '.';
    x[1] = '\0';
    return x;
}

char * loader_strchr(const char * haystack, int needle) {
    int idx=0;
    while (haystack[idx] != needle) {
        if (haystack[idx] == 0) {
            return NULL;
        }
        idx++;
    }
    return (char *)haystack + idx;
}
