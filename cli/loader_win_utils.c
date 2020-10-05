// Workarounds for compiling via mingw without using libgcc_s
typedef struct {
    HANDLE fd;
    BOOL isconsole;
} FILE;

static FILE _stdout = { INVALID_HANDLE_VALUE };
static FILE _stderr = { INVALID_HANDLE_VALUE };

FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

int loader_fwrite(const WCHAR *str, size_t nchars, FILE *out) {
    DWORD written;
    if (out->isconsole) {
        if (WriteConsole(out->fd, str, nchars, &written, NULL))
            return written;
    } else {
        if (WriteFile(out->fd, str, sizeof(WCHAR) * nchars, &written, NULL))
            return written;
    }
    return -1;
}

int loader_fputs(const char *str, FILE *out) {
    wchar_t wstr[1024];
    utf8_to_wchar(str, wstr, 1024);
    return fwrite(wstr, wcslen(wstr), out);
}

void * loader_malloc(const size_t size) {
    return HeapAlloc(GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS, size);
}

void * loader_realloc(void * mem, const size_t size) {
    return HeapReAlloc(GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS, mem, size);
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
int wchar_to_utf8(const wchar_t * wstr, char *str, size_t maxlen) {
    /* Fast-path empty strings, as WideCharToMultiByte() returns zero for them. */
    if (wstr[0] == L'\0') {
        str[0] = '\0';
        return 1;
    }
    size_t len = WideCharToMultiByte(CP_UTF8, 0, wstr, -1, NULL, 0, NULL, NULL);
    if (!len)
        return 0;
    if (len > maxlen)
        return 0;
    if (!WideCharToMultiByte(CP_UTF8, 0, wstr, -1, str, len, NULL, NULL))
        return 0;
    return 1;
}

int utf8_to_wchar(const char * str, wchar_t * wstr, size_t maxlen) {
    /* Fast-path empty strings, as WideCharToMultiByte() returns zero for them. */
    if (str[0] == '\0') {
        wstr[0] = L'\0';
        return 1;
    }
    size_t len = MultiByteToWideChar(CP_UTF8, 0, str, -1, NULL, 0);
    if (!len)
        return 0;
    if (len > maxlen)
        return 0;
    if (!MultiByteToWideChar(CP_UTF8, 0, str, -1, wstr, len))
        return 0;
    return 1;
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
