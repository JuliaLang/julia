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

void JL_HIDDEN free(void* mem) {
    HeapFree(GetProcessHeap(), 0, mem);
}

int JL_HIDDEN fwrite(const char *str, size_t nchars, FILE *out) {
    DWORD written;
    if (out->isconsole) {
        // Windows consoles do not support UTF-8 (for reading input, though new Windows Terminal does for writing), only UTF-16.
        wchar_t* wstr = utf8_to_wchar(str);
        if (!wstr)
            return -1;
        if (WriteConsoleW(out->fd, wstr, wcslen(wstr), &written, NULL)) {
            free(wstr);
            return written;
        }
        free(wstr);
    } else {
        // However, we want to print UTF-8 if the output is a file.
        if (WriteFile(out->fd, str, nchars, &written, NULL))
            return written;
    }
    return -1;
}

int JL_HIDDEN fputs(const char *str, FILE *out) {
    return fwrite(str, strlen(str), out);
}

void JL_HIDDEN *malloc(const size_t size) {
    return HeapAlloc(GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS, size);
}

void JL_HIDDEN *realloc(void * mem, const size_t size) {
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

void JL_HIDDEN exit(int code) {
    ExitProcess(code);
}


/* Utilities to convert from Windows' wchar_t stuff to UTF-8 */

/* Decode one WTF-8 code point from the input string, advancing the pointer.
   Based on libuv's uv__wtf8_decode1. */
static int wtf8_decode1(const char** input) {
    unsigned int code_point;
    unsigned char b1;
    unsigned char b2;
    unsigned char b3;
    unsigned char b4;

    b1 = **input;
    if (b1 <= 0x7F)
        return b1; /* ASCII code point */
    if (b1 < 0xC2)
        return -1; /* invalid: continuation byte */
    code_point = b1;

    b2 = *++*input;
    if ((b2 & 0xC0) != 0x80)
        return -1; /* invalid: not a continuation byte */
    code_point = (code_point << 6) | (b2 & 0x3F);
    if (b1 <= 0xDF)
        return 0x7FF & code_point; /* two-byte character */

    b3 = *++*input;
    if ((b3 & 0xC0) != 0x80)
        return -1; /* invalid: not a continuation byte */
    code_point = (code_point << 6) | (b3 & 0x3F);
    if (b1 <= 0xEF)
        return 0xFFFF & code_point; /* three-byte character */

    b4 = *++*input;
    if ((b4 & 0xC0) != 0x80)
        return -1; /* invalid: not a continuation byte */
    code_point = (code_point << 6) | (b4 & 0x3F);
    if (b1 <= 0xF4) {
        code_point &= 0x1FFFFF;
        if (code_point <= 0x10FFFF)
            return code_point; /* four-byte character */
    }

    /* code point too large */
    return -1;
}

/* Get the surrogate pair value from UTF-16 input.
   Based on libuv's uv__get_surrogate_value. */
static int get_surrogate_value(const wchar_t* w_source_ptr) {
    unsigned short u;
    unsigned short next;

    u = w_source_ptr[0];
    if (u >= 0xD800 && u <= 0xDBFF) {
        next = w_source_ptr[1];
        if (next >= 0xDC00 && next <= 0xDFFF)
            return 0x10000 + ((u - 0xD800) << 10) + (next - 0xDC00);
    }
    return u;
}

/* Convert UTF-16 to WTF-8, returning allocated string.
   Based on libuv's uv_utf16_to_wtf8. */
char *wchar_to_utf8(const wchar_t *wstr) {
    const wchar_t *w_source_ptr;
    size_t target_len;
    int code_point;
    char *target;
    char *p;

    /* First pass: calculate required length */
    w_source_ptr = wstr;
    target_len = 0;
    while (1) {
        code_point = get_surrogate_value(w_source_ptr);
        if (code_point == 0)
            break;
        if (code_point < 0x80)
            target_len += 1;
        else if (code_point < 0x800)
            target_len += 2;
        else if (code_point < 0x10000)
            target_len += 3;
        else {
            target_len += 4;
            w_source_ptr++;
        }
        w_source_ptr++;
    }

    /* Allocate buffer */
    target = (char *)malloc(target_len + 1);
    if (!target)
        return NULL;

    /* Second pass: perform conversion */
    w_source_ptr = wstr;
    p = target;
    while (1) {
        code_point = get_surrogate_value(w_source_ptr);
        if (code_point == 0)
            break;
        if (code_point < 0x80) {
            *p++ = code_point;
        } else if (code_point < 0x800) {
            *p++ = 0xC0 | (code_point >> 6);
            *p++ = 0x80 | (code_point & 0x3F);
        } else if (code_point < 0x10000) {
            *p++ = 0xE0 | (code_point >> 12);
            *p++ = 0x80 | ((code_point >> 6) & 0x3F);
            *p++ = 0x80 | (code_point & 0x3F);
        } else {
            *p++ = 0xF0 | (code_point >> 18);
            *p++ = 0x80 | ((code_point >> 12) & 0x3F);
            *p++ = 0x80 | ((code_point >> 6) & 0x3F);
            *p++ = 0x80 | (code_point & 0x3F);
            w_source_ptr++;
        }
        w_source_ptr++;
    }
    *p = '\0';

    return target;
}

/* Convert WTF-8 to UTF-16, returning allocated string.
   Based on libuv's uv_wtf8_to_utf16 and uv_wtf8_length_as_utf16. */
wchar_t *utf8_to_wchar(const char *str) {
    const char *source_ptr;
    size_t w_target_len;
    int code_point;
    wchar_t *w_target;
    wchar_t *p;

    /* First pass: calculate required length */
    source_ptr = str;
    w_target_len = 0;
    do {
        code_point = wtf8_decode1(&source_ptr);
        if (code_point < 0)
            return NULL;
        if (code_point > 0xFFFF)
            w_target_len++;
        w_target_len++;
    } while (*source_ptr++);

    /* Allocate buffer */
    w_target = (wchar_t *)malloc(w_target_len * sizeof(wchar_t));
    if (!w_target)
        return NULL;

    /* Second pass: perform conversion */
    source_ptr = str;
    p = w_target;
    do {
        code_point = wtf8_decode1(&source_ptr);
        if (code_point > 0xFFFF) {
            *p++ = (((code_point - 0x10000) >> 10) + 0xD800);
            *p++ = ((code_point - 0x10000) & 0x3FF) + 0xDC00;
        } else {
            *p++ = code_point;
        }
    } while (*source_ptr++);

    return w_target;
}

size_t JL_HIDDEN strlen(const char * x) {
    int idx = 0;
    while (x[idx] != 0)
        idx++;
    return idx;
}

size_t JL_HIDDEN wcslen(const wchar_t * x) {
    int idx = 0;
    while (x[idx] != 0)
        idx++;
    return idx;
}

char JL_HIDDEN *strncat(char * base, const char * tail, size_t maxlen) {
    int base_len = strlen(base);
    int tail_len = strlen(tail);
    for (int idx=base_len; idx<min(maxlen, base_len + tail_len); ++idx) {
        base[idx] = tail[idx - base_len];
    }
    return base;
}

void JL_HIDDEN *memcpy(void * dest, const void * src, size_t len) {
    for (int idx=0; idx<len; ++idx) {
        ((char *)dest)[idx] = ((const char *)src)[idx];
    }
    return dest;
}

void JL_HIDDEN *memset(void *s, int c, size_t n) {
  unsigned char* p = s;
  while(n--)
    *p++ = (unsigned char)c;
  return s;
}

char JL_HIDDEN *dirname(char * x) {
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

char JL_HIDDEN *strchr(const char * haystack, int needle) {
    int idx=0;
    while (haystack[idx] != needle) {
        if (haystack[idx] == 0) {
            return NULL;
        }
        idx++;
    }
    return (char *)haystack + idx;
}
