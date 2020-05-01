// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <wchar.h>
#include <stdio.h> // for printf

#include "dtypes.h"

#ifdef _OS_WINDOWS_
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#define fileno _fileno
//#define lseek _lseek
#else
#include <unistd.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include "utils.h"
#include "utf8.h"
#include "utf8proc.h"
#include "ios.h"
#include "timefuncs.h"

#define MOST_OF(x) ((x) - ((x)>>4))

#ifdef __cplusplus
extern "C" {
#endif

void (*ios_set_io_wait_func)(int) = NULL;
static void set_io_wait_begin(int v)
{
    if (__likely(ios_set_io_wait_func)) {
        ios_set_io_wait_func(v);
    }
}

/* OS-level primitive wrappers */

#if defined(__APPLE__) || defined(_OS_WINDOWS_)
JL_DLLEXPORT void *memrchr(const void *s, int c, size_t n)
{
    const unsigned char *src = (unsigned char*)s + n;
    unsigned char uc = c;
    while (--src >= (unsigned char*)s)
        if (*src == uc)
            return (void*)src;
    return NULL;
}
#else
extern void *memrchr(const void *s, int c, size_t n);
#endif

/*
static int _fd_available(long fd)
{
#ifndef _OS_WINDOWS_
    fd_set set;
    struct timeval tv = {0, 0};

    FD_ZERO(&set);
    FD_SET(fd, &set);
    return (select(fd+1, &set, NULL, NULL, &tv)!=0);
#else
    return 1;
#endif
}
*/

static int _enonfatal(int err)
{
    return (err == EAGAIN ||/* err == EINPROGRESS ||*/ err == EINTR /*|| err == EWOULDBLOCK*/); //jwn
}

#define SLEEP_TIME 5//ms

#if defined(__APPLE__)
#define MAXSIZE ((1l << 31) - 1)   // OSX cannot handle blocks larger than this
#define LIMIT_IO_SIZE(n) ((n) < MAXSIZE ? (n) : MAXSIZE)
#elif defined(_OS_WINDOWS_)
#define MAXSIZE (0x7fffffff)       // Windows read() takes a uint
#define LIMIT_IO_SIZE(n) ((n) < (size_t)MAXSIZE ? (unsigned int)(n) : MAXSIZE)
#else
#define LIMIT_IO_SIZE(n) (n)
#endif

// return error code, #bytes read in *nread
// these wrappers retry operations until success or a fatal error
static int _os_read(long fd, void *buf, size_t n, size_t *nread)
{
    ssize_t r;

    n = LIMIT_IO_SIZE(n);
    while (1) {
        set_io_wait_begin(1);
        r = read((int)fd, buf, n);
        set_io_wait_begin(0);
        if (r > -1) {
            *nread = (size_t)r;
            return 0;
        }
        // This test is a hack to fix #11481 for Windows 7. Unnecessary for Windows 10.
        if (errno == ENOMEM && n > 80) {
            n >>= 3;
            continue;
        }
        if (!_enonfatal(errno)) {
            *nread = 0;
            return errno;
        }
        sleep_ms(SLEEP_TIME);
    }
    return 0;
}

static int _os_read_all(long fd, void *buf, size_t n, size_t *nread)
{
    size_t got;

    *nread = 0;

    while (n>0) {
        set_io_wait_begin(1);
        int err = _os_read(fd, buf, n, &got);
        set_io_wait_begin(0);
        n -= got;
        *nread += got;
        buf = (char *)buf + got;
        if (err || got==0)
            return err;
    }
    return 0;
}

static int _os_write(long fd, const void *buf, size_t n, size_t *nwritten)
{
    ssize_t r;

    while (1) {
        r = write((int)fd, buf, LIMIT_IO_SIZE(n));
        if (r > -1) {
            *nwritten = (size_t)r;
            return 0;
        }
        if (!_enonfatal(errno)) {
            *nwritten = 0;
            return errno;
        }
        sleep_ms(SLEEP_TIME);
    }
    return 0;
}

int _os_write_all(long fd, const void *buf, size_t n, size_t *nwritten)
{
    size_t wrote;

    *nwritten = 0;

    while (n>0) {
        int err = _os_write(fd, buf, n, &wrote);
        n -= wrote;
        *nwritten += wrote;
        buf = (char *)buf + wrote;
        if (err)
            return err;
    }
    return 0;
}


/* internal utility functions */

static char *_buf_realloc(ios_t *s, size_t sz)
{
    char *temp;

    if ((s->buf==NULL || s->buf==&s->local[0]) && (sz <= IOS_INLSIZE)) {
        /* TODO: if we want to allow shrinking, see if the buffer shrank
           down to this size, in which case we need to copy. */
        s->buf = &s->local[0];
        s->maxsize = IOS_INLSIZE;
        s->ownbuf = 1;
        return s->buf;
    }

    if (sz <= s->maxsize) return s->buf;

    if (s->ownbuf && s->buf != &s->local[0]) {
        // if we own the buffer we're free to resize it
        // always allocate 1 bigger in case user wants to add a NUL
        // terminator after taking over the buffer
        temp = (char*)LLT_REALLOC(s->buf, sz+1);
        if (temp == NULL)
            return NULL;
    }
    else {
        temp = (char*)LLT_ALLOC(sz+1);
        if (temp == NULL)
            return NULL;
        s->ownbuf = 1;
        if (s->size > 0)
            memcpy(temp, s->buf, (size_t)s->size);
    }

    s->buf = temp;
    s->maxsize = sz;
    return s->buf;
}

// write a block of data into the buffer at the current position, resizing
// if necessary. returns # written.
static size_t _write_grow(ios_t *s, const char *data, size_t n)
{
    size_t amt;
    size_t newsize;

    if (n == 0)
        return 0;

    if (s->bpos + n > s->size) {
        if (s->bpos + n > s->maxsize) {
            /* TODO: here you might want to add a mechanism for limiting
               the growth of the stream. */
            newsize = (size_t)(s->maxsize ? s->maxsize * 2 : 8);
            while (s->bpos + n > newsize)
                newsize *= 2;
            if (_buf_realloc(s, newsize) == NULL) {
                /* no more space; write as much as we can */
                amt = (size_t)(s->maxsize - s->bpos);
                if (amt > 0) {
                    memcpy(&s->buf[s->bpos], data, amt);
                }
                s->bpos += amt;
                s->size = s->maxsize;
                return amt;
            }
        }
        s->size = s->bpos + n;
    }
    memcpy(s->buf + s->bpos, data, n);
    s->bpos += n;

    return n;
}


/* interface functions, low level */

static size_t _ios_read(ios_t *s, char *dest, size_t n, int all)
{
    size_t tot = 0;
    size_t got, avail;
    int didread = 0;

    if (s->state == bst_wr) {
        ios_seek(s, ios_pos(s));
    }
    s->state = bst_rd;

    while (n > 0) {
        avail = (size_t)(s->size - s->bpos);

        if (avail > 0) {
            size_t ncopy = (avail >= n) ? n : avail;
            memcpy(dest, s->buf + s->bpos, ncopy);
            s->bpos += ncopy;
            if (ncopy >= n)
                return tot+ncopy;
        }
        if (s->bm == bm_mem || s->fd == -1) {
            // can't get any more data
            if (avail == 0)
                s->_eof = 1;
            return avail;
        }

        dest += avail;
        n -= avail;
        tot += avail;

        if (didread && !all) return tot;

        ios_flush(s);
        s->bpos = s->size = 0;
        s->fpos = -1;
        if (n > MOST_OF(s->maxsize)) {
            // doesn't fit comfortably in buffer; go direct
            if (all) {
                //result = _os_read_all(s->fd, dest, n, &got);
                _os_read_all(s->fd, dest, n, &got);
            }
            else {
                //result = _os_read(s->fd, dest, n, &got);
                _os_read(s->fd, dest, n, &got);
            }
            tot += got;
            if (got == 0)
                s->_eof = 1;
            return tot;
        }
        else {
            // refill buffer
            if (_os_read(s->fd, s->buf, (size_t)s->maxsize, &got)) {
                s->_eof = 1;
                return tot;
            }
            if (got == 0) {
                s->_eof = 1;
                return tot;
            }
            s->size = got;
        }
        didread = 1;
    }

    return tot;
}

size_t ios_read(ios_t *s, char *dest, size_t n)
{
    return _ios_read(s, dest, n, 0);
}

size_t ios_readall(ios_t *s, char *dest, size_t n)
{
    return _ios_read(s, dest, n, 1);
}

size_t ios_readprep(ios_t *s, size_t n)
{
    if (s->state == bst_wr && s->bm != bm_mem) {
        ios_flush(s);
        s->bpos = s->size = 0;
    }
    size_t space = (size_t)(s->size - s->bpos);
    s->state = bst_rd;
    if (space >= n || s->bm == bm_mem || s->fd == -1)
        return space;
    if (s->maxsize < s->bpos+n) {
        // it won't fit. grow buffer or move data back.
        if (n <= s->maxsize && space <= ((s->maxsize)>>2)) {
            if (space)
                memmove(s->buf, s->buf+s->bpos, space);
            s->size -= s->bpos;
            s->bpos = 0;
        }
        else {
            if (_buf_realloc(s, (size_t)(s->bpos + n))==NULL)
                return space;
        }
    }
    size_t got;
    s->fpos = -1;
    int result = _os_read(s->fd, s->buf+s->size, (size_t)(s->maxsize - s->size), &got);
    if (result)
        return space;
    s->size += got;
    return (size_t)(s->size - s->bpos);
}

static void _write_update_pos(ios_t *s)
{
    if (s->bpos > s->ndirty) s->ndirty = s->bpos;
    if (s->bpos > s->size)   s->size = s->bpos;
}

// directly copy a buffer to a descriptor
JL_DLLEXPORT size_t ios_write_direct(ios_t *dest, ios_t *src)
{
    char *data = src->buf;
    size_t n = (size_t)src->size;
    size_t nwr;
    dest->fpos = -1;
    _os_write_all(dest->fd, data, n, &nwr);
    return nwr;
}

size_t ios_write(ios_t *s, const char *data, size_t n)
{
    if (!s->writable) return 0;
    if (n == 0) return 0;
    size_t space;
    size_t wrote = 0;

    if (s->state == bst_rd) {
        ios_seek(s, ios_pos(s));
    }
    s->state = bst_wr;
    space = (size_t)(s->maxsize - s->bpos);

    if (s->bm == bm_mem) {
        wrote = _write_grow(s, data, n);
    }
    else if (s->bm == bm_none) {
        s->fpos = -1;
        _os_write_all(s->fd, data, n, &wrote);
        return wrote;
    }
    else if (n <= space) {
        if (s->bm == bm_line) {
            char *nl;
            if ((nl=(char*)memrchr(data, '\n', n)) != NULL) {
                size_t linesz = nl-data+1;
                s->bm = bm_block;
                wrote += ios_write(s, data, linesz);
                ios_flush(s);
                s->bm = bm_line;
                n -= linesz;
                data += linesz;
            }
        }
        memcpy(s->buf + s->bpos, data, n);
        s->bpos += n;
        wrote += n;
    }
    else {
        ios_flush(s);
        if (n > MOST_OF(s->maxsize)) {
            s->fpos = -1;
            _os_write_all(s->fd, data, n, &wrote);
            return wrote;
        }
        return ios_write(s, data, n);
    }
    _write_update_pos(s);
    return wrote;
}

// Returns 0 on success,
//        -1 on error which set errno, and
//        -2 on error which doesn't set errno.
int64_t ios_seek(ios_t *s, int64_t pos)
{
    s->_eof = 0;
    if (s->bm == bm_mem) {
        if (pos < 0 || pos > s->size)
            return -2;
        s->bpos = pos;
    }
    else {
        ios_flush(s);
        int64_t fdpos = lseek(s->fd, (off_t)pos, SEEK_SET);
        if (fdpos == (int64_t)-1)
            return fdpos;
        s->fpos = fdpos;
        s->bpos = s->size = 0;
    }
    return 0;
}

int64_t ios_seek_end(ios_t *s)
{
    s->_eof = 1;
    if (s->bm == bm_mem) {
        s->bpos = s->size;
    }
    else {
        ios_flush(s);
        int64_t fdpos = lseek(s->fd, 0, SEEK_END);
        if (fdpos == (int64_t)-1)
            return fdpos;
        s->fpos = fdpos;
        s->bpos = s->size = 0;
    }
    return 0;
}

// Returns 0 on success,
//        -1 on error which set errno, and
//        -2 on error which doesn't set errno.
int64_t ios_skip(ios_t *s, int64_t offs)
{
    if (offs != 0) {
        if (offs > 0) {
            if (offs <= (s->size - s->bpos)) {
                s->bpos += offs;
                return 0;
            }
            else if (s->bm == bm_mem) {
                // TODO: maybe grow buffer
                return -2;
            }
        }
        else if (offs < 0) {
            if (-offs <= (int64_t)s->bpos) {
                s->bpos += offs;
                s->_eof = 0;
                return 0;
            }
            else if (s->bm == bm_mem) {
                return -2;
            }
        }
        ios_flush(s);
        if (s->state == bst_wr)
            offs += s->bpos;
        else if (s->state == bst_rd)
            offs -= (s->size - s->bpos);
        int64_t fdpos = lseek(s->fd, (off_t)offs, SEEK_CUR);
        if (fdpos == (int64_t)-1)
            return fdpos;
        s->fpos = fdpos;
        s->bpos = s->size = 0;
        s->_eof = 0;
    }
    return 0;
}

int64_t ios_pos(ios_t *s)
{
    if (s->bm == bm_mem)
        return s->bpos;

    int64_t fdpos = s->fpos;
    if (fdpos == (int64_t)-1) {
        fdpos = lseek(s->fd, 0, SEEK_CUR);
        if (fdpos == (int64_t)-1)
            return fdpos;
        s->fpos = fdpos;
    }

    if (s->state == bst_wr)
        fdpos += s->bpos;
    else if (s->state == bst_rd)
        fdpos -= (s->size - s->bpos);
    return fdpos;
}

int ios_trunc(ios_t *s, size_t size)
{
    if (s->bm == bm_mem) {
        if (size == s->size)
            return 0;
        if (size < s->size) {
            if (s->bpos > size)
                s->bpos = size;
        }
        else {
            if (_buf_realloc(s, size)==NULL)
                return 0;
        }
        s->size = size;
        return 0;
    }
    else {
        ios_flush(s);
        if (s->state == bst_rd) {
            int64_t p = ios_pos(s);
            if (size < p + (s->size - s->bpos))
                s->size -= (p + (s->size - s->bpos) - size);
        }
#if !defined(_OS_WINDOWS_)
        if (ftruncate(s->fd, size) == 0)
#else
        if (_chsize_s(s->fd, size) == 0)
#endif
            return 0;
    }
    return 1;
}

int ios_eof(ios_t *s)
{
    if (s->state == bst_rd && s->bpos < s->size)
        return 0;
    if (s->bm == bm_mem)
        return (s->_eof ? 1 : 0);
    if (s->fd == -1)
        return 1;
    if (s->_eof)
        return 1;
    return 0;
    /*
    if (_fd_available(s->fd))
        return 0;
    s->_eof = 1;
    return 1;
    */
}

int ios_eof_blocking(ios_t *s)
{
    if (s->state == bst_rd && s->bpos < s->size)
        return 0;
    if (s->bm == bm_mem)
        return (s->_eof ? 1 : 0);
    if (s->fd == -1)
        return 1;

    if (ios_readprep(s, 1) < 1)
        return 1;
    return 0;
}

int ios_flush(ios_t *s)
{
    if (s->ndirty == 0 || s->bm == bm_mem || s->buf == NULL)
        return 0;
    if (s->fd == -1)
        return -1;

    if (s->state == bst_rd) {
        if (lseek(s->fd, -(off_t)s->size, SEEK_CUR) == (off_t)-1) {
        }
    }

    size_t nw, ntowrite=s->ndirty;
    s->fpos = -1;
    int err = _os_write_all(s->fd, s->buf, ntowrite, &nw);
    // todo: try recovering from some kinds of errors (e.g. retry)

    if (s->state == bst_rd) {
        if (lseek(s->fd, (off_t)(s->size - nw), SEEK_CUR) == (off_t)-1) {
        }
    }
    else if (s->state == bst_wr) {
        if (s->bpos != nw &&
            lseek(s->fd, (off_t)(s->bpos - nw), SEEK_CUR) == (off_t)-1) {
        }
        // now preserve the invariant that data to write
        // begins at the beginning of the buffer, and s->size refers
        // to how much valid file data is stored in the buffer.
        if (s->size > s->ndirty) {
            size_t delta = (size_t)(s->size - s->ndirty);
            memmove(s->buf, s->buf + s->ndirty, delta);
        }
        s->size -= s->ndirty;
        s->bpos = 0;
    }

    s->ndirty = 0;

    if (err)
        return err;
    if (nw < ntowrite)
        return -1;
    return 0;
}

int ios_close(ios_t *s)
{
    int err = ios_flush(s);
    if (s->fd != -1 && s->ownfd) {
        int err2 = close(s->fd);
        if (err2 != 0)
            err = err2;
    }
    s->fd = -1;
    if (s->buf!=NULL && s->ownbuf && s->buf!=&s->local[0]) {
        LLT_FREE(s->buf);
    }
    s->buf = NULL;
    s->size = s->maxsize = s->bpos = 0;
    return err;
}

int ios_isopen(ios_t *s)
{
    return s->fd != -1;
}

static void _buf_init(ios_t *s, bufmode_t bm)
{
    s->bm = bm;
    if (s->bm == bm_mem || s->bm == bm_none) {
        s->buf = &s->local[0];
        s->maxsize = IOS_INLSIZE;
    }
    else {
        s->buf = NULL;
        s->maxsize = 0;
        _buf_realloc(s, IOS_BUFSIZE);
    }
    s->size = s->bpos = 0;
}

char *ios_take_buffer(ios_t *s, size_t *psize)
{
    char *buf;

    ios_flush(s);

    if (s->buf == &s->local[0]) {
        buf = (char*)LLT_ALLOC((size_t)s->size + 1);
        if (buf == NULL)
            return NULL;
        if (s->size)
            memcpy(buf, s->buf, (size_t)s->size);
    }
    else {
        if (s->buf == NULL)
            buf = (char*)LLT_ALLOC((size_t)s->size + 1);
        else
            buf = s->buf;
    }
    buf[s->size] = '\0';

    *psize = s->size+1;  // buffer is actually 1 bigger for terminating NUL

    /* empty stream and reinitialize */
    _buf_init(s, s->bm);

    return buf;
}

int ios_setbuf(ios_t *s, char *buf, size_t size, int own)
{
    ios_flush(s);
    size_t nvalid=0;

    nvalid = (size_t)((size < s->size) ? size : s->size);
    if (nvalid > 0)
        memcpy(buf, s->buf, nvalid);
    if (s->bpos > nvalid) {
        // truncated
        s->bpos = nvalid;
    }
    s->size = nvalid;

    if (s->buf!=NULL && s->ownbuf && s->buf!=&s->local[0]) {
        LLT_FREE(s->buf);
    }
    s->buf = buf;
    s->maxsize = size;
    s->ownbuf = own;
    return 0;
}

int ios_bufmode(ios_t *s, bufmode_t mode)
{
    // no fd; can only do mem-only buffering
    if (s->fd == -1 && mode != bm_mem)
        return -1;
    s->bm = mode;
    return 0;
}

int ios_get_readable(ios_t *s)
{
    return s->readable;
}

int ios_get_writable(ios_t *s)
{
    return s->writable;
}

void ios_set_readonly(ios_t *s)
{
    if (!s->writable) return;
    ios_flush(s);
    s->state = bst_none;
    s->writable = 0;
}

static size_t ios_copy_(ios_t *to, ios_t *from, size_t nbytes, bool_t all)
{
    size_t total = 0, avail;
    if (!ios_eof(from)) {
        do {
            avail = ios_readprep(from, IOS_BUFSIZE/2);
            if (avail == 0) {
                from->_eof = 1;
                break;
            }
            size_t written, ntowrite;
            ntowrite = (avail <= nbytes || all) ? avail : nbytes;
            written = ios_write(to, from->buf+from->bpos, ntowrite);
            // TODO: should this be +=written instead?
            from->bpos += ntowrite;
            total += written;
            if (!all) {
                nbytes -= written;
                if (nbytes == 0)
                    break;
            }
            if (written < ntowrite)
                break;
        } while (!ios_eof(from));
    }
    return total;
}

size_t ios_copy(ios_t *to, ios_t *from, size_t nbytes)
{
    return ios_copy_(to, from, nbytes, 0);
}

size_t ios_copyall(ios_t *to, ios_t *from)
{
    return ios_copy_(to, from, 0, 1);
}

#define LINE_CHUNK_SIZE 160

size_t ios_copyuntil(ios_t *to, ios_t *from, char delim)
{
    size_t total = 0, avail = (size_t)(from->size - from->bpos);
    while (!ios_eof(from)) {
        if (avail == 0) {
            avail = ios_readprep(from, LINE_CHUNK_SIZE);
            if (avail == 0)
                break;
        }
        size_t written;
        char *pd = (char*)memchr(from->buf+from->bpos, delim, avail);
        if (pd == NULL) {
            written = ios_write(to, from->buf+from->bpos, avail);
            from->bpos += avail;
            total += written;
            avail = 0;
        }
        else {
            size_t ntowrite = pd - (from->buf+from->bpos) + 1;
            written = ios_write(to, from->buf+from->bpos, ntowrite);
            from->bpos += ntowrite;
            total += written;
            return total;
        }
    }
    from->_eof = 1;
    return total;
}

size_t ios_nchomp(ios_t *from, size_t ntowrite)
{
    assert(ntowrite > 0);
    size_t nchomp;
    if (ntowrite > 1 && from->buf[from->bpos+ntowrite - 2] == '\r') {
        nchomp = 2;
    }
    else {
        nchomp = 1;
    }
    return nchomp;
}

static void _ios_init(ios_t *s)
{
    // put all fields in a sane initial state
    s->bm = bm_block;
    s->state = bst_none;
    s->errcode = 0;
    s->buf = NULL;
    s->maxsize = 0;
    s->size = 0;
    s->bpos = 0;
    s->ndirty = 0;
    s->fpos = -1;
    s->lineno = 1;
    s->u_colno = 0;
    s->fd = -1;
    s->ownbuf = 1;
    s->ownfd = 0;
    s->_eof = 0;
    s->readable = 1;
    s->writable = 1;
    s->rereadable = 0;
}

/* stream object initializers. we do no allocation. */

#if !defined(_OS_WINDOWS_)
/*
 * NOTE: we do not handle system call restart in this function,
 * please do it manually:
 *
 *  do
 *      open_cloexec(...)
 *  while (-1 == fd && _enonfatal(errno))
 */
static int open_cloexec(const char *path, int flags, mode_t mode)
{
#ifdef O_CLOEXEC
    static int no_cloexec = 0;

    if (!no_cloexec) {
        set_io_wait_begin(1);
        int fd = open(path, flags | O_CLOEXEC, mode);
        set_io_wait_begin(0);

        if (fd != -1)
            return fd;
        if (errno != EINVAL)
            return -1;

        /* O_CLOEXEC not supported. */
        no_cloexec = 1;
    }
#endif
    set_io_wait_begin(1);
    int fd = open(path, flags, mode);
    set_io_wait_begin(0);
    return fd;
}
#endif

ios_t *ios_file(ios_t *s, const char *fname, int rd, int wr, int create, int trunc)
{
    int flags;
    int fd;
    if (!(rd || wr))
        // must specify read and/or write
        goto open_file_err;
    flags = wr ? (rd ? O_RDWR : O_WRONLY) : O_RDONLY;
    if (create) flags |= O_CREAT;
    if (trunc)  flags |= O_TRUNC;
#if defined(_OS_WINDOWS_)
    size_t wlen = MultiByteToWideChar(CP_UTF8, 0, fname, -1, NULL, 0);
    if (!wlen) goto open_file_err;
    wchar_t *fname_w = (wchar_t*)alloca(wlen*sizeof(wchar_t));
    if (!MultiByteToWideChar(CP_UTF8, 0, fname, -1, fname_w, wlen)) goto open_file_err;
    set_io_wait_begin(1);
    fd = _wopen(fname_w, flags | O_BINARY | O_NOINHERIT, _S_IREAD | _S_IWRITE);
    set_io_wait_begin(0);
#else
    // The mode of the created file is (mode & ~umask), which resolves with
    // default umask to u=rw,g=r,o=r
    do
        fd = open_cloexec(fname, flags,
                          S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    while (-1 == fd && _enonfatal(errno));
#endif
    if (fd == -1)
        goto open_file_err;

    s = ios_fd(s, fd, 1, 1);
    if (!rd)
        s->readable = 0;
    if (!wr)
        s->writable = 0;
    return s;
 open_file_err:
    s->fd = -1;
    return NULL;
}

// Portable ios analogue of mkstemp: modifies fname to replace
// trailing XXXX's with unique ID and returns the file handle s
// for writing and reading.
ios_t *ios_mkstemp(ios_t *s, char *fname)
{
    int fd;
    // would be better to use a libuv function once it exists (see libuv/libuv#322)
#ifdef _OS_WINDOWS_
    size_t wlen = MultiByteToWideChar(CP_UTF8, 0, fname, -1, NULL, 0);
    if (!wlen) goto open_file_err;
    wchar_t *fname_w = (wchar_t*)alloca(wlen*sizeof(wchar_t));
    if (!MultiByteToWideChar(CP_UTF8, 0, fname, -1, fname_w, wlen) ||
        !_wmktemp(fname_w) ||
        !WideCharToMultiByte(CP_UTF8, 0, fname_w, -1, fname, strlen(fname)+1,
                             NULL, NULL))
        goto open_file_err;
    fd = _wopen(fname_w, O_CREAT|O_TRUNC|O_RDWR | O_BINARY | O_NOINHERIT, _S_IREAD | _S_IWRITE);
#else
    fd = mkstemp(fname);
#endif
    ios_fd(s, fd, 1, 1);
    if (fd == -1)
        goto open_file_err;
    return s;
open_file_err:
    s->fd = -1;
    return NULL;
}

ios_t *ios_mem(ios_t *s, size_t initsize)
{
    _ios_init(s);
    s->bm = bm_mem;
    s->rereadable = 1;
    _buf_realloc(s, initsize);
    return s;
}

ios_t *ios_str(ios_t *s, char *str)
{
    size_t n = strlen(str);
    if (ios_mem(s, n+1)==NULL) return NULL;
    ios_write(s, str, n+1);
    ios_seek(s, 0);
    return s;
}

ios_t *ios_static_buffer(ios_t *s, char *buf, size_t sz)
{
    ios_mem(s, 0);
    ios_setbuf(s, buf, sz, 0);
    s->size = sz;
    ios_set_readonly(s);
    return s;
}

ios_t *ios_fd(ios_t *s, long fd, int isfile, int own)
{
    _ios_init(s);
    s->fd = fd;
    if (isfile) s->rereadable = 1;
    _buf_init(s, bm_block);
    s->ownfd = own;
    if (fd == STDERR_FILENO)
        s->bm = bm_none;
    if (fd == STDOUT_FILENO)
        s->bm = bm_line;
    return s;
}

ios_t *ios_stdin = NULL;
ios_t *ios_stdout = NULL;
ios_t *ios_stderr = NULL;

void ios_init_stdstreams(void)
{
    ios_stdin = (ios_t*)malloc_s(sizeof(ios_t));
    ios_fd(ios_stdin, STDIN_FILENO, 0, 0);

    ios_stdout = (ios_t*)malloc_s(sizeof(ios_t));
    ios_fd(ios_stdout, STDOUT_FILENO, 0, 0);
    ios_stdout->bm = bm_line;

    ios_stderr = (ios_t*)malloc_s(sizeof(ios_t));
    ios_fd(ios_stderr, STDERR_FILENO, 0, 0);
    ios_stderr->bm = bm_none;
}

/* higher level interface */

int ios_putc(int c, ios_t *s)
{
    char ch = (char)c;

    if (s->state == bst_wr && s->bpos < s->maxsize && s->bm != bm_none) {
        s->buf[s->bpos++] = ch;
        _write_update_pos(s);
        if (s->bm == bm_line && ch == '\n')
            ios_flush(s);
        return 1;
    }
    return (int)ios_write(s, &ch, 1);
}

int ios_getc(ios_t *s)
{
    char ch;
    if (s->state == bst_rd && s->bpos < s->size) {
        ch = s->buf[s->bpos++];
    }
    else {
        if (s->_eof) return IOS_EOF;
        if (ios_read(s, &ch, 1) < 1)
            return IOS_EOF;
    }
    if (ch == '\n') s->lineno++;
    return (unsigned char)ch;
}

int ios_peekc(ios_t *s)
{
    if (s->bpos < s->size)
        return (unsigned char)s->buf[s->bpos];
    if (s->_eof) return IOS_EOF;
    size_t n = ios_readprep(s, 1);
    if (n == 0)  return IOS_EOF;
    return (unsigned char)s->buf[s->bpos];
}

int ios_ungetc(int c, ios_t *s)
{
    if (s->state == bst_wr)
        return IOS_EOF;
    if (s->bpos > 0) {
        s->bpos--;
        s->buf[s->bpos] = (char)c;
        s->_eof = 0;
        return c;
    }
    if (s->size == s->maxsize) {
        if (_buf_realloc(s, s->maxsize*2) == NULL)
            return IOS_EOF;
    }
    memmove(s->buf + 1, s->buf, s->size);
    s->buf[0] = (char)c;
    s->size++;
    s->_eof = 0;
    return c;
}

int ios_getutf8(ios_t *s, uint32_t *pwc)
{
    int c;
    size_t sz;
    char c0;
    char buf[8];

    c = ios_getc(s);
    if (c == IOS_EOF)
        return IOS_EOF;
    c0 = (char)c;
    if ((unsigned char)c0 < 0x80) {
        *pwc = (uint32_t)(unsigned char)c0;
        if (c == '\n')
            s->u_colno = 0;
        else
            s->u_colno += utf8proc_charwidth(*pwc);
        return 1;
    }
    if (ios_ungetc(c, s) == IOS_EOF)
        return IOS_EOF;
    sz = u8_seqlen(&c0);
    if (!isutf(c0) || sz > 4)
        return 0;
    if (ios_readprep(s, sz) < sz)
        // NOTE: this can return EOF even if some bytes are available
        return IOS_EOF;
    int valid = u8_isvalid(&s->buf[s->bpos], sz);
    if (valid) {
        size_t i = s->bpos;
        *pwc = u8_nextchar(s->buf, &i);
        s->u_colno += utf8proc_charwidth(*pwc);
        ios_read(s, buf, sz);
    }
    return valid;
}

int ios_peekutf8(ios_t *s, uint32_t *pwc)
{
    int c;
    size_t sz;
    char c0;

    c = ios_peekc(s);
    if (c == IOS_EOF)
        return IOS_EOF;
    c0 = (char)c;
    if ((unsigned char)c0 < 0x80) {
        *pwc = (uint32_t)(unsigned char)c0;
        return 1;
    }
    sz = u8_seqlen(&c0);
    if (!isutf(c0) || sz > 4)
        return 0;
    if (ios_readprep(s, sz) < sz)
        return IOS_EOF;
    int valid = u8_isvalid(&s->buf[s->bpos], sz);
    if (valid) {
        size_t i = s->bpos;
        *pwc = u8_nextchar(s->buf, &i);
    }
    return valid;
}

int ios_pututf8(ios_t *s, uint32_t wc)
{
    char buf[8];
    if (wc < 0x80)
        return ios_putc((int)wc, s);
    size_t n = u8_toutf8(buf, 8, &wc, 1);
    return ios_write(s, buf, n);
}

void ios_purge(ios_t *s)
{
    if (s->state == bst_rd) {
        s->bpos = s->size;
    }
}

char *ios_readline(ios_t *s)
{
    ios_t dest;
    ios_mem(&dest, 0);
    ios_copyuntil(&dest, s, '\n');
    size_t n;
    return ios_take_buffer(&dest, &n);
}

extern int vasprintf(char **strp, const char *fmt, va_list ap);

int ios_vprintf(ios_t *s, const char *format, va_list args)
{
    char *str=NULL;
    int c;
    va_list al;
#if defined(_OS_WINDOWS_)
    al = args;
#else
    va_copy(al, args);
#endif /* _OS_WINDOWS_ */

    if (s->state == bst_wr && s->bpos < s->maxsize && s->bm != bm_none) {
        size_t avail = (size_t)(s->maxsize - s->bpos);
        char *start = s->buf + s->bpos;
        c = vsnprintf(start, avail, format, args);
        if (c < 0) {
#if defined(_OS_WINDOWS_)
            // on windows this can mean not enough space was available
#else
            va_end(al);
            return c;
#endif
        }
        else if (c < avail) {
            s->bpos += (size_t)c;
            _write_update_pos(s);
            // TODO: only works right if newline is at end
            if (s->bm == bm_line && memrchr(start, '\n', (size_t)c))
                ios_flush(s);
            va_end(al);
            return c;
        }
    }
    c = vasprintf(&str, format, al);

    if (c >= 0) {
        ios_write(s, str, c);

        LLT_FREE(str);
    }
    va_end(al);
    return c;
}

int ios_printf(ios_t *s, const char *format, ...)
{
    va_list args;
    int c;

    va_start(args, format);
    c = ios_vprintf(s, format, args);
    va_end(args);
    return c;
}

#ifdef __cplusplus
}
#endif
