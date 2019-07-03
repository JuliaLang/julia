#include "julia.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

JL_DLLEXPORT int jl_printf(JL_STREAM *s, const char *format, ...)
{
    va_list args;
    int c;

    va_start(args, format);
	c = vprintf(format, args);
    va_end(args);
    return c;
}

JL_DLLEXPORT void jl_safe_printf(const char *fmt, ...)
{
    static char buf[1000];
    buf[0] = '\0';

    va_list args;
    va_start(args, fmt);
    // Not async signal safe on some platforms?
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    buf[999] = '\0';
    if (write(STDERR_FILENO, buf, strlen(buf)) < 0) {
        // nothing we can do; ignore the failure
    }
}

void jl_init_signal_async(void)
{
}

JL_DLLEXPORT int jl_vprintf(JL_STREAM *s, const char *format, va_list args)
{
    int c;
	va_list _args;
	va_copy(_args, args);
	c = vprintf(format, _args);
    va_end(_args);
    return c;
}

JL_DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path)
{
    return rename(src_path, dst_path);
}

JL_DLLEXPORT void jl_uv_flush(JL_STREAM *stream) {
    if (stream == (void*)STDOUT_FILENO ||
        stream == (void*)STDERR_FILENO)
        return;
    ios_flush((ios_t*)stream);
}

JL_DLLEXPORT void jl_uv_puts(JL_STREAM *stream, const char *str, size_t n)
{
    assert(stream);

		int fd = -1;
		// Fallback for output during early initialisation...
    if (stream == (void*)STDOUT_FILENO) {
        fd = STDOUT_FILENO;
    }
    else if (stream == (void*)STDERR_FILENO) {
        fd = STDERR_FILENO;
    }

    if ((ssize_t)fd != -1) {
        // Write to file descriptor...
        write(fd, str, n);
				return;
    }

    ios_write((ios_t*)stream, str, n);
}

JL_DLLEXPORT void jl_uv_putb(JL_STREAM *stream, uint8_t b)
{
    jl_uv_puts(stream, (char*)&b, 1);
}

JL_DLLEXPORT void JL_NORETURN jl_exit(int status)
{
    exit(status);
}

JL_DLLEXPORT int jl_cwd(char *buf, size_t *sz)
{
    if (getcwd(buf, *sz) == NULL)
        return -1;
    *sz = strlen(buf);
    return 0;
}

// --- stat ---
JL_DLLEXPORT int jl_sizeof_stat(void) { return sizeof(struct stat); }

JL_DLLEXPORT int32_t jl_stat(const char *path, char *statbuf)
{
    int err = stat(path, (struct stat*)statbuf);
		if (err != 0)
			return -errno;
		return err;
}

JL_DLLEXPORT int32_t jl_lstat(const char *path, char *statbuf)
{
		int err = lstat(path, (struct stat*)statbuf);
		if (err != 0)
			return -errno;
		return err;
}

JL_DLLEXPORT int32_t jl_fstat(int fd, char *statbuf)
{
		int err = fstat(fd, (struct stat*)statbuf);
		if (err != 0)
			return -errno;
		return err;
}

JL_DLLEXPORT unsigned int jl_stat_dev(char *statbuf)
{
    return ((struct stat*)statbuf)->st_dev;
}

JL_DLLEXPORT unsigned int jl_stat_ino(char *statbuf)
{
    return ((struct stat*)statbuf)->st_ino;
}

JL_DLLEXPORT unsigned int jl_stat_mode(char *statbuf)
{
    return ((struct stat*)statbuf)->st_mode;
}

JL_DLLEXPORT unsigned int jl_stat_nlink(char *statbuf)
{
    return ((struct stat*)statbuf)->st_nlink;
}

JL_DLLEXPORT unsigned int jl_stat_uid(char *statbuf)
{
    return ((struct stat*)statbuf)->st_uid;
}

JL_DLLEXPORT unsigned int jl_stat_gid(char *statbuf)
{
    return ((struct stat*)statbuf)->st_gid;
}

JL_DLLEXPORT unsigned int jl_stat_rdev(char *statbuf)
{
    return ((struct stat*)statbuf)->st_rdev;
}

JL_DLLEXPORT uint64_t jl_stat_size(char *statbuf)
{
    return ((struct stat*)statbuf)->st_size;
}

JL_DLLEXPORT uint64_t jl_stat_blksize(char *statbuf)
{
    return ((struct stat*)statbuf)->st_blksize;
}

JL_DLLEXPORT uint64_t jl_stat_blocks(char *statbuf)
{
    return ((struct stat*)statbuf)->st_blocks;
}

/*
// atime is stupid, let's not support it
JL_DLLEXPORT double jl_stat_atime(char *statbuf)
{
  uv_stat_t *s;
  s = (uv_stat_t*)statbuf;
  return (double)s->st_atim.tv_sec + (double)s->st_atim.tv_nsec * 1e-9;
}
*/

JL_DLLEXPORT double jl_stat_mtime(char *statbuf)
{
    struct stat *s;
    s = (struct stat*)statbuf;
    return (double)s->st_mtim.tv_sec + (double)s->st_mtim.tv_nsec * 1e-9;
}

JL_DLLEXPORT double jl_stat_ctime(char *statbuf)
{
    struct stat *s;
    s = (struct stat*)statbuf;
    return (double)s->st_ctim.tv_sec + (double)s->st_ctim.tv_nsec * 1e-9;
}

JL_DLLEXPORT int jl_is_file(char *fname)
{
    struct stat stbuf;
    return !(jl_stat(fname, (char*)&stbuf) != 0 || (stbuf.st_mode & S_IFMT) != S_IFREG);
}

JL_DLLEXPORT void jl_wake_libuv(void)
{
    return;
}
