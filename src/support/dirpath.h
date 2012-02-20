#ifndef DIRPATH_H
#define DIRPATH_H

#ifdef WIN32
#define PATHSEP '\\'
#define PATHSEPSTRING "\\"
#define PATHLISTSEP ';'
#define PATHLISTSEPSTRING ";"
#define ISPATHSEP(c) ((c)=='/' || (c)=='\\')
#define MAXPATHLEN 1024
#else
#define PATHSEP '/'
#define PATHSEPSTRING "/"
#define PATHLISTSEP ':'
#define PATHLISTSEPSTRING ":"
#define ISPATHSEP(c) ((c)=='/')
#endif

void get_cwd(char *buf, size_t size);
int set_cwd(char *buf);
DLLEXPORT char *get_exename(char *buf, size_t size);

#endif
