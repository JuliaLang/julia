/* This file is adapted from musl-libc
----------------------------------------------------------------------
Copyright © 2005-2014 Rich Felker, et al.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
----------------------------------------------------------------------
*/

#include <wchar.h>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include "getopt.h"

char *optarg;
int optind=1, opterr=1, optopt, __optpos, __optreset=0;

#define optpos __optpos

int getopt(int argc, char * const argv[], const char *optstring)
{
  int i;
  wchar_t c, d;
  int k, l;
  char *optchar;

  if (!optind || __optreset) {
    __optreset = 0;
    __optpos = 0;
    optind = 1;
  }

  if (optind >= argc || !argv[optind] || argv[optind][0] != '-' || !argv[optind][1])
    return -1;
  if (argv[optind][1] == '-' && !argv[optind][2])
    return optind++, -1;

  if (!optpos) optpos++;
  if ((k = mbtowc(&c, argv[optind]+optpos, MB_LEN_MAX)) < 0) {
    k = 1;
    c = 0xfffd; /* replacement char */
  }
  optchar = argv[optind]+optpos;
  optopt = c;
  optpos += k;

  if (!argv[optind][optpos]) {
    optind++;
    optpos = 0;
  }

  for (i=0; (l = mbtowc(&d, optstring+i, MB_LEN_MAX)) && d!=c; i+=l>0?l:1);

  if (d != c) {
    if (optstring[0] != ':' && opterr) {
      fprintf(stderr, "%s: illegal option: %c\n", argv[0], optchar);
    }
    return '?';
  }
  if (optstring[i+1] == ':') {
    if (optind >= argc) {
      if (optstring[0] == ':') return ':';
      if (opterr) {
        fprintf(stderr, "%s: option requires an argument: %c\n", argv[0], optchar);
      }
      return '?';
    }
    if (optstring[i+2] == ':') optarg = 0;
    if (optstring[i+2] != ':' || optpos) {
      optarg = argv[optind++] + optpos;
      optpos = 0;
    }
  }
  return c;
}

static int __getopt_long(int argc, char *const *argv, const char *optstring, const struct option *longopts, int *idx, int longonly)
{
  if (!optind || __optreset) {
    __optreset = 0;
    __optpos = 0;
    optind = 1;
  }
  if (optind >= argc || !argv[optind] || argv[optind][0] != '-') return -1;
  if ((longonly && argv[optind][1]) ||
    (argv[optind][1] == '-' && argv[optind][2]))
  {
    int i;
    for (i=0; longopts[i].name; i++) {
      const char *name = longopts[i].name;
      char *opt = argv[optind]+1;
      if (*opt == '-') opt++;
      for (; *name && *name == *opt; name++, opt++);
      if (*name || (*opt && *opt != '=')) continue;
      if (*opt == '=') {
        if (!longopts[i].has_arg) continue;
        optarg = opt+1;
      } else {
        if (longopts[i].has_arg == required_argument) {
          if (!(optarg = argv[++optind]))
            return ':';
        } else optarg = NULL;
      }
      optind++;
      if (idx) *idx = i;
      if (longopts[i].flag) {
        *longopts[i].flag = longopts[i].val;
        return 0;
      }
      return longopts[i].val;
    }
    if (argv[optind][1] == '-') {
      optind++;
      return '?';
    }
  }
  return getopt(argc, argv, optstring);
}

int getopt_long(int argc, char *const *argv, const char *optstring, const struct option *longopts, int *idx)
{
  return __getopt_long(argc, argv, optstring, longopts, idx, 0);
}

int getopt_long_only(int argc, char *const *argv, const char *optstring, const struct option *longopts, int *idx)
{
  return __getopt_long(argc, argv, optstring, longopts, idx, 1);
}
