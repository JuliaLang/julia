#ifndef LIBSUPPORT_H
#define LIBSUPPORT_H

#include <stdlib.h>
#include <stdarg.h>
#include "dtypes.h"
#include "utils.h"
#include "utf8.h"
#include "ios.h"
#include "socket.h"
#include "timefuncs.h"
#include "hashing.h"
#include "ptrhash.h"
#include "bitvector.h"
#include "dirpath.h"

DLLEXPORT void libsupport_init(void);

#endif
