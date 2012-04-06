/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-7  The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 */

#define MATHLIB_STANDALONE 1
#include <Rmath.h>

#include <stdio.h>
typedef enum {
    BUGGY_KINDERMAN_RAMAGE,
    AHRENS_DIETER,
    BOX_MULLER,
    USER_NORM,
    INVERSION,
    KINDERMAN_RAMAGE
} N01type;

int
main(int argc, char** argv)
{
/* something to force the library to be included */
    qnorm(0.7, 0.0, 1.0, 0, 0);
    printf("*** loaded '%s'\n", argv[0]);
    set_seed(123, 456);
    N01_kind = AHRENS_DIETER;
    printf("one normal %f\n", norm_rand());
    set_seed(123, 456);
    N01_kind = BOX_MULLER;
    printf("normal via BM %f\n", norm_rand());
    
    return 0;
}
