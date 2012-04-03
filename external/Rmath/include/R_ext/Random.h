/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2011    Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_RANDOM_H
#define R_RANDOM_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <R_ext/Boolean.h>

typedef enum {
    WICHMANN_HILL,
    MARSAGLIA_MULTICARRY,
    SUPER_DUPER,
    MERSENNE_TWISTER,
    KNUTH_TAOCP,
    USER_UNIF,
    KNUTH_TAOCP2,
    LECUYER_CMRG
} RNGtype;

/* Different kinds of "N(0,1)" generators :*/
typedef enum {
    BUGGY_KINDERMAN_RAMAGE,
    AHRENS_DIETER,
    BOX_MULLER,
    USER_NORM,
    INVERSION,
    KINDERMAN_RAMAGE
} N01type;


void GetRNGstate(void);
void PutRNGstate(void);

double unif_rand(void);
/* These are also defined in Rmath.h */
double norm_rand(void);
double exp_rand(void);

typedef unsigned int Int32;
double * user_unif_rand(void);
void user_unif_init(Int32);
int * user_unif_nseed(void);
int * user_unif_seedloc(void);

double * user_norm_rand(void);

void FixupProb(double *, int, int, Rboolean);

#ifdef  __cplusplus
}
#endif

#endif /* R_RANDOM_H */
