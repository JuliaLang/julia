/*	$OpenBSD: s_llrintf.c,v 1.2 2006/09/25 22:16:48 kettenis Exp $	*/
/* $NetBSD: llrintf.c,v 1.2 2004/10/13 15:18:32 drochner Exp $ */

/*
 * Written by Matthias Drochner <drochner@NetBSD.org>.
 * Public domain.
 */

#define LRINTNAME llrintf
#define RESTYPE long long int
#define RESTYPE_MIN LLONG_MIN
#define RESTYPE_MAX LLONG_MAX

#include "s_lrintf.c"
