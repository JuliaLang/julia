/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001, 2004  The R Development Core Team.
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

/* don't disallow including this one more than once */

/* This is intended to be called from other header files, so not callable
   from C++ */

#undef LibExtern
#undef LibImport
#undef LibExport

/* Don't try to include CYGWIN here: decorating some symbols breaks
   the auto-export that it relies on, even if R_DLL_BUILD were set. */
#ifdef WIN32 /* WIN32 as does not depend on config.h */
#define LibImport __declspec(dllimport)
#define LibExport __declspec(dllexport)
#else
#define LibImport
#define LibExport
#endif

#ifdef __MAIN__
#define LibExtern LibExport
#define extern
#elif defined(R_DLL_BUILD)
#define LibExtern extern
#else
#define LibExtern extern LibImport
#endif
