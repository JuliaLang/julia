/**
 * @file basename.c
 * Copyright 2012, 2013 MinGW.org project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/*
 * Provides an implementation of the "basename" function, conforming
 * to SUSv3, with extensions to accommodate Win32 drive designators,
 * and suitable for use on native Microsoft(R) Win32 platforms.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <malloc.h>
#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

DLLEXPORT char *basename( char *path )
{
    size_t len;
    static char *retfail = NULL;

    /* to handle path names for files in multibyte character locales,
     * we need to set up LC_CTYPE to match the host file system locale
     */

    char *locale = setlocale( LC_CTYPE, NULL );
    if( locale != NULL ) locale = strdup( locale );
    setlocale( LC_CTYPE, "" );

    if( path && *path )
    {
        /* allocate sufficient local storage space,
         * in which to create a wide character reference copy of path
         */

        wchar_t *refcopy = (wchar_t*)alloca((1 + (len = mbstowcs(NULL, path, 0)))*sizeof(wchar_t));

        /* create the wide character reference copy of path,
         * and step over the drive designator, if present ...
         */

        wchar_t *refpath = refcopy;
        if( ((len = mbstowcs( refpath, path, len )) > 1) && (refpath[1] == L':') )
        {
            /* FIXME: maybe should confirm *refpath is a valid drive designator */

            refpath += 2;
        }

        /* ensure that our wide character reference path is NUL terminated */

        refcopy[ len ] = L'\0';

        /* check again, just to ensure we still have a non-empty path name ... */

        if( *refpath )
        {
            /* and, when we do, process it in the wide character domain ...
             * scanning from left to right, to the char after the final dir separator
             */

            wchar_t *refname;
            for( refname = refpath ; *refpath ; ++refpath )
            {
                if( (*refpath == L'/') || (*refpath == L'\\') )
                {
                    /* we found a dir separator ...
                     * step over it, and any others which immediately follow it
                     */

                    while( (*refpath == L'/') || (*refpath == L'\\') )
                        ++refpath;

                    /* if we didn't reach the end of the path string ... */

                    if( *refpath )

                        /* then we have a new candidate for the base name */

                        refname = refpath;

                    /* otherwise ...
                     * strip off any trailing dir separators which we found
                     */

                    else while(  (refpath > refname)
                                 &&          ((*--refpath == L'/') || (*refpath == L'\\'))   )
                        *refpath = L'\0';
                }
            }

            /* in the wide character domain ...
             * refname now points at the resolved base name ...
             */

            if( *refname )
            {
                /* if it's not empty,
                 * then we transform the full normalised path back into
                 * the multibyte character domain, and skip over the dirname,
                 * to return the resolved basename.
                 */

                if( (len = wcstombs( path, refcopy, len )) != (size_t)(-1) )
                    path[ len ] = '\0';
                *refname = L'\0';
                if( (len = wcstombs( NULL, refcopy, 0 )) != (size_t)(-1) )
                    path += len;
            }

            else
            {
                /* the basename is empty, so return the default value of "/",
                 * transforming from wide char to multibyte char domain, and
                 * returning it in our own buffer.
                 */

                retfail = (char*)realloc( retfail, len = 1 + wcstombs( NULL, L"/", 0 ));
                wcstombs( path = retfail, L"/", len );
            }

            /* restore the caller's locale, clean up, and return the result */

            setlocale( LC_CTYPE, locale );
            free( locale );
            return( path );
        }

        /* or we had an empty residual path name, after the drive designator,
         * in which case we simply fall through ...
         */
    }

    /* and, if we get to here ...
     * the path name is either NULL, or it decomposes to an empty string;
     * in either case, we return the default value of "." in our own buffer,
     * reloading it with the correct value, transformed from the wide char
     * to the multibyte char domain, just in case the caller trashed it
     * after a previous call.
     */

    retfail = (char*)realloc( retfail, len = 1 + wcstombs( NULL, L".", 0 ));
    wcstombs( retfail, L".", len );

    /* restore the caller's locale, clean up, and return the result */

    setlocale( LC_CTYPE, locale );
    free( locale );
    return( retfail );
}

#ifdef __cplusplus
}
#endif

/* $RCSfile: basename.c,v $$Revision: 1.2 $: end of file */
