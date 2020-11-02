/**
 * @file dirname.c
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
 * Provides an implementation of the "dirname" function, conforming
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

JL_DLLEXPORT char *dirname( char *path )
{
    size_t len;
    static JL_THREAD_LOCAL char *retfail = NULL;

    /* to handle path names for files in multibyte character locales,
     * we need to set up LC_CTYPE to match the host file system locale.
     */

    char *locale = setlocale( LC_CTYPE, NULL );
    if( locale != NULL ) locale = strdup( locale );
    setlocale( LC_CTYPE, "" );

    if( path && *path )
    {
        /* allocate sufficient local storage space,
         * in which to create a wide character reference copy of path
         */

        wchar_t* refcopy = (wchar_t*)alloca((1 + (len = mbstowcs(NULL, path, 0)))*sizeof(wchar_t));

        /* create the wide character reference copy of path */

        wchar_t *refpath = refcopy;
        len = mbstowcs( refpath, path, len );
        refcopy[ len ] = L'\0';

        /* SUSv3 identifies a special case, where path is exactly equal to "//";
         * (we will also accept "\\" in the Win32 context, but not "/\" or "\/",
         *  and neither will we consider paths with an initial drive designator).
         * For this special case, SUSv3 allows the implementation to choose to
         * return "/" or "//", (or "\" or "\\", since this is Win32); we will
         * simply return the path unchanged, (i.e. "//" or "\\").
         */

        if( (len > 1) && ((refpath[0] == L'/') || (refpath[0] == L'\\')) )
        {
            if( (refpath[1] == refpath[0]) && (refpath[2] == L'\0') )
            {
                setlocale( LC_CTYPE, locale );
                free( locale );
                return path;
            }
        }

        /* For all other cases ...
         * step over the drive designator, if present ...
         */

        else if( (len > 1) && (refpath[1] == L':') )
        {
            /* FIXME: maybe should confirm *refpath is a valid drive designator */

            refpath += 2;
        }

        /* check again, just to ensure we still have a non-empty path name ... */

        if( *refpath )
        {
            /* reproduce the scanning logic of the "basename" function
             * to locate the basename component of the current path string,
             * (but also remember where the dirname component starts).
             */

            wchar_t *refname, *basename;
            for( refname = basename = refpath ; *refpath ; ++refpath )
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

                        basename = refpath;

                    else

                        /* we struck an early termination of the path string,
                         * with trailing dir separators following the base name,
                         * so break out of the for loop, to avoid overrun.
                         */

                        break;
                }
            }

            /* now check,
             * to confirm that we have distinct dirname and basename components
             */

            if( basename > refname )
            {
                /* and, when we do ...
                 * backtrack over all trailing separators on the dirname component,
                 * (but preserve exactly two initial dirname separators, if identical),
                 * and add a NUL terminator in their place.
                 */

                do --basename;
                while( (basename > refname) && ((*basename == L'/') || (*basename == L'\\')) );
                if( (basename == refname) && ((refname[0] == L'/') || (refname[0] == L'\\'))
                    &&  (refname[1] == refname[0]) && (refname[2] != L'/') && (refname[2] != L'\\') )
                    ++basename;
                *++basename = L'\0';

                /* if the resultant dirname begins with EXACTLY two dir separators,
                 * AND both are identical, then we preserve them.
                 */

                refpath = refcopy;
                while( ((*refpath == L'/') || (*refpath == L'\\')) )
                    ++refpath;
                if( ((refpath - refcopy) > 2) || (refcopy[1] != refcopy[0]) )
                    refpath = refcopy;

                /* and finally ...
                 * we remove any residual, redundantly duplicated separators from the dirname,
                 * reterminate, and return it.
                 */

                refname = refpath;
                while( *refpath )
                {
                    if( ((*refname++ = *refpath) == L'/') || (*refpath++ == L'\\') )
                    {
                        while( (*refpath == L'/') || (*refpath == L'\\') )
                            ++refpath;
                    }
                }
                *refname = L'\0';

                /* finally ...
                 * transform the resolved dirname back into the multibyte char domain,
                 * restore the caller's locale, and return the resultant dirname
                 */

                if( (len = wcstombs( path, refcopy, len )) != (size_t)(-1) )
                    path[ len ] = '\0';
            }

            else
            {
                /* either there were no dirname separators in the path name,
                 * or there was nothing else ...
                 */

                if( (*refname == L'/') || (*refname == L'\\') )
                {
                    /* it was all separators, so return one */

                    ++refname;
                }

                else
                {
                    /* there were no separators, so return '.' */

                    *refname++ = L'.';
                }

                /* add a NUL terminator, in either case,
                 * then transform to the multibyte char domain,
                 * using our own buffer
                 */

                *refname = L'\0';
                retfail = (char*)realloc( retfail, len = 1 + wcstombs( NULL, refcopy, 0 ));
                wcstombs( path = retfail, refcopy, len );
            }

            /* restore caller's locale, clean up, and return the resolved dirname */

            setlocale( LC_CTYPE, locale );
            free( locale );
            return path;
        }
    }

    /* path is NULL, or an empty string; default return value is "." ...
     * return this in our own buffer, regenerated by wide char transform,
     * in case the caller trashed it after a previous call.
     */

    retfail = (char*)realloc( retfail, len = 1 + wcstombs( NULL, L".", 0 ));
    wcstombs( retfail, L".", len );

    /* restore caller's locale, clean up, and return the default dirname */

    setlocale( LC_CTYPE, locale );
    free( locale );
    return retfail;
}

#ifdef __cplusplus
}
#endif

/* $RCSfile: dirname.c,v $$Revision: 1.2 $: end of file */
