// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main( int argc, char ** argv ) {
    if( argc < 5 ) {
        printf("Usage:\n");
        printf("  %s <hex offset> <string to write> <maxlen> <file>\n", argv[0] );
        return -1;
    }

    unsigned long offset = strtoul(argv[1], NULL, 16);
    char * replacement = argv[2];
    unsigned long maxlen = strtoul(argv[3], NULL, 10);

    FILE * f = fopen( argv[4], "r+" );
    if( !f ) {
        printf( "ERROR: Could not open %s for writing!\n", argv[4] );
        return -1;
    }

    if( strlen(replacement) > maxlen ) {
        printf( "ERROR: Replacement string length (%lu) is greater than maxlen! (%lu)\n", strlen(replacement), maxlen );
        return -1;
    }

    fseek( f, offset, SEEK_SET );
    fwrite( replacement, strlen(replacement)+1, 1, f );

    fclose( f );
    return 0;
}
