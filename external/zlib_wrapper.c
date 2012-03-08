#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

extern int _jl_zlib_deflate(char *source, unsigned long *output_bytes, char **dest)
{
    unsigned long compress_bound = compressBound(strlen(source));
    char *compress_buffer = (char *)malloc(compress_bound);
    memset(compress_buffer, '\0', compress_bound);
    *output_bytes = compress_bound; 
    
    int ret = compress2(compress_buffer, &compress_bound, source, strlen(source),  6);
    if(ret == Z_OK) {
        *output_bytes = compress_bound;
        *dest = (char *)malloc(*output_bytes);
        memcpy(*dest, compress_buffer, *output_bytes);
        *dest[*output_bytes] = '\0';
    }

    free(compress_buffer);    
    return ret;
}

extern int _jl_zlib_inflate(char *source, unsigned long source_bytes, char **dest)
{
	size_t dest_bytes = source_bytes*2;
    char *temp_buffer = (char *)malloc(dest_bytes); // guess how much we need
    memset(temp_buffer, '\0', dest_bytes);
	
    int ret = uncompress(temp_buffer, &dest_bytes, source, source_bytes); 
	// Keep dynamically 
	while(ret == Z_BUF_ERROR) {
        dest_bytes = dest_bytes * 2;
        temp_buffer = (char *)realloc(temp_buffer, dest_bytes);
        uncompress(temp_buffer, &dest_bytes, source, source_bytes);
	}
	if(ret == Z_OK) {
       	printf("Dest Bytes: %lu", dest_bytes);
	    *dest = (char *)malloc(dest_bytes);
		memcpy(*dest, temp_buffer, dest_bytes);
	    *dest[dest_bytes] = '\0';
	}
	
	free(temp_buffer);
    return ret;
}

extern void _jl_zlib_free(char *str) {
  free(str);
}

/* report a zlib or i/o error */
extern char *zerr(int ret)
{
    switch (ret) {
    case Z_OK:
      return("");
      break;
    case Z_ERRNO:
        if (ferror(stdin))
            return("error reading stdin");
        if (ferror(stdout))
            return("error writing stdout");
        break;
    case Z_STREAM_ERROR:
        return("invalid compression level");
        break;
    case Z_DATA_ERROR:
        return("invalid or incomplete deflate data");
        break;
    case Z_MEM_ERROR:
        return("out of memory");
        break;
    case Z_VERSION_ERROR:
        return("zlib version mismatch!");
    }
}

int main(int argc, char *argv[]) {
  return 0;
}