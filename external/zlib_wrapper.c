#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

typedef struct {
	char *s;
	unsigned long sz;
} zlib_pack;

extern zlib_pack *_jl_zlib_deflate(char *source)
{
    unsigned long compress_bound = compressBound(strlen(source));
    char *compress_buffer = (char *)malloc(compress_bound);
	char *final_buffer = NULL;
	zlib_pack *pack = NULL;
    memset(compress_buffer, '\0', compress_bound);

    int ret = compress2(compress_buffer, &compress_bound, source, strlen(source),  6);
    
	if(ret == Z_OK) {
        final_buffer = (char *)malloc(compress_bound);
        memcpy(final_buffer, compress_buffer, compress_bound);
        final_buffer[compress_bound] = '\0';

		pack = (zlib_pack *)malloc(sizeof(zlib_pack));
		pack->s = final_buffer;
		pack->sz = compress_bound;
    }

    free(compress_buffer);    
    return pack;
}

extern zlib_pack *_jl_zlib_inflate(zlib_pack *pack)
{
	char *source = pack->s;
	unsigned long source_bytes = pack->sz;
	zlib_pack *opack = NULL;
	
	size_t dest_bytes = source_bytes*2;
    char *temp_buffer = (char *)malloc(dest_bytes); // guess how much we need
	char *final_buffer = NULL;
    memset(temp_buffer, '\0', dest_bytes);
	
    int ret = uncompress(temp_buffer, &dest_bytes, source, source_bytes); 
	// Keep dynamically 
	while(ret == Z_BUF_ERROR) {
        dest_bytes = dest_bytes * 2;
        temp_buffer = (char *)realloc(temp_buffer, dest_bytes);
        uncompress(temp_buffer, &dest_bytes, source, source_bytes);
	}
	
	if(ret == Z_OK) {
	    final_buffer = (char *)malloc(dest_bytes);
		memcpy(final_buffer, temp_buffer, dest_bytes);
	    final_buffer[dest_bytes] = '\0';
	
		opack = (zlib_pack *)malloc(sizeof(zlib_pack));
		opack->s = final_buffer;
		opack->sz = dest_bytes;
	}
	
	free(temp_buffer);
	
	return opack;
}

extern char *_jl_zlib_print_pack(zlib_pack *pack) {
	char *pp;
	asprintf(&pp, "%s", pack->s);
	return pp;
}

extern void _jl_zlib_free_pack(zlib_pack *pack) {
  free(pack->s);
  free(pack);
}