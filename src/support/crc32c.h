#ifndef CRC32C_H
#define CRC32C_H 1

#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void jl_crc32c_init(int force_sw);
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const void *buf, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* CRC32C_H */
