/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#ifndef NVTX_IMPL_GUARD_CUDART
#error Never include this file directly -- it is automatically included by nvToolsExtCudaRt.h (except when NVTX_NO_IMPL is defined).
#endif

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef void (NVTX_API * nvtxNameCudaDeviceA_impl_fntype)(int device, const char* name);
typedef void (NVTX_API * nvtxNameCudaDeviceW_impl_fntype)(int device, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCudaStreamA_impl_fntype)(cudaStream_t stream, const char* name);
typedef void (NVTX_API * nvtxNameCudaStreamW_impl_fntype)(cudaStream_t stream, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCudaEventA_impl_fntype)(cudaEvent_t event, const char* name);
typedef void (NVTX_API * nvtxNameCudaEventW_impl_fntype)(cudaEvent_t event, const wchar_t* name);

NVTX_DECLSPEC void NVTX_API nvtxNameCudaDeviceA(int device, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaDeviceA_impl_fntype local = (nvtxNameCudaDeviceA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceA_impl_fnptr;
    if(local!=0)
        (*local)(device, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCudaDeviceW(int device, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaDeviceW_impl_fntype local = (nvtxNameCudaDeviceW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceW_impl_fnptr;
    if(local!=0)
        (*local)(device, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCudaStreamA(cudaStream_t stream, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaStreamA_impl_fntype local = (nvtxNameCudaStreamA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamA_impl_fnptr;
    if(local!=0)
        (*local)(stream, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCudaStreamW(cudaStream_t stream, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaStreamW_impl_fntype local = (nvtxNameCudaStreamW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamW_impl_fnptr;
    if(local!=0)
        (*local)(stream, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCudaEventA(cudaEvent_t event, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaEventA_impl_fntype local = (nvtxNameCudaEventA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventA_impl_fnptr;
    if(local!=0)
        (*local)(event, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCudaEventW(cudaEvent_t event, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCudaEventW_impl_fntype local = (nvtxNameCudaEventW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventW_impl_fnptr;
    if(local!=0)
        (*local)(event, name);
#endif /*NVTX_DISABLE*/
}

#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */

