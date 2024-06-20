/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

/* This header defines types which are used by the internal implementation
*  of NVTX and callback subscribers.  API clients do not use these types,
*  so they are defined here instead of in nvToolsExt.h to clarify they are
*  not part of the NVTX client API. */

#ifndef NVTX_IMPL_GUARD
#error Never include this file directly -- it is automatically included by nvToolsExt.h.
#endif

/* ------ Dependency-free types binary-compatible with real types ------- */

/* In order to avoid having the NVTX core API headers depend on non-NVTX
*  headers like cuda.h, NVTX defines binary-compatible types to use for
*  safely making the initialization versions of all NVTX functions without
*  needing to have definitions for the real types. */

typedef int   nvtx_CUdevice;
typedef void* nvtx_CUcontext;
typedef void* nvtx_CUstream;
typedef void* nvtx_CUevent;

typedef void* nvtx_cudaStream_t;
typedef void* nvtx_cudaEvent_t;

typedef void* nvtx_cl_platform_id;
typedef void* nvtx_cl_device_id;
typedef void* nvtx_cl_context;
typedef void* nvtx_cl_command_queue;
typedef void* nvtx_cl_mem;
typedef void* nvtx_cl_program;
typedef void* nvtx_cl_kernel;
typedef void* nvtx_cl_event;
typedef void* nvtx_cl_sampler;

typedef struct nvtxSyncUser* nvtxSyncUser_t;
struct nvtxSyncUserAttributes_v0;
typedef struct nvtxSyncUserAttributes_v0 nvtxSyncUserAttributes_t;

/* --------- Types for function pointers (with fake API types) ---------- */

typedef void (NVTX_API * nvtxMarkEx_impl_fntype)(const nvtxEventAttributes_t* eventAttrib);
typedef void (NVTX_API * nvtxMarkA_impl_fntype)(const char* message);
typedef void (NVTX_API * nvtxMarkW_impl_fntype)(const wchar_t* message);
typedef nvtxRangeId_t (NVTX_API * nvtxRangeStartEx_impl_fntype)(const nvtxEventAttributes_t* eventAttrib);
typedef nvtxRangeId_t (NVTX_API * nvtxRangeStartA_impl_fntype)(const char* message);
typedef nvtxRangeId_t (NVTX_API * nvtxRangeStartW_impl_fntype)(const wchar_t* message);
typedef void (NVTX_API * nvtxRangeEnd_impl_fntype)(nvtxRangeId_t id);
typedef int (NVTX_API * nvtxRangePushEx_impl_fntype)(const nvtxEventAttributes_t* eventAttrib);
typedef int (NVTX_API * nvtxRangePushA_impl_fntype)(const char* message);
typedef int (NVTX_API * nvtxRangePushW_impl_fntype)(const wchar_t* message);
typedef int (NVTX_API * nvtxRangePop_impl_fntype)(void);
typedef void (NVTX_API * nvtxNameCategoryA_impl_fntype)(uint32_t category, const char* name);
typedef void (NVTX_API * nvtxNameCategoryW_impl_fntype)(uint32_t category, const wchar_t* name);
typedef void (NVTX_API * nvtxNameOsThreadA_impl_fntype)(uint32_t threadId, const char* name);
typedef void (NVTX_API * nvtxNameOsThreadW_impl_fntype)(uint32_t threadId, const wchar_t* name);

/* Real impl types are defined in nvtxImplCuda_v3.h, where CUDA headers are included */
typedef void (NVTX_API * nvtxNameCuDeviceA_fakeimpl_fntype)(nvtx_CUdevice device, const char* name);
typedef void (NVTX_API * nvtxNameCuDeviceW_fakeimpl_fntype)(nvtx_CUdevice device, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCuContextA_fakeimpl_fntype)(nvtx_CUcontext context, const char* name);
typedef void (NVTX_API * nvtxNameCuContextW_fakeimpl_fntype)(nvtx_CUcontext context, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCuStreamA_fakeimpl_fntype)(nvtx_CUstream stream, const char* name);
typedef void (NVTX_API * nvtxNameCuStreamW_fakeimpl_fntype)(nvtx_CUstream stream, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCuEventA_fakeimpl_fntype)(nvtx_CUevent event, const char* name);
typedef void (NVTX_API * nvtxNameCuEventW_fakeimpl_fntype)(nvtx_CUevent event, const wchar_t* name);

/* Real impl types are defined in nvtxImplOpenCL_v3.h, where OPENCL headers are included */
typedef void (NVTX_API * nvtxNameClDeviceA_fakeimpl_fntype)(nvtx_cl_device_id device, const char* name);
typedef void (NVTX_API * nvtxNameClDeviceW_fakeimpl_fntype)(nvtx_cl_device_id device, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClContextA_fakeimpl_fntype)(nvtx_cl_context context, const char* name);
typedef void (NVTX_API * nvtxNameClContextW_fakeimpl_fntype)(nvtx_cl_context context, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClCommandQueueA_fakeimpl_fntype)(nvtx_cl_command_queue command_queue, const char* name);
typedef void (NVTX_API * nvtxNameClCommandQueueW_fakeimpl_fntype)(nvtx_cl_command_queue command_queue, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClMemObjectA_fakeimpl_fntype)(nvtx_cl_mem memobj, const char* name);
typedef void (NVTX_API * nvtxNameClMemObjectW_fakeimpl_fntype)(nvtx_cl_mem memobj, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClSamplerA_fakeimpl_fntype)(nvtx_cl_sampler sampler, const char* name);
typedef void (NVTX_API * nvtxNameClSamplerW_fakeimpl_fntype)(nvtx_cl_sampler sampler, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClProgramA_fakeimpl_fntype)(nvtx_cl_program program, const char* name);
typedef void (NVTX_API * nvtxNameClProgramW_fakeimpl_fntype)(nvtx_cl_program program, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClEventA_fakeimpl_fntype)(nvtx_cl_event evnt, const char* name);
typedef void (NVTX_API * nvtxNameClEventW_fakeimpl_fntype)(nvtx_cl_event evnt, const wchar_t* name);

/* Real impl types are defined in nvtxImplCudaRt_v3.h, where CUDART headers are included */
typedef void (NVTX_API * nvtxNameCudaDeviceA_impl_fntype)(int device, const char* name);
typedef void (NVTX_API * nvtxNameCudaDeviceW_impl_fntype)(int device, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCudaStreamA_fakeimpl_fntype)(nvtx_cudaStream_t stream, const char* name);
typedef void (NVTX_API * nvtxNameCudaStreamW_fakeimpl_fntype)(nvtx_cudaStream_t stream, const wchar_t* name);
typedef void (NVTX_API * nvtxNameCudaEventA_fakeimpl_fntype)(nvtx_cudaEvent_t event, const char* name);
typedef void (NVTX_API * nvtxNameCudaEventW_fakeimpl_fntype)(nvtx_cudaEvent_t event, const wchar_t* name);

typedef void (NVTX_API * nvtxDomainMarkEx_impl_fntype)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
typedef nvtxRangeId_t (NVTX_API * nvtxDomainRangeStartEx_impl_fntype)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
typedef void (NVTX_API * nvtxDomainRangeEnd_impl_fntype)(nvtxDomainHandle_t domain, nvtxRangeId_t id);
typedef int (NVTX_API * nvtxDomainRangePushEx_impl_fntype)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
typedef int (NVTX_API * nvtxDomainRangePop_impl_fntype)(nvtxDomainHandle_t domain);
typedef nvtxResourceHandle_t (NVTX_API * nvtxDomainResourceCreate_impl_fntype)(nvtxDomainHandle_t domain, nvtxResourceAttributes_t* attribs);
typedef void (NVTX_API * nvtxDomainResourceDestroy_impl_fntype)(nvtxResourceHandle_t resource);
typedef void (NVTX_API * nvtxDomainNameCategoryA_impl_fntype)(nvtxDomainHandle_t domain, uint32_t category, const char* name);
typedef void (NVTX_API * nvtxDomainNameCategoryW_impl_fntype)(nvtxDomainHandle_t domain, uint32_t category, const wchar_t* name);
typedef nvtxStringHandle_t (NVTX_API * nvtxDomainRegisterStringA_impl_fntype)(nvtxDomainHandle_t domain, const char* string);
typedef nvtxStringHandle_t (NVTX_API * nvtxDomainRegisterStringW_impl_fntype)(nvtxDomainHandle_t domain, const wchar_t* string);
typedef nvtxDomainHandle_t (NVTX_API * nvtxDomainCreateA_impl_fntype)(const char* message);
typedef nvtxDomainHandle_t (NVTX_API * nvtxDomainCreateW_impl_fntype)(const wchar_t* message);
typedef void (NVTX_API * nvtxDomainDestroy_impl_fntype)(nvtxDomainHandle_t domain);
typedef void (NVTX_API * nvtxInitialize_impl_fntype)(const void* reserved);

typedef nvtxSyncUser_t (NVTX_API * nvtxDomainSyncUserCreate_impl_fntype)(nvtxDomainHandle_t domain, const nvtxSyncUserAttributes_t* attribs);
typedef void (NVTX_API * nvtxDomainSyncUserDestroy_impl_fntype)(nvtxSyncUser_t handle);
typedef void (NVTX_API * nvtxDomainSyncUserAcquireStart_impl_fntype)(nvtxSyncUser_t handle);
typedef void (NVTX_API * nvtxDomainSyncUserAcquireFailed_impl_fntype)(nvtxSyncUser_t handle);
typedef void (NVTX_API * nvtxDomainSyncUserAcquireSuccess_impl_fntype)(nvtxSyncUser_t handle);
typedef void (NVTX_API * nvtxDomainSyncUserReleasing_impl_fntype)(nvtxSyncUser_t handle);

/* ---------------- Types for callback subscription --------------------- */

typedef const void *(NVTX_API * NvtxGetExportTableFunc_t)(uint32_t exportTableId);
typedef int (NVTX_API * NvtxInitializeInjectionNvtxFunc_t)(NvtxGetExportTableFunc_t exportTable);

typedef enum NvtxCallbackModule
{
    NVTX_CB_MODULE_INVALID                 = 0,
    NVTX_CB_MODULE_CORE                    = 1,
    NVTX_CB_MODULE_CUDA                    = 2,
    NVTX_CB_MODULE_OPENCL                  = 3,
    NVTX_CB_MODULE_CUDART                  = 4,
    NVTX_CB_MODULE_CORE2                   = 5,
    NVTX_CB_MODULE_SYNC                    = 6,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CB_MODULE_SIZE,
    NVTX_CB_MODULE_FORCE_INT               = 0x7fffffff
} NvtxCallbackModule;

typedef enum NvtxCallbackIdCore
{
    NVTX_CBID_CORE_INVALID                 =  0,
    NVTX_CBID_CORE_MarkEx                  =  1,
    NVTX_CBID_CORE_MarkA                   =  2,
    NVTX_CBID_CORE_MarkW                   =  3,
    NVTX_CBID_CORE_RangeStartEx            =  4,
    NVTX_CBID_CORE_RangeStartA             =  5,
    NVTX_CBID_CORE_RangeStartW             =  6,
    NVTX_CBID_CORE_RangeEnd                =  7,
    NVTX_CBID_CORE_RangePushEx             =  8,
    NVTX_CBID_CORE_RangePushA              =  9,
    NVTX_CBID_CORE_RangePushW              = 10,
    NVTX_CBID_CORE_RangePop                = 11,
    NVTX_CBID_CORE_NameCategoryA           = 12,
    NVTX_CBID_CORE_NameCategoryW           = 13,
    NVTX_CBID_CORE_NameOsThreadA           = 14,
    NVTX_CBID_CORE_NameOsThreadW           = 15,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_CORE_SIZE,
    NVTX_CBID_CORE_FORCE_INT = 0x7fffffff
} NvtxCallbackIdCore;

typedef enum NvtxCallbackIdCore2
{
    NVTX_CBID_CORE2_INVALID                 = 0,
    NVTX_CBID_CORE2_DomainMarkEx            = 1,
    NVTX_CBID_CORE2_DomainRangeStartEx      = 2,
    NVTX_CBID_CORE2_DomainRangeEnd          = 3,
    NVTX_CBID_CORE2_DomainRangePushEx       = 4,
    NVTX_CBID_CORE2_DomainRangePop          = 5,
    NVTX_CBID_CORE2_DomainResourceCreate    = 6,
    NVTX_CBID_CORE2_DomainResourceDestroy   = 7,
    NVTX_CBID_CORE2_DomainNameCategoryA     = 8,
    NVTX_CBID_CORE2_DomainNameCategoryW     = 9,
    NVTX_CBID_CORE2_DomainRegisterStringA   = 10,
    NVTX_CBID_CORE2_DomainRegisterStringW   = 11,
    NVTX_CBID_CORE2_DomainCreateA           = 12,
    NVTX_CBID_CORE2_DomainCreateW           = 13,
    NVTX_CBID_CORE2_DomainDestroy           = 14,
    NVTX_CBID_CORE2_Initialize              = 15,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_CORE2_SIZE,
    NVTX_CBID_CORE2_FORCE_INT               = 0x7fffffff
} NvtxCallbackIdCore2;

typedef enum NvtxCallbackIdCuda
{
    NVTX_CBID_CUDA_INVALID                 =  0,
    NVTX_CBID_CUDA_NameCuDeviceA           =  1,
    NVTX_CBID_CUDA_NameCuDeviceW           =  2,
    NVTX_CBID_CUDA_NameCuContextA          =  3,
    NVTX_CBID_CUDA_NameCuContextW          =  4,
    NVTX_CBID_CUDA_NameCuStreamA           =  5,
    NVTX_CBID_CUDA_NameCuStreamW           =  6,
    NVTX_CBID_CUDA_NameCuEventA            =  7,
    NVTX_CBID_CUDA_NameCuEventW            =  8,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_CUDA_SIZE,
    NVTX_CBID_CUDA_FORCE_INT               = 0x7fffffff
} NvtxCallbackIdCuda;

typedef enum NvtxCallbackIdCudaRt
{
    NVTX_CBID_CUDART_INVALID               =  0,
    NVTX_CBID_CUDART_NameCudaDeviceA       =  1,
    NVTX_CBID_CUDART_NameCudaDeviceW       =  2,
    NVTX_CBID_CUDART_NameCudaStreamA       =  3,
    NVTX_CBID_CUDART_NameCudaStreamW       =  4,
    NVTX_CBID_CUDART_NameCudaEventA        =  5,
    NVTX_CBID_CUDART_NameCudaEventW        =  6,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_CUDART_SIZE,
    NVTX_CBID_CUDART_FORCE_INT             = 0x7fffffff
} NvtxCallbackIdCudaRt;

typedef enum NvtxCallbackIdOpenCL
{
    NVTX_CBID_OPENCL_INVALID               =  0,
    NVTX_CBID_OPENCL_NameClDeviceA         =  1,
    NVTX_CBID_OPENCL_NameClDeviceW         =  2,
    NVTX_CBID_OPENCL_NameClContextA        =  3,
    NVTX_CBID_OPENCL_NameClContextW        =  4,
    NVTX_CBID_OPENCL_NameClCommandQueueA   =  5,
    NVTX_CBID_OPENCL_NameClCommandQueueW   =  6,
    NVTX_CBID_OPENCL_NameClMemObjectA      =  7,
    NVTX_CBID_OPENCL_NameClMemObjectW      =  8,
    NVTX_CBID_OPENCL_NameClSamplerA        =  9,
    NVTX_CBID_OPENCL_NameClSamplerW        = 10,
    NVTX_CBID_OPENCL_NameClProgramA        = 11,
    NVTX_CBID_OPENCL_NameClProgramW        = 12,
    NVTX_CBID_OPENCL_NameClEventA          = 13,
    NVTX_CBID_OPENCL_NameClEventW          = 14,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_OPENCL_SIZE,
    NVTX_CBID_OPENCL_FORCE_INT             = 0x7fffffff
} NvtxCallbackIdOpenCL;

typedef enum NvtxCallbackIdSync
{
    NVTX_CBID_SYNC_INVALID                      = 0,
    NVTX_CBID_SYNC_DomainSyncUserCreate         = 1,
    NVTX_CBID_SYNC_DomainSyncUserDestroy        = 2,
    NVTX_CBID_SYNC_DomainSyncUserAcquireStart   = 3,
    NVTX_CBID_SYNC_DomainSyncUserAcquireFailed  = 4,
    NVTX_CBID_SYNC_DomainSyncUserAcquireSuccess = 5,
    NVTX_CBID_SYNC_DomainSyncUserReleasing      = 6,
    /* --- New constants must only be added directly above this line --- */
    NVTX_CBID_SYNC_SIZE,
    NVTX_CBID_SYNC_FORCE_INT                    = 0x7fffffff
} NvtxCallbackIdSync;

/* IDs for NVTX Export Tables */
typedef enum NvtxExportTableID
{
    NVTX_ETID_INVALID                      = 0,
    NVTX_ETID_CALLBACKS                    = 1,
    NVTX_ETID_RESERVED0                    = 2,
    NVTX_ETID_VERSIONINFO                  = 3,
    /* --- New constants must only be added directly above this line --- */
    NVTX_ETID_SIZE,
    NVTX_ETID_FORCE_INT                    = 0x7fffffff
} NvtxExportTableID;

typedef void (* NvtxFunctionPointer)(void); /* generic uncallable function pointer, must be casted to appropriate function type */
typedef NvtxFunctionPointer** NvtxFunctionTable; /* double pointer because array(1) of pointers(2) to function pointers */

typedef struct NvtxExportTableCallbacks
{
    size_t struct_size;

    /* returns an array of pointer to function pointers*/
    int (NVTX_API *GetModuleFunctionTable)(
        NvtxCallbackModule module,
        NvtxFunctionTable* out_table,
        unsigned int* out_size);
} NvtxExportTableCallbacks;

typedef struct NvtxExportTableVersionInfo
{
    /* sizeof(NvtxExportTableVersionInfo) */
    size_t struct_size;

    /* The API version comes from the NVTX library linked to the app.  The
    * injection library is can use this info to make some assumptions */
    uint32_t version;

    /* Reserved for alignment, do not use */
    uint32_t reserved0;

    /* This must be set by tools when attaching to provide applications
    *  the ability to, in emergency situations, detect problematic tools
    *  versions and modify the NVTX source to prevent attaching anything
    *  that causes trouble in the app.  Currently, this value is ignored. */
    void (NVTX_API *SetInjectionNvtxVersion)(
        uint32_t version);
} NvtxExportTableVersionInfo;







