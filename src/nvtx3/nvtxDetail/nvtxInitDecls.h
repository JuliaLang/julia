/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#ifndef NVTX_IMPL_GUARD
#error Never include this file directly -- it is automatically included by nvToolsExt.h (except when NVTX_NO_IMPL is defined).
#endif

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkEx_impl_init)(const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkA_impl_init)(const char* message);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkW_impl_init)(const wchar_t* message);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartEx_impl_init)(const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartA_impl_init)(const char* message);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartW_impl_init)(const wchar_t* message);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeEnd_impl_init)(nvtxRangeId_t id);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushEx_impl_init)(const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushA_impl_init)(const char* message);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushW_impl_init)(const wchar_t* message);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePop_impl_init)(void);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryA_impl_init)(uint32_t category, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryW_impl_init)(uint32_t category, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadA_impl_init)(uint32_t threadId, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadW_impl_init)(uint32_t threadId, const wchar_t* name);

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceA_impl_init)(nvtx_CUdevice device, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceW_impl_init)(nvtx_CUdevice device, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextA_impl_init)(nvtx_CUcontext context, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextW_impl_init)(nvtx_CUcontext context, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamA_impl_init)(nvtx_CUstream stream, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamW_impl_init)(nvtx_CUstream stream, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventA_impl_init)(nvtx_CUevent event, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventW_impl_init)(nvtx_CUevent event, const wchar_t* name);

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceA_impl_init)(nvtx_cl_device_id device, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceW_impl_init)(nvtx_cl_device_id device, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextA_impl_init)(nvtx_cl_context context, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextW_impl_init)(nvtx_cl_context context, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueA_impl_init)(nvtx_cl_command_queue command_queue, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueW_impl_init)(nvtx_cl_command_queue command_queue, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectA_impl_init)(nvtx_cl_mem memobj, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectW_impl_init)(nvtx_cl_mem memobj, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerA_impl_init)(nvtx_cl_sampler sampler, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerW_impl_init)(nvtx_cl_sampler sampler, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramA_impl_init)(nvtx_cl_program program, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramW_impl_init)(nvtx_cl_program program, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventA_impl_init)(nvtx_cl_event evnt, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventW_impl_init)(nvtx_cl_event evnt, const wchar_t* name);

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceA_impl_init)(int device, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceW_impl_init)(int device, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamA_impl_init)(nvtx_cudaStream_t stream, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamW_impl_init)(nvtx_cudaStream_t stream, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventA_impl_init)(nvtx_cudaEvent_t event, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventW_impl_init)(nvtx_cudaEvent_t event, const wchar_t* name);

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainMarkEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeStartEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeEnd_impl_init)(nvtxDomainHandle_t domain, nvtxRangeId_t id);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePushEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePop_impl_init)(nvtxDomainHandle_t domain);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxResourceHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceCreate_impl_init)(nvtxDomainHandle_t domain, nvtxResourceAttributes_t* attribs);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceDestroy_impl_init)(nvtxResourceHandle_t resource);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryA_impl_init)(nvtxDomainHandle_t domain, uint32_t category, const char* name);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryW_impl_init)(nvtxDomainHandle_t domain, uint32_t category, const wchar_t* name);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxStringHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringA_impl_init)(nvtxDomainHandle_t domain, const char* string);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxStringHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringW_impl_init)(nvtxDomainHandle_t domain, const wchar_t* string);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxDomainHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateA_impl_init)(const char* message);
NVTX_LINKONCE_FWDDECL_FUNCTION nvtxDomainHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateW_impl_init)(const wchar_t* message);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainDestroy_impl_init)(nvtxDomainHandle_t domain);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxInitialize_impl_init)(const void* reserved);

NVTX_LINKONCE_FWDDECL_FUNCTION nvtxSyncUser_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserCreate_impl_init)(nvtxDomainHandle_t domain, const nvtxSyncUserAttributes_t* attribs);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserDestroy_impl_init)(nvtxSyncUser_t handle);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireStart_impl_init)(nvtxSyncUser_t handle);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireFailed_impl_init)(nvtxSyncUser_t handle);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireSuccess_impl_init)(nvtxSyncUser_t handle);
NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserReleasing_impl_init)(nvtxSyncUser_t handle);
