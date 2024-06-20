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

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkEx_impl_init)(const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxMarkEx(eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkA_impl_init)(const char* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxMarkA(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxMarkW_impl_init)(const wchar_t* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxMarkW(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartEx_impl_init)(const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangeStartEx(eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartA_impl_init)(const char* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangeStartA(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartW_impl_init)(const wchar_t* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangeStartW(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangeEnd_impl_init)(nvtxRangeId_t id){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxRangeEnd(id);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushEx_impl_init)(const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangePushEx(eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushA_impl_init)(const char* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangePushA(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePushW_impl_init)(const wchar_t* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangePushW(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxRangePop_impl_init)(void){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxRangePop();
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryA_impl_init)(uint32_t category, const char* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxNameCategoryA(category, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryW_impl_init)(uint32_t category, const wchar_t* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxNameCategoryW(category, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadA_impl_init)(uint32_t threadId, const char* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxNameOsThreadA(threadId, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadW_impl_init)(uint32_t threadId, const wchar_t* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxNameOsThreadW(threadId, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainMarkEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainMarkEx(domain, eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxRangeId_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeStartEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainRangeStartEx(domain, eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeEnd_impl_init)(nvtxDomainHandle_t domain, nvtxRangeId_t id){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainRangeEnd(domain, id);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePushEx_impl_init)(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainRangePushEx(domain, eventAttrib);
}

NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePop_impl_init)(nvtxDomainHandle_t domain){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainRangePop(domain);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxResourceHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceCreate_impl_init)(nvtxDomainHandle_t domain, nvtxResourceAttributes_t* attribs){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainResourceCreate(domain, attribs);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceDestroy_impl_init)(nvtxResourceHandle_t resource){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainResourceDestroy(resource);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryA_impl_init)(nvtxDomainHandle_t domain, uint32_t category, const char* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainNameCategoryA(domain, category, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryW_impl_init)(nvtxDomainHandle_t domain, uint32_t category, const wchar_t* name){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainNameCategoryW(domain, category, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxStringHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringA_impl_init)(nvtxDomainHandle_t domain, const char* string){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainRegisterStringA(domain, string);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxStringHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringW_impl_init)(nvtxDomainHandle_t domain, const wchar_t* string){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainRegisterStringW(domain, string);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxDomainHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateA_impl_init)(const char* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainCreateA(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxDomainHandle_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateW_impl_init)(const wchar_t* message){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    return nvtxDomainCreateW(message);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainDestroy_impl_init)(nvtxDomainHandle_t domain){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxDomainDestroy(domain);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxInitialize_impl_init)(const void* reserved){
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    nvtxInitialize(reserved);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceA_impl_init)(nvtx_CUdevice device, const char* name){
    nvtxNameCuDeviceA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceA_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceW_impl_init)(nvtx_CUdevice device, const wchar_t* name){
    nvtxNameCuDeviceW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceW_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextA_impl_init)(nvtx_CUcontext context, const char* name){
    nvtxNameCuContextA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextA_impl_fnptr;
    if (local)
        local(context, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextW_impl_init)(nvtx_CUcontext context, const wchar_t* name){
    nvtxNameCuContextW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextW_impl_fnptr;
    if (local)
        local(context, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamA_impl_init)(nvtx_CUstream stream, const char* name){
    nvtxNameCuStreamA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamA_impl_fnptr;
    if (local)
        local(stream, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamW_impl_init)(nvtx_CUstream stream, const wchar_t* name){
    nvtxNameCuStreamW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamW_impl_fnptr;
    if (local)
        local(stream, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventA_impl_init)(nvtx_CUevent event, const char* name){
    nvtxNameCuEventA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventA_impl_fnptr;
    if (local)
        local(event, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventW_impl_init)(nvtx_CUevent event, const wchar_t* name){
    nvtxNameCuEventW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventW_impl_fnptr;
    if (local)
        local(event, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceA_impl_init)(int device, const char* name){
    nvtxNameCudaDeviceA_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceA_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceW_impl_init)(int device, const wchar_t* name){
    nvtxNameCudaDeviceW_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceW_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamA_impl_init)(nvtx_cudaStream_t stream, const char* name){
    nvtxNameCudaStreamA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamA_impl_fnptr;
    if (local)
        local(stream, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamW_impl_init)(nvtx_cudaStream_t stream, const wchar_t* name){
    nvtxNameCudaStreamW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamW_impl_fnptr;
    if (local)
        local(stream, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventA_impl_init)(nvtx_cudaEvent_t event, const char* name){
    nvtxNameCudaEventA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventA_impl_fnptr;
    if (local)
        local(event, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventW_impl_init)(nvtx_cudaEvent_t event, const wchar_t* name){
    nvtxNameCudaEventW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventW_impl_fnptr;
    if (local)
        local(event, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceA_impl_init)(nvtx_cl_device_id device, const char* name){
    nvtxNameClDeviceA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceA_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceW_impl_init)(nvtx_cl_device_id device, const wchar_t* name){
    nvtxNameClDeviceW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceW_impl_fnptr;
    if (local)
        local(device, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextA_impl_init)(nvtx_cl_context context, const char* name){
    nvtxNameClContextA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextA_impl_fnptr;
    if (local)
        local(context, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextW_impl_init)(nvtx_cl_context context, const wchar_t* name){
    nvtxNameClContextW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextW_impl_fnptr;
    if (local)
        local(context, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueA_impl_init)(nvtx_cl_command_queue command_queue, const char* name){
    nvtxNameClCommandQueueA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueA_impl_fnptr;
    if (local)
        local(command_queue, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueW_impl_init)(nvtx_cl_command_queue command_queue, const wchar_t* name){
    nvtxNameClCommandQueueW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueW_impl_fnptr;
    if (local)
        local(command_queue, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectA_impl_init)(nvtx_cl_mem memobj, const char* name){
    nvtxNameClMemObjectA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectA_impl_fnptr;
    if (local)
        local(memobj, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectW_impl_init)(nvtx_cl_mem memobj, const wchar_t* name){
    nvtxNameClMemObjectW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectW_impl_fnptr;
    if (local)
        local(memobj, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerA_impl_init)(nvtx_cl_sampler sampler, const char* name){
    nvtxNameClSamplerA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerA_impl_fnptr;
    if (local)
        local(sampler, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerW_impl_init)(nvtx_cl_sampler sampler, const wchar_t* name){
    nvtxNameClSamplerW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerW_impl_fnptr;
    if (local)
        local(sampler, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramA_impl_init)(nvtx_cl_program program, const char* name){
    nvtxNameClProgramA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramA_impl_fnptr;
    if (local)
        local(program, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramW_impl_init)(nvtx_cl_program program, const wchar_t* name){
    nvtxNameClProgramW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramW_impl_fnptr;
    if (local)
        local(program, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventA_impl_init)(nvtx_cl_event evnt, const char* name){
    nvtxNameClEventA_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventA_impl_fnptr;
    if (local)
        local(evnt, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventW_impl_init)(nvtx_cl_event evnt, const wchar_t* name){
    nvtxNameClEventW_fakeimpl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventW_impl_fnptr;
    if (local)
        local(evnt, name);
}

NVTX_LINKONCE_DEFINE_FUNCTION nvtxSyncUser_t NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserCreate_impl_init)(nvtxDomainHandle_t domain, const nvtxSyncUserAttributes_t* attribs){
    nvtxDomainSyncUserCreate_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserCreate_impl_fnptr;
    if (local) {
        return local(domain, attribs);
    }
    return (nvtxSyncUser_t)0;
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserDestroy_impl_init)(nvtxSyncUser_t handle){
    nvtxDomainSyncUserDestroy_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserDestroy_impl_fnptr;
    if (local)
        local(handle);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireStart_impl_init)(nvtxSyncUser_t handle){
    nvtxDomainSyncUserAcquireStart_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireStart_impl_fnptr;
    if (local)
        local(handle);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireFailed_impl_init)(nvtxSyncUser_t handle){
    nvtxDomainSyncUserAcquireFailed_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireFailed_impl_fnptr;
    if (local)
        local(handle);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireSuccess_impl_init)(nvtxSyncUser_t handle){
    nvtxDomainSyncUserAcquireSuccess_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireSuccess_impl_fnptr;
    if (local)
        local(handle);
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_API NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserReleasing_impl_init)(nvtxSyncUser_t handle){
    nvtxDomainSyncUserReleasing_impl_fntype local;
    NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)();
    local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserReleasing_impl_fnptr;
    if (local)
        local(handle);
}

NVTX_LINKONCE_FWDDECL_FUNCTION void NVTX_VERSIONED_IDENTIFIER(nvtxSetInitFunctionsToNoops)(int forceAllToNoops);
NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_VERSIONED_IDENTIFIER(nvtxSetInitFunctionsToNoops)(int forceAllToNoops)
{
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxMarkEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxMarkA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxMarkW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangeStartW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeEnd_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangeEnd_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeEnd_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangePushEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangePushA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangePushW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePop_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxRangePop_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePop_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCategoryW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameOsThreadW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadW_impl_fnptr = NULL;

    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuDeviceW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuDeviceW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuContextW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuContextW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuStreamW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuStreamW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCuEventW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCuEventW_impl_fnptr = NULL;

    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClDeviceW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClContextW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClCommandQueueW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClMemObjectW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClSamplerW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClProgramW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameClEventW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventW_impl_fnptr = NULL;

    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaDeviceW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaDeviceW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaStreamW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaStreamW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxNameCudaEventW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCudaEventW_impl_fnptr = NULL;

    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainMarkEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainMarkEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainMarkEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeStartEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeStartEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeStartEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeEnd_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangeEnd_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeEnd_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePushEx_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePushEx_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePushEx_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePop_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRangePop_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePop_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceCreate_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceCreate_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceCreate_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceDestroy_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainResourceDestroy_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceDestroy_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainNameCategoryW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainRegisterStringW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateA_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateA_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateA_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateW_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainCreateW_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateW_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainDestroy_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainDestroy_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainDestroy_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxInitialize_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxInitialize_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxInitialize_impl_fnptr = NULL;

    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserCreate_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserCreate_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserCreate_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserDestroy_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserDestroy_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserDestroy_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireStart_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireStart_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireStart_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireFailed_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireFailed_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireFailed_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireSuccess_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserAcquireSuccess_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserAcquireSuccess_impl_fnptr = NULL;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserReleasing_impl_fnptr == NVTX_VERSIONED_IDENTIFIER(nvtxDomainSyncUserReleasing_impl_init) || forceAllToNoops)
        NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainSyncUserReleasing_impl_fnptr = NULL;
}
