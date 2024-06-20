/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#ifndef NVTX_IMPL_GUARD_OPENCL
#error Never include this file directly -- it is automatically included by nvToolsExtCuda.h (except when NVTX_NO_IMPL is defined).
#endif


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef void (NVTX_API * nvtxNameClDeviceA_impl_fntype)(cl_device_id device, const char* name);
typedef void (NVTX_API * nvtxNameClDeviceW_impl_fntype)(cl_device_id device, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClContextA_impl_fntype)(cl_context context, const char* name);
typedef void (NVTX_API * nvtxNameClContextW_impl_fntype)(cl_context context, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClCommandQueueA_impl_fntype)(cl_command_queue command_queue, const char* name);
typedef void (NVTX_API * nvtxNameClCommandQueueW_impl_fntype)(cl_command_queue command_queue, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClMemObjectA_impl_fntype)(cl_mem memobj, const char* name);
typedef void (NVTX_API * nvtxNameClMemObjectW_impl_fntype)(cl_mem memobj, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClSamplerA_impl_fntype)(cl_sampler sampler, const char* name);
typedef void (NVTX_API * nvtxNameClSamplerW_impl_fntype)(cl_sampler sampler, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClProgramA_impl_fntype)(cl_program program, const char* name);
typedef void (NVTX_API * nvtxNameClProgramW_impl_fntype)(cl_program program, const wchar_t* name);
typedef void (NVTX_API * nvtxNameClEventA_impl_fntype)(cl_event evnt, const char* name);
typedef void (NVTX_API * nvtxNameClEventW_impl_fntype)(cl_event evnt, const wchar_t* name);

NVTX_DECLSPEC void NVTX_API nvtxNameClDeviceA(cl_device_id device, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClDeviceA_impl_fntype local = (nvtxNameClDeviceA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceA_impl_fnptr;
    if(local!=0)
        (*local)(device, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClDeviceW(cl_device_id device, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClDeviceW_impl_fntype local = (nvtxNameClDeviceW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClDeviceW_impl_fnptr;
    if(local!=0)
        (*local)(device, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClContextA(cl_context context, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClContextA_impl_fntype local = (nvtxNameClContextA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextA_impl_fnptr;
    if(local!=0)
        (*local)(context, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClContextW(cl_context context, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClContextW_impl_fntype local = (nvtxNameClContextW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClContextW_impl_fnptr;
    if(local!=0)
        (*local)(context, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClCommandQueueA(cl_command_queue command_queue, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClCommandQueueA_impl_fntype local = (nvtxNameClCommandQueueA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueA_impl_fnptr;
    if(local!=0)
        (*local)(command_queue, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClCommandQueueW(cl_command_queue command_queue, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClCommandQueueW_impl_fntype local = (nvtxNameClCommandQueueW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClCommandQueueW_impl_fnptr;
    if(local!=0)
        (*local)(command_queue, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClMemObjectA(cl_mem memobj, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClMemObjectA_impl_fntype local = (nvtxNameClMemObjectA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectA_impl_fnptr;
    if(local!=0)
        (*local)(memobj, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClMemObjectW(cl_mem memobj, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClMemObjectW_impl_fntype local = (nvtxNameClMemObjectW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClMemObjectW_impl_fnptr;
    if(local!=0)
        (*local)(memobj, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClSamplerA(cl_sampler sampler, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClSamplerA_impl_fntype local = (nvtxNameClSamplerA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerA_impl_fnptr;
    if(local!=0)
        (*local)(sampler, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClSamplerW(cl_sampler sampler, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClSamplerW_impl_fntype local = (nvtxNameClSamplerW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClSamplerW_impl_fnptr;
    if(local!=0)
        (*local)(sampler, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClProgramA(cl_program program, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClProgramA_impl_fntype local = (nvtxNameClProgramA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramA_impl_fnptr;
    if(local!=0)
        (*local)(program, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClProgramW(cl_program program, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClProgramW_impl_fntype local = (nvtxNameClProgramW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClProgramW_impl_fnptr;
    if(local!=0)
        (*local)(program, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClEventA(cl_event evnt, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClEventA_impl_fntype local = (nvtxNameClEventA_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventA_impl_fnptr;
    if(local!=0)
        (*local)(evnt, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameClEventW(cl_event evnt, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameClEventW_impl_fntype local = (nvtxNameClEventW_impl_fntype)NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameClEventW_impl_fnptr;
    if(local!=0)
        (*local)(evnt, name);
#endif /*NVTX_DISABLE*/
}

#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */
