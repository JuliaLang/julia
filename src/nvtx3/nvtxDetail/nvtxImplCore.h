/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

NVTX_DECLSPEC void NVTX_API nvtxMarkEx(const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxMarkEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkEx_impl_fnptr;
    if(local!=0)
        (*local)(eventAttrib);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxMarkA(const char* message)
{
#ifndef NVTX_DISABLE
    nvtxMarkA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkA_impl_fnptr;
    if(local!=0)
        (*local)(message);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxMarkW(const wchar_t* message)
{
#ifndef NVTX_DISABLE
    nvtxMarkW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxMarkW_impl_fnptr;
    if(local!=0)
        (*local)(message);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartEx(const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxRangeStartEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartEx_impl_fnptr;
    if(local!=0)
        return (*local)(eventAttrib);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxRangeId_t)0;
}

NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartA(const char* message)
{
#ifndef NVTX_DISABLE
    nvtxRangeStartA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartA_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxRangeId_t)0;
}

NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartW(const wchar_t* message)
{
#ifndef NVTX_DISABLE
    nvtxRangeStartW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeStartW_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxRangeId_t)0;
}

NVTX_DECLSPEC void NVTX_API nvtxRangeEnd(nvtxRangeId_t id)
{
#ifndef NVTX_DISABLE
    nvtxRangeEnd_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangeEnd_impl_fnptr;
    if(local!=0)
        (*local)(id);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC int NVTX_API nvtxRangePushEx(const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxRangePushEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushEx_impl_fnptr;
    if(local!=0)
        return (*local)(eventAttrib);
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC int NVTX_API nvtxRangePushA(const char* message)
{
#ifndef NVTX_DISABLE
    nvtxRangePushA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushA_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC int NVTX_API nvtxRangePushW(const wchar_t* message)
{
#ifndef NVTX_DISABLE
    nvtxRangePushW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePushW_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC int NVTX_API nvtxRangePop(void)
{
#ifndef NVTX_DISABLE
    nvtxRangePop_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxRangePop_impl_fnptr;
    if(local!=0)
        return (*local)();
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC void NVTX_API nvtxNameCategoryA(uint32_t category, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCategoryA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryA_impl_fnptr;
    if(local!=0)
        (*local)(category, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameCategoryW(uint32_t category, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameCategoryW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameCategoryW_impl_fnptr;
    if(local!=0)
        (*local)(category, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameOsThreadA(uint32_t threadId, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxNameOsThreadA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadA_impl_fnptr;
    if(local!=0)
        (*local)(threadId, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxNameOsThreadW(uint32_t threadId, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxNameOsThreadW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxNameOsThreadW_impl_fnptr;
    if(local!=0)
        (*local)(threadId, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxDomainMarkEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxDomainMarkEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainMarkEx_impl_fnptr;
    if(local!=0)
        (*local)(domain, eventAttrib);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxDomainRangeStartEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxDomainRangeStartEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeStartEx_impl_fnptr;
    if(local!=0)
        return (*local)(domain, eventAttrib);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxRangeId_t)0;
}

NVTX_DECLSPEC void NVTX_API nvtxDomainRangeEnd(nvtxDomainHandle_t domain, nvtxRangeId_t id)
{
#ifndef NVTX_DISABLE
    nvtxDomainRangeEnd_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangeEnd_impl_fnptr;
    if(local!=0)
        (*local)(domain, id);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC int NVTX_API nvtxDomainRangePushEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib)
{
#ifndef NVTX_DISABLE
    nvtxDomainRangePushEx_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePushEx_impl_fnptr;
    if(local!=0)
        return (*local)(domain, eventAttrib);
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC int NVTX_API nvtxDomainRangePop(nvtxDomainHandle_t domain)
{
#ifndef NVTX_DISABLE
    nvtxDomainRangePop_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRangePop_impl_fnptr;
    if(local!=0)
        return (*local)(domain);
    else
#endif  /*NVTX_DISABLE*/
        return (int)NVTX_NO_PUSH_POP_TRACKING;
}

NVTX_DECLSPEC nvtxResourceHandle_t NVTX_API nvtxDomainResourceCreate(nvtxDomainHandle_t domain, nvtxResourceAttributes_t* attribs)
{
#ifndef NVTX_DISABLE
    nvtxDomainResourceCreate_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceCreate_impl_fnptr;
    if(local!=0)
        return (*local)(domain, attribs);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxResourceHandle_t)0;
}

NVTX_DECLSPEC void NVTX_API nvtxDomainResourceDestroy(nvtxResourceHandle_t resource)
{
#ifndef NVTX_DISABLE
    nvtxDomainResourceDestroy_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainResourceDestroy_impl_fnptr;
    if(local!=0)
        (*local)(resource);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxDomainNameCategoryA(nvtxDomainHandle_t domain, uint32_t category, const char* name)
{
#ifndef NVTX_DISABLE
    nvtxDomainNameCategoryA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryA_impl_fnptr;
    if(local!=0)
        (*local)(domain, category, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxDomainNameCategoryW(nvtxDomainHandle_t domain, uint32_t category, const wchar_t* name)
{
#ifndef NVTX_DISABLE
    nvtxDomainNameCategoryW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainNameCategoryW_impl_fnptr;
    if(local!=0)
        (*local)(domain, category, name);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC nvtxStringHandle_t NVTX_API nvtxDomainRegisterStringA(nvtxDomainHandle_t domain, const char* string)
{
#ifndef NVTX_DISABLE
    nvtxDomainRegisterStringA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringA_impl_fnptr;
    if(local!=0)
        return (*local)(domain, string);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxStringHandle_t)0;
}

NVTX_DECLSPEC nvtxStringHandle_t NVTX_API nvtxDomainRegisterStringW(nvtxDomainHandle_t domain, const wchar_t* string)
{
#ifndef NVTX_DISABLE
    nvtxDomainRegisterStringW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainRegisterStringW_impl_fnptr;
    if(local!=0)
        return (*local)(domain, string);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxStringHandle_t)0;
}

NVTX_DECLSPEC nvtxDomainHandle_t NVTX_API nvtxDomainCreateA(const char* message)
{
#ifndef NVTX_DISABLE
    nvtxDomainCreateA_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateA_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxDomainHandle_t)0;
}

NVTX_DECLSPEC nvtxDomainHandle_t NVTX_API nvtxDomainCreateW(const wchar_t* message)
{
#ifndef NVTX_DISABLE
    nvtxDomainCreateW_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainCreateW_impl_fnptr;
    if(local!=0)
        return (*local)(message);
    else
#endif  /*NVTX_DISABLE*/
        return (nvtxDomainHandle_t)0;
}

NVTX_DECLSPEC void NVTX_API nvtxDomainDestroy(nvtxDomainHandle_t domain)
{
#ifndef NVTX_DISABLE
    nvtxDomainDestroy_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxDomainDestroy_impl_fnptr;
    if(local!=0)
        (*local)(domain);
#endif /*NVTX_DISABLE*/
}

NVTX_DECLSPEC void NVTX_API nvtxInitialize(const void* reserved)
{
#ifndef NVTX_DISABLE
    nvtxInitialize_impl_fntype local = NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).nvtxInitialize_impl_fnptr;
    if(local!=0)
        (*local)(reserved);
#endif /*NVTX_DISABLE*/
}
