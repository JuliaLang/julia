/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#include "nvToolsExt.h"

#include <CL/cl.h>

#ifndef NVTOOLSEXT_OPENCL_V3
#define NVTOOLSEXT_OPENCL_V3

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ========================================================================= */
/** \name Functions for OpenCL Resource Naming
 */
/** \addtogroup RESOURCE_NAMING
 * \section RESOURCE_NAMING_OPENCL OpenCL Resource Naming
 *
 * This section covers the API functions that allow to annotate OpenCL resources
 * with user-provided names.
 *
 * @{
 */

/*  ------------------------------------------------------------------------- */
/* \cond SHOW_HIDDEN 
* \brief Used to build a non-colliding value for resource types separated class
* \version \NVTX_VERSION_2
*/
#define NVTX_RESOURCE_CLASS_OPENCL 6 
/** \endcond */

/*  ------------------------------------------------------------------------- */
/** \brief Resource types for OpenCL
*/
typedef enum nvtxResourceOpenCLType_t
{
    NVTX_RESOURCE_TYPE_OPENCL_DEVICE = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 1),
    NVTX_RESOURCE_TYPE_OPENCL_CONTEXT = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 2),
    NVTX_RESOURCE_TYPE_OPENCL_COMMANDQUEUE = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 3),
    NVTX_RESOURCE_TYPE_OPENCL_MEMOBJECT = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 4),
    NVTX_RESOURCE_TYPE_OPENCL_SAMPLER = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 5),
    NVTX_RESOURCE_TYPE_OPENCL_PROGRAM = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 6),
    NVTX_RESOURCE_TYPE_OPENCL_EVENT = NVTX_RESOURCE_MAKE_TYPE(OPENCL, 7),
} nvtxResourceOpenCLType_t;


/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL device.
 *
 * Allows to associate an OpenCL device with a user-provided name.
 *
 * \param device - The handle of the OpenCL device to name.
 * \param name   - The name of the OpenCL device.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClDeviceA(cl_device_id device, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClDeviceW(cl_device_id device, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL context.
 *
 * Allows to associate an OpenCL context with a user-provided name.
 *
 * \param context - The handle of the OpenCL context to name.
 * \param name    - The name of the OpenCL context.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClContextA(cl_context context, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClContextW(cl_context context, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL command queue.
 *
 * Allows to associate an OpenCL command queue with a user-provided name.
 *
 * \param command_queue - The handle of the OpenCL command queue to name.
 * \param name          - The name of the OpenCL command queue.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClCommandQueueA(cl_command_queue command_queue, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClCommandQueueW(cl_command_queue command_queue, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL memory object.
 *
 * Allows to associate an OpenCL memory object with a user-provided name.
 *
 * \param memobj - The handle of the OpenCL memory object to name.
 * \param name   - The name of the OpenCL memory object.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClMemObjectA(cl_mem memobj, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClMemObjectW(cl_mem memobj, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL sampler.
 *
 * Allows to associate an OpenCL sampler with a user-provided name.
 *
 * \param sampler - The handle of the OpenCL sampler to name.
 * \param name    - The name of the OpenCL sampler.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClSamplerA(cl_sampler sampler, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClSamplerW(cl_sampler sampler, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL program.
 *
 * Allows to associate an OpenCL program with a user-provided name.
 *
 * \param program - The handle of the OpenCL program to name.
 * \param name    - The name of the OpenCL program.
 *
 * \code
 * cpProgram = clCreateProgramWithSource(cxGPUContext, 1,
 *     (const char **) &cSourceCL, &program_length, &ciErrNum);
 * shrCheckErrorEX(ciErrNum, CL_SUCCESS, pCleanup);
 * nvtxNameClProgram(cpProgram, L"PROGRAM_NAME");
 * \endcode
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClProgramA(cl_program program, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClProgramW(cl_program program, const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Annotates an OpenCL event.
 *
 * Allows to associate an OpenCL event with a user-provided name.
 *
 * \param evnt - The handle of the OpenCL event to name.
 * \param name - The name of the OpenCL event.
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameClEventA(cl_event evnt, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameClEventW(cl_event evnt, const wchar_t* name);
/** @} */

/** @} */ /* END RESOURCE_NAMING */

/* ========================================================================= */
#ifdef UNICODE
  #define nvtxNameClDevice        nvtxNameClDeviceW
  #define nvtxNameClContext       nvtxNameClContextW
  #define nvtxNameClCommandQueue  nvtxNameClCommandQueueW
  #define nvtxNameClMemObject     nvtxNameClMemObjectW
  #define nvtxNameClSampler       nvtxNameClSamplerW
  #define nvtxNameClProgram       nvtxNameClProgramW
  #define nvtxNameClEvent         nvtxNameClEventW
#else
  #define nvtxNameClDevice        nvtxNameClDeviceA
  #define nvtxNameClContext       nvtxNameClContextA
  #define nvtxNameClCommandQueue  nvtxNameClCommandQueueA
  #define nvtxNameClMemObject     nvtxNameClMemObjectA
  #define nvtxNameClSampler       nvtxNameClSamplerA
  #define nvtxNameClProgram       nvtxNameClProgramA
  #define nvtxNameClEvent         nvtxNameClEventA
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#ifndef NVTX_NO_IMPL
#define NVTX_IMPL_GUARD_OPENCL /* Ensure other headers cannot included directly */
#include "nvtxDetail/nvtxImplOpenCL_v3.h"
#undef NVTX_IMPL_GUARD_OPENCL
#endif /*NVTX_NO_IMPL*/

#endif /* NVTOOLSEXT_OPENCL_V3 */
