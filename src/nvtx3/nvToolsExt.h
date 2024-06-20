/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

/** \file nvToolsExt.h
 */

/* ========================================================================= */
/** \mainpage
 * \tableofcontents
 * \section INTRODUCTION Introduction
 *
 * The NVIDIA Tools Extension library is a set of functions that a
 * developer can use to provide additional information to tools.
 * The additional information is used by the tool to improve
 * analysis and visualization of data.
 *
 * The library introduces close to zero overhead if no tool is
 * attached to the application.  The overhead when a tool is
 * attached is specific to the tool.
 *
 * \section INITIALIZATION_SECTION Initialization
 *
 * Typically the tool's library that plugs into NVTX is indirectly 
 * loaded via enviromental properties that are platform specific. 
 * For some platform or special cases, the user may be required 
 * to instead explicity initialize instead though.   This can also
 * be helpful to control when the API loads a tool's library instead
 * of what would typically be the first function call to emit info.
 * For these rare case, see \ref INITIALIZATION for additional information.
 *
 * \section MARKERS_AND_RANGES Markers and Ranges
 *
 * Markers and ranges are used to describe events at a specific time (markers)
 * or over a time span (ranges) during the execution of the application
 * respectively. 
 *
 * \subsection MARKERS Markers
 * 
 * Markers denote specific moments in time.
 * 
 * 
 * See \ref DOMAINS and \ref EVENT_ATTRIBUTES for additional information on
 * how to specify the domain.
 * 
 * \subsection THREAD_RANGES Thread Ranges
 *
 * Thread ranges denote nested time ranges. Nesting is maintained per thread
 * per domain and does not require any additional correlation mechanism. The
 * duration of a thread range is defined by the corresponding pair of
 * nvtxRangePush* to nvtxRangePop API calls.
 *
 * See \ref DOMAINS and \ref EVENT_ATTRIBUTES for additional information on
 * how to specify the domain.
 *
 * \subsection PROCESS_RANGES Process Ranges
 *
 * Process ranges denote a time span that can expose arbitrary concurrency, as 
 * opposed to thread ranges that only support nesting. In addition the range
 * start event can happen on a different thread than the end marker. For the 
 * correlation of a start/end pair an unique correlation ID is used that is
 * returned from the start API call and needs to be passed into the end API
 * call.
 *
 * \subsection EVENT_ATTRIBUTES Event Attributes
 *
 * \ref MARKERS_AND_RANGES can be annotated with various attributes to provide
 * additional information for an event or to guide the tool's visualization of
 * the data. Each of the attributes is optional and if left unused the
 * attributes fall back to a default value. The attributes include:
 * - color
 * - category
 *
 * To specify any attribute other than the text message, the \ref
 * EVENT_ATTRIBUTE_STRUCTURE "Event Attribute Structure" must be used.
 *
 * \section DOMAINS Domains
 *
 * Domains enable developers to scope annotations. By default all events and
 * annotations are in the default domain. Additional domains can be registered.
 * This allows developers to scope markers, ranges, and resources names to
 * avoid conflicts.
 *
 * The function ::nvtxDomainCreateA or ::nvtxDomainCreateW is used to create
 * a named domain.
 * 
 * Each domain maintains its own
 * - categories
 * - thread range stacks
 * - registered strings
 *
 * The function ::nvtxDomainDestroy marks the end of the domain. Destroying 
 * a domain unregisters and destroys all objects associated with it such as 
 * registered strings, resource objects, named categories, and started ranges. 
 *
 * \section RESOURCE_NAMING Resource Naming
 *
 * This section covers calls that allow to annotate objects with user-provided
 * names in order to allow for a better analysis of complex trace data. All of
 * the functions take the handle or the ID of the object to name and the name.
 * The functions can be called multiple times during the execution of an
 * application, however, in that case it is implementation dependent which
 * name will be reported by the tool.
 * 
 * \subsection CATEGORY_NAMING Category Naming
 *
 * Some function in this library support associating an integer category 
 * to enable filtering and sorting.  The category naming functions allow 
 * the application to associate a user friendly name with the integer 
 * category.  Support for domains have been added in NVTX_VERSION_2 to 
 * avoid collisions when domains are developed independantly. 
 *
 * \subsection RESOURCE_OBJECTS Resource Objects
 *
 * Resource objects are a generic mechanism for attaching data to an application 
 * resource.  The identifier field makes the association to a pointer or handle, 
 * while the type field helps provide deeper understanding of the identifier as 
 * well as enabling differentiation in cases where handles generated by different
 * APIs may collide.  The resource object may also have an associated message to
 * associate with the application resource, enabling further annotation of this 
 * object and how it is used.
 * 
 * The resource object was introduced in NVTX_VERSION_2 to supersede existing naming
 * functions and allow the application resource identified by those functions to be
 * associated to a domain.  The other naming functions are still supported for backward
 * compatibility but will be associated only to the default domain.
 *
 * \subsection RESOURCE_NAMING_OS Resource Naming
 * 
 * Some operating system resources creation APIs do not support providing a user friendly 
 * name, such as some OS thread creation APIs.  This API support resource naming though 
 * both through resource objects and functions following the pattern 
 * nvtxName[RESOURCE_TYPE][A|W](identifier, name).  Resource objects introduced in NVTX_VERSION 2 
 * supersede the other functions with a a more general method of assigning names to OS resources,
 * along with associating them to domains too.  The older nvtxName* functions are only associated 
 * with the default domain.
 * \section EXTENSIONS Optional Extensions
 * Optional extensions will either appear within the existing sections the extend or appear 
 * in the "Related Pages" when they introduce new concepts.
 */

 /**
 * Tools Extension API version
 */
#if defined(NVTX_VERSION) && NVTX_VERSION < 3
#error "Trying to #include NVTX version 3 in a source file where an older NVTX version has already been included.  If you are not directly using NVTX (the NVIDIA Tools Extension library), you are getting this error because libraries you are using have included different versions of NVTX.  Suggested solutions are: (1) reorder #includes so the newest NVTX version is included first, (2) avoid using the conflicting libraries in the same .c/.cpp file, or (3) update the library using the older NVTX version to use the newer version instead."
#endif

/* Header guard */
#if !defined(NVTX_VERSION)
#define NVTX_VERSION 3

#if defined(_MSC_VER)
#define NVTX_API __stdcall
#define NVTX_INLINE_STATIC __inline static
#else /*defined(__GNUC__)*/
#define NVTX_API
#define NVTX_INLINE_STATIC inline static
#endif /* Platform */

#if defined(NVTX_NO_IMPL)
/* When omitting implementation, avoid declaring functions inline */
/* without definitions, since this causes compiler warnings. */
#define NVTX_DECLSPEC
#elif defined(NVTX_EXPORT_API)
/* Allow overriding definition of NVTX_DECLSPEC when exporting API. */
/* Default is empty, meaning non-inline with external linkage. */
#if !defined(NVTX_DECLSPEC)
#define NVTX_DECLSPEC
#endif
#else
/* Normal NVTX usage defines the NVTX API inline with static */
/* (internal) linkage. */
#define NVTX_DECLSPEC NVTX_INLINE_STATIC
#endif

#include "nvtxDetail/nvtxLinkOnce.h"

#define NVTX_VERSIONED_IDENTIFIER_L3(NAME, VERSION) NAME##_v##VERSION
#define NVTX_VERSIONED_IDENTIFIER_L2(NAME, VERSION) NVTX_VERSIONED_IDENTIFIER_L3(NAME, VERSION)
#define NVTX_VERSIONED_IDENTIFIER(NAME) NVTX_VERSIONED_IDENTIFIER_L2(NAME, NVTX_VERSION)

/**
 * The nvToolsExt library depends on stdint.h.  If the build tool chain in use
 * does not include stdint.h then define NVTX_STDINT_TYPES_ALREADY_DEFINED
 * and define the following types:
 * <ul>
 *   <li>uint8_t
 *   <li>int8_t
 *   <li>uint16_t
 *   <li>int16_t
 *   <li>uint32_t
 *   <li>int32_t
 *   <li>uint64_t
 *   <li>int64_t
 *   <li>uintptr_t
 *   <li>intptr_t
 * </ul>
 * #define NVTX_STDINT_TYPES_ALREADY_DEFINED if you are using your own header file.
 */
#ifndef NVTX_STDINT_TYPES_ALREADY_DEFINED
#include <stdint.h>
#endif

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** 
* Result Codes
*/

#define NVTX_SUCCESS 0
#define NVTX_FAIL 1
#define NVTX_ERR_INIT_LOAD_PROPERTY 2
#define NVTX_ERR_INIT_ACCESS_LIBRARY 3
#define NVTX_ERR_INIT_LOAD_LIBRARY 4
#define NVTX_ERR_INIT_MISSING_LIBRARY_ENTRY_POINT 5
#define NVTX_ERR_INIT_FAILED_LIBRARY_ENTRY_POINT 6
#define NVTX_ERR_NO_INJECTION_LIBRARY_AVAILABLE 7

/**
 * Size of the nvtxEventAttributes_t structure.
 */
#define NVTX_EVENT_ATTRIB_STRUCT_SIZE ( (uint16_t)( sizeof(nvtxEventAttributes_t) ) )

#define NVTX_NO_PUSH_POP_TRACKING ((int)-2)

typedef uint64_t nvtxRangeId_t;

/* Forward declaration of opaque domain registration structure */
struct nvtxDomainRegistration_st;
typedef struct nvtxDomainRegistration_st nvtxDomainRegistration;

/* \brief Domain Handle Structure.
* \anchor DOMAIN_HANDLE_STRUCTURE
*
* This structure is opaque to the user and is used as a handle to reference
* a domain.  This type is returned from tools when using the NVTX API to
* create a domain.
*
*/
typedef nvtxDomainRegistration* nvtxDomainHandle_t;

/* Forward declaration of opaque string registration structure */
struct nvtxStringRegistration_st;
typedef struct nvtxStringRegistration_st nvtxStringRegistration;

/* \brief Registered String Handle Structure.
* \anchor REGISTERED_STRING_HANDLE_STRUCTURE
*
* This structure is opaque to the user and is used as a handle to reference
* a registered string.  This type is returned from tools when using the NVTX
* API to create a registered string.
*
*/
typedef nvtxStringRegistration* nvtxStringHandle_t;

/* ========================================================================= */
/** \defgroup GENERAL General
 * @{
 */

/** ---------------------------------------------------------------------------
 * Color Types
 * ------------------------------------------------------------------------- */
typedef enum nvtxColorType_t
{
    NVTX_COLOR_UNKNOWN  = 0,                 /**< Color attribute is unused. */
    NVTX_COLOR_ARGB     = 1                  /**< An ARGB color is provided. */
} nvtxColorType_t;

/** ---------------------------------------------------------------------------
 * Message Types
 * ------------------------------------------------------------------------- */
typedef enum nvtxMessageType_t
{
    NVTX_MESSAGE_UNKNOWN          = 0,    /**< Message payload is unused. */
    NVTX_MESSAGE_TYPE_ASCII       = 1,    /**< A character sequence is used as payload. */
    NVTX_MESSAGE_TYPE_UNICODE     = 2,     /**< A wide character sequence is used as payload. */
    /* NVTX_VERSION_2 */
    NVTX_MESSAGE_TYPE_REGISTERED  = 3,    /**< A unique string handle that was registered
                                                with \ref nvtxDomainRegisterStringA() or 
                                                \ref nvtxDomainRegisterStringW(). */
} nvtxMessageType_t;

typedef union nvtxMessageValue_t
{
    const char* ascii;
    const wchar_t* unicode;
    /* NVTX_VERSION_2 */
    nvtxStringHandle_t registered;
} nvtxMessageValue_t;


/** @} */ /*END defgroup*/
/* ------------------------------------------------------------------------- */
/** \brief Force initialization (optional)
*
* Force NVTX library to initialize.  The first call to any NVTX API function
* will automatically initialize the entire API.  This can make the first call
* much slower than subsequent calls.  In applications where the first call to
* NVTX may be in a performance-critical section, calling nvtxInitialize before
* any performance-critical sections will ensure NVTX initialization occurs at
* an acceptable time.  Since nvtxInitialize takes no parameters and has no
* expected behavior besides initialization, it is convenient to add a call to
* nvtxInitialize in NVTX-instrumented applications that need to force earlier
* initialization without changing any other code.  For example, if an app's
* first NVTX call is nvtxDomainCreate, and it is difficult to move that call
* earlier because the domain handle must be stored in an object only created
* at that point, adding a call to nvtxInitialize at the top of main() will
* ensure the later call to nvtxDomainCreate is as fast as possible.
*
* \version \NVTX_VERSION_3
*
* \param reserved - must be zero or NULL.
*
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxInitialize(const void* reserved);
/** @} */


/** @} */ /*END defgroup*/

/* ========================================================================= */
/** \defgroup EVENT_ATTRIBUTES Event Attributes
* @{
*/

/** ---------------------------------------------------------------------------
* Payload Types
* ------------------------------------------------------------------------- */
typedef enum nvtxPayloadType_t
{
    NVTX_PAYLOAD_UNKNOWN = 0,   /**< Color payload is unused. */
    NVTX_PAYLOAD_TYPE_UNSIGNED_INT64 = 1,   /**< A 64 bit unsigned integer value is used as payload. */
    NVTX_PAYLOAD_TYPE_INT64 = 2,   /**< A 64 bit signed integer value is used as payload. */
    NVTX_PAYLOAD_TYPE_DOUBLE = 3,   /**< A 64 bit floating point value is used as payload. */
    /* NVTX_VERSION_2 */
    NVTX_PAYLOAD_TYPE_UNSIGNED_INT32 = 4,   /**< A 32 bit floating point value is used as payload. */
    NVTX_PAYLOAD_TYPE_INT32 = 5,   /**< A 32 bit floating point value is used as payload. */
    NVTX_PAYLOAD_TYPE_FLOAT = 6    /**< A 32 bit floating point value is used as payload. */
} nvtxPayloadType_t;

/** \brief Event Attribute Structure.
 * \anchor EVENT_ATTRIBUTE_STRUCTURE
 *
 * This structure is used to describe the attributes of an event. The layout of
 * the structure is defined by a specific version of the tools extension
 * library and can change between different versions of the Tools Extension
 * library.
 *
 * \par Initializing the Attributes
 *
 * The caller should always perform the following three tasks when using
 * attributes:
 * <ul>
 *    <li>Zero the structure
 *    <li>Set the version field
 *    <li>Set the size field
 * </ul>
 *
 * Zeroing the structure sets all the event attributes types and values
 * to the default value.
 *
 * The version and size field are used by the Tools Extension
 * implementation to handle multiple versions of the attributes structure.
 *
 * It is recommended that the caller use one of the following to methods
 * to initialize the event attributes structure:
 *
 * \par Method 1: Initializing nvtxEventAttributes for future compatibility
 * \code
 * nvtxEventAttributes_t eventAttrib = {0};
 * eventAttrib.version = NVTX_VERSION;
 * eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
 * \endcode
 *
 * \par Method 2: Initializing nvtxEventAttributes for a specific version
 * \code
 * nvtxEventAttributes_t eventAttrib = {0};
 * eventAttrib.version = 1;
 * eventAttrib.size = (uint16_t)(sizeof(nvtxEventAttributes_v1));
 * \endcode
 *
 * If the caller uses Method 1 it is critical that the entire binary
 * layout of the structure be configured to 0 so that all fields
 * are initialized to the default value.
 *
 * The caller should either use both NVTX_VERSION and
 * NVTX_EVENT_ATTRIB_STRUCT_SIZE (Method 1) or use explicit values
 * and a versioned type (Method 2).  Using a mix of the two methods
 * will likely cause either source level incompatibility or binary
 * incompatibility in the future.
 *
 * \par Settings Attribute Types and Values
 *
 *
 * \par Example:
 * \code
 * // Initialize
 * nvtxEventAttributes_t eventAttrib = {0};
 * eventAttrib.version = NVTX_VERSION;
 * eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
 *
 * // Configure the Attributes
 * eventAttrib.colorType = NVTX_COLOR_ARGB;
 * eventAttrib.color = 0xFF880000;
 * eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
 * eventAttrib.message.ascii = "Example";
 * \endcode
 *
 * In the example the caller does not have to set the value of
 * \ref ::nvtxEventAttributes_v2::category or
 * \ref ::nvtxEventAttributes_v2::payload as these fields were set to
 * the default value by {0}.
 * \sa
 * ::nvtxDomainMarkEx
 * ::nvtxDomainRangeStartEx
 * ::nvtxDomainRangePushEx
 */
typedef struct nvtxEventAttributes_v2
{
    /**
    * \brief Version flag of the structure.
    *
    * Needs to be set to NVTX_VERSION to indicate the version of NVTX APIs
    * supported in this header file. This can optionally be overridden to
    * another version of the tools extension library.
    */
    uint16_t version;

    /**
    * \brief Size of the structure.
    *
    * Needs to be set to the size in bytes of the event attribute
    * structure used to specify the event.
    */
    uint16_t size;

    /**
     * \brief ID of the category the event is assigned to.
     *
     * A category is a user-controlled ID that can be used to group
     * events.  The tool may use category IDs to improve filtering or
     * enable grouping of events in the same category. The functions
     * \ref ::nvtxNameCategoryA or \ref ::nvtxNameCategoryW can be used
     * to name a category.
     *
     * Default Value is 0
     */
    uint32_t category;

    /** \brief Color type specified in this attribute structure.
     *
     * Defines the color format of the attribute structure's \ref COLOR_FIELD
     * "color" field.
     *
     * Default Value is NVTX_COLOR_UNKNOWN
     */
    int32_t colorType;              /* nvtxColorType_t */

    /** \brief Color assigned to this event. \anchor COLOR_FIELD
     *
     * The color that the tool should use to visualize the event.
     */
    uint32_t color;

    /**
     * \brief Payload type specified in this attribute structure.
     *
     * Defines the payload format of the attribute structure's \ref PAYLOAD_FIELD
     * "payload" field.
     *
     * Default Value is NVTX_PAYLOAD_UNKNOWN
     */
    int32_t payloadType;            /* nvtxPayloadType_t */

    int32_t reserved0;

    /**
     * \brief Payload assigned to this event. \anchor PAYLOAD_FIELD
     *
     * A numerical value that can be used to annotate an event. The tool could
     * use the payload data to reconstruct graphs and diagrams.
     */
    union payload_t
    {
        uint64_t ullValue;
        int64_t llValue;
        double dValue;
        /* NVTX_VERSION_2 */
        uint32_t uiValue;
        int32_t iValue;
        float fValue;
    } payload;

    /** \brief Message type specified in this attribute structure.
     *
     * Defines the message format of the attribute structure's \ref MESSAGE_FIELD
     * "message" field.
     *
     * Default Value is NVTX_MESSAGE_UNKNOWN
     */
    int32_t messageType;            /* nvtxMessageType_t */

    /** \brief Message assigned to this attribute structure. \anchor MESSAGE_FIELD
     *
     * The text message that is attached to an event.
     */
    nvtxMessageValue_t message;

} nvtxEventAttributes_v2;

typedef struct nvtxEventAttributes_v2 nvtxEventAttributes_t;

/** @} */ /*END defgroup*/
/* ========================================================================= */
/** \defgroup MARKERS_AND_RANGES Markers and Ranges
 *
 * See \ref MARKERS_AND_RANGES for more details
 *
 * @{
 */

/** \name Marker */

/* ------------------------------------------------------------------------- */
/** \brief Marks an instantaneous event in the application.
*
* A marker can contain a text message or specify additional information
* using the event attributes structure.  These attributes include a text
* message, color, category, and a payload. Each of the attributes is optional
* and can only be sent out using the \ref nvtxDomainMarkEx function.
*
* nvtxDomainMarkEx(NULL, event) is equivalent to calling
* nvtxMarkEx(event).
*
* \param domain    - The domain of scoping the category.
* \param eventAttrib - The event attribute structure defining the marker's
* attribute types and attribute values.
*
* \sa
* ::nvtxMarkEx
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxDomainMarkEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Marks an instantaneous event in the application.
 *
 * A marker can contain a text message or specify additional information
 * using the event attributes structure.  These attributes include a text
 * message, color, category, and a payload. Each of the attributes is optional
 * and can only be sent out using the \ref nvtxMarkEx function.
 * If \ref nvtxMarkA or \ref nvtxMarkW are used to specify the marker
 * or if an attribute is unspecified then a default value will be used.
 *
 * \param eventAttrib - The event attribute structure defining the marker's
 * attribute types and attribute values.
 *
 * \par Example:
 * \code
 * // zero the structure
 * nvtxEventAttributes_t eventAttrib = {0};
 * // set the version and the size information
 * eventAttrib.version = NVTX_VERSION;
 * eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
 * // configure the attributes.  0 is the default for all attributes.
 * eventAttrib.colorType = NVTX_COLOR_ARGB;
 * eventAttrib.color = 0xFF880000;
 * eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
 * eventAttrib.message.ascii = "Example nvtxMarkEx";
 * nvtxMarkEx(&eventAttrib);
 * \endcode
 *
 * \sa
 * ::nvtxDomainMarkEx
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxMarkEx(const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Marks an instantaneous event in the application.
 *
 * A marker created using \ref nvtxMarkA or \ref nvtxMarkW contains only a
 * text message.
 *
 * \param message     - The message associated to this marker event.
 *
 * \par Example:
 * \code
 * nvtxMarkA("Example nvtxMarkA");
 * nvtxMarkW(L"Example nvtxMarkW");
 * \endcode
 *
 * \sa
 * ::nvtxDomainMarkEx
 * ::nvtxMarkEx
 *
 * \version \NVTX_VERSION_0
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxMarkA(const char* message);
NVTX_DECLSPEC void NVTX_API nvtxMarkW(const wchar_t* message);
/** @} */


/** \name Process Ranges */

/* ------------------------------------------------------------------------- */
/** \brief Starts a process range in a domain.
*
* \param domain    - The domain of scoping the category.
* \param eventAttrib - The event attribute structure defining the range's
* attribute types and attribute values.
*
* \return The unique ID used to correlate a pair of Start and End events.
*
* \remarks Ranges defined by Start/End can overlap.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("my domain");
* nvtxEventAttributes_t eventAttrib = {0};
* eventAttrib.version = NVTX_VERSION;
* eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
* eventAttrib.message.ascii = "my range";
* nvtxRangeId_t rangeId = nvtxDomainRangeStartEx(&eventAttrib);
* // ...
* nvtxDomainRangeEnd(rangeId);
* \endcode
*
* \sa
* ::nvtxDomainRangeEnd
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxDomainRangeStartEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Starts a process range.
 *
 * \param eventAttrib - The event attribute structure defining the range's
 * attribute types and attribute values.
 *
 * \return The unique ID used to correlate a pair of Start and End events.
 *
 * \remarks Ranges defined by Start/End can overlap.
 *
 * \par Example:
 * \code
 * nvtxEventAttributes_t eventAttrib = {0};
 * eventAttrib.version = NVTX_VERSION;
 * eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
 * eventAttrib.category = 3;
 * eventAttrib.colorType = NVTX_COLOR_ARGB;
 * eventAttrib.color = 0xFF0088FF;
 * eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
 * eventAttrib.message.ascii = "Example Range";
 * nvtxRangeId_t rangeId = nvtxRangeStartEx(&eventAttrib);
 * // ...
 * nvtxRangeEnd(rangeId);
 * \endcode
 *
 * \sa
 * ::nvtxRangeEnd
 * ::nvtxDomainRangeStartEx
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartEx(const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Starts a process range.
 *
 * \param message     - The event message associated to this range event.
 *
 * \return The unique ID used to correlate a pair of Start and End events.
 *
 * \remarks Ranges defined by Start/End can overlap.
 *
 * \par Example:
 * \code
 * nvtxRangeId_t r1 = nvtxRangeStartA("Range 1");
 * nvtxRangeId_t r2 = nvtxRangeStartW(L"Range 2");
 * nvtxRangeEnd(r1);
 * nvtxRangeEnd(r2);
 * \endcode
 *
 * \sa
 * ::nvtxRangeEnd
 * ::nvtxRangeStartEx
 * ::nvtxDomainRangeStartEx
 *
 * \version \NVTX_VERSION_0
 * @{ */
NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartA(const char* message);
NVTX_DECLSPEC nvtxRangeId_t NVTX_API nvtxRangeStartW(const wchar_t* message);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Ends a process range.
*
* \param domain - The domain 
* \param id - The correlation ID returned from a nvtxRangeStart call.
*
* \remarks This function is offered completeness but is an alias for ::nvtxRangeEnd. 
* It does not need a domain param since that is associated iwth the range ID at ::nvtxDomainRangeStartEx
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("my domain");
* nvtxEventAttributes_t eventAttrib = {0};
* eventAttrib.version = NVTX_VERSION;
* eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
* eventAttrib.message.ascii = "my range";
* nvtxRangeId_t rangeId = nvtxDomainRangeStartEx(&eventAttrib);
* // ...
* nvtxDomainRangeEnd(rangeId);
* \endcode
*
* \sa
* ::nvtxDomainRangeStartEx
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxDomainRangeEnd(nvtxDomainHandle_t domain, nvtxRangeId_t id);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Ends a process range.
 *
 * \param id - The correlation ID returned from an nvtxRangeStart call.
 *
 * \sa
 * ::nvtxDomainRangeStartEx
 * ::nvtxRangeStartEx
 * ::nvtxRangeStartA
 * ::nvtxRangeStartW
 *
 * \version \NVTX_VERSION_0
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxRangeEnd(nvtxRangeId_t id);
/** @} */

/** \name Thread Ranges */

/* ------------------------------------------------------------------------- */
/** \brief Starts a nested thread range.
*
* \param domain    - The domain of scoping.
* \param eventAttrib - The event attribute structure defining the range's
* attribute types and attribute values.
*
* \return The 0 based level of range being started. This value is scoped to the domain.
* If an error occurs, a negative value is returned.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("example domain");
* nvtxEventAttributes_t eventAttrib = {0};
* eventAttrib.version = NVTX_VERSION;
* eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib.colorType = NVTX_COLOR_ARGB;
* eventAttrib.color = 0xFFFF0000;
* eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
* eventAttrib.message.ascii = "Level 0";
* nvtxDomainRangePushEx(domain, &eventAttrib);
*
* // Re-use eventAttrib
* eventAttrib.messageType = NVTX_MESSAGE_TYPE_UNICODE;
* eventAttrib.message.unicode = L"Level 1";
* nvtxDomainRangePushEx(domain, &eventAttrib);
*
* nvtxDomainRangePop(domain); //level 1
* nvtxDomainRangePop(domain); //level 0
* \endcode
*
* \sa
* ::nvtxDomainRangePop
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC int NVTX_API nvtxDomainRangePushEx(nvtxDomainHandle_t domain, const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Starts a nested thread range.
 *
 * \param eventAttrib - The event attribute structure defining the range's
 * attribute types and attribute values.
 *
 * \return The 0 based level of range being started. This level is per domain.
 * If an error occurs a negative value is returned.
 *
 * \par Example:
 * \code
 * nvtxEventAttributes_t eventAttrib = {0};
 * eventAttrib.version = NVTX_VERSION;
 * eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
 * eventAttrib.colorType = NVTX_COLOR_ARGB;
 * eventAttrib.color = 0xFFFF0000;
 * eventAttrib.messageType = NVTX_MESSAGE_TYPE_ASCII;
 * eventAttrib.message.ascii = "Level 0";
 * nvtxRangePushEx(&eventAttrib);
 *
 * // Re-use eventAttrib
 * eventAttrib.messageType = NVTX_MESSAGE_TYPE_UNICODE;
 * eventAttrib.message.unicode = L"Level 1";
 * nvtxRangePushEx(&eventAttrib);
 *
 * nvtxRangePop();
 * nvtxRangePop();
 * \endcode
 *
 * \sa
 * ::nvtxDomainRangePushEx
 * ::nvtxRangePop
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC int NVTX_API nvtxRangePushEx(const nvtxEventAttributes_t* eventAttrib);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Starts a nested thread range.
 *
 * \param message     - The event message associated to this range event.
 *
 * \return The 0 based level of range being started.  If an error occurs a
 * negative value is returned.
 *
 * \par Example:
 * \code
 * nvtxRangePushA("Level 0");
 * nvtxRangePushW(L"Level 1");
 * nvtxRangePop();
 * nvtxRangePop();
 * \endcode
 *
 * \sa
 * ::nvtxDomainRangePushEx
 * ::nvtxRangePop
 *
 * \version \NVTX_VERSION_0
 * @{ */
NVTX_DECLSPEC int NVTX_API nvtxRangePushA(const char* message);
NVTX_DECLSPEC int NVTX_API nvtxRangePushW(const wchar_t* message);
/** @} */


/* ------------------------------------------------------------------------- */
/** \brief Ends a nested thread range.
*
* \return The level of the range being ended. If an error occurs a negative
* value is returned on the current thread.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreate("example library");
* nvtxDomainRangePushA(domain, "Level 0");
* nvtxDomainRangePushW(domain, L"Level 1");
* nvtxDomainRangePop(domain);
* nvtxDomainRangePop(domain);
* \endcode
*
* \sa
* ::nvtxRangePushEx
* ::nvtxRangePushA
* ::nvtxRangePushW
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC int NVTX_API nvtxDomainRangePop(nvtxDomainHandle_t domain);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Ends a nested thread range.
 *
 * \return The level of the range being ended. If an error occurs a negative
 * value is returned on the current thread.
 *
 * \par Example:
 * \code
 * nvtxRangePushA("Level 0");
 * nvtxRangePushW(L"Level 1");
 * nvtxRangePop();
 * nvtxRangePop();
 * \endcode
 *
 * \sa
 * ::nvtxRangePushEx
 * ::nvtxRangePushA
 * ::nvtxRangePushW
 *
 * \version \NVTX_VERSION_0
 * @{ */
NVTX_DECLSPEC int NVTX_API nvtxRangePop(void);
/** @} */


/** @} */ /*END defgroup*/
/* ========================================================================= */
/** \defgroup RESOURCE_NAMING Resource Naming
 *
 * See \ref RESOURCE_NAMING for more details
 *
 * @{
 */


/*  ------------------------------------------------------------------------- */
/** \name Functions for Generic Resource Naming*/
/*  ------------------------------------------------------------------------- */

/*  ------------------------------------------------------------------------- */
/** \cond SHOW_HIDDEN
* \brief Resource typing helpers.  
*
* Classes are used to make it easy to create a series of resource types 
* per API without collisions 
*/
#define NVTX_RESOURCE_MAKE_TYPE(CLASS, INDEX) ((((uint32_t)(NVTX_RESOURCE_CLASS_ ## CLASS))<<16)|((uint32_t)(INDEX)))
#define NVTX_RESOURCE_CLASS_GENERIC 1
/** \endcond */

/* ------------------------------------------------------------------------- */
/** \brief Generic resource type for when a resource class is not available.
*
* \sa
* ::nvtxDomainResourceCreate
*
* \version \NVTX_VERSION_2
*/
typedef enum nvtxResourceGenericType_t
{
    NVTX_RESOURCE_TYPE_UNKNOWN = 0,
    NVTX_RESOURCE_TYPE_GENERIC_POINTER = NVTX_RESOURCE_MAKE_TYPE(GENERIC, 1), /**< Generic pointer assumed to have no collisions with other pointers. */
    NVTX_RESOURCE_TYPE_GENERIC_HANDLE = NVTX_RESOURCE_MAKE_TYPE(GENERIC, 2), /**< Generic handle assumed to have no collisions with other handles. */
    NVTX_RESOURCE_TYPE_GENERIC_THREAD_NATIVE = NVTX_RESOURCE_MAKE_TYPE(GENERIC, 3), /**< OS native thread identifier. */
    NVTX_RESOURCE_TYPE_GENERIC_THREAD_POSIX = NVTX_RESOURCE_MAKE_TYPE(GENERIC, 4) /**< POSIX pthread identifier. */
} nvtxResourceGenericType_t;



/** \brief Resource Attribute Structure.
* \anchor RESOURCE_ATTRIBUTE_STRUCTURE
*
* This structure is used to describe the attributes of a resource. The layout of
* the structure is defined by a specific version of the tools extension
* library and can change between different versions of the Tools Extension
* library.
*
* \par Initializing the Attributes
*
* The caller should always perform the following three tasks when using
* attributes:
* <ul>
*    <li>Zero the structure
*    <li>Set the version field
*    <li>Set the size field
* </ul>
*
* Zeroing the structure sets all the resource attributes types and values
* to the default value.
*
* The version and size field are used by the Tools Extension
* implementation to handle multiple versions of the attributes structure.
*
* It is recommended that the caller use one of the following to methods
* to initialize the event attributes structure:
*
* \par Method 1: Initializing nvtxEventAttributes for future compatibility
* \code
* nvtxResourceAttributes_t attribs = {0};
* attribs.version = NVTX_VERSION;
* attribs.size = NVTX_RESOURCE_ATTRIB_STRUCT_SIZE;
* \endcode
*
* \par Method 2: Initializing nvtxEventAttributes for a specific version
* \code
* nvtxResourceAttributes_v0 attribs = {0};
* attribs.version = 2;
* attribs.size = (uint16_t)(sizeof(nvtxResourceAttributes_v0));
* \endcode
*
* If the caller uses Method 1 it is critical that the entire binary
* layout of the structure be configured to 0 so that all fields
* are initialized to the default value.
*
* The caller should either use both NVTX_VERSION and
* NVTX_RESOURCE_ATTRIB_STRUCT_SIZE (Method 1) or use explicit values
* and a versioned type (Method 2).  Using a mix of the two methods
* will likely cause either source level incompatibility or binary
* incompatibility in the future.
*
* \par Settings Attribute Types and Values
*
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("example domain");
*
* // Initialize
* nvtxResourceAttributes_t attribs = {0};
* attribs.version = NVTX_VERSION;
* attribs.size = NVTX_RESOURCE_ATTRIB_STRUCT_SIZE;
*
* // Configure the Attributes
* attribs.identifierType = NVTX_RESOURCE_TYPE_GENERIC_POINTER;
* attribs.identifier.pValue = (const void*)pMutex;
* attribs.messageType = NVTX_MESSAGE_TYPE_ASCII;
* attribs.message.ascii = "Single thread access to database.";
*
* nvtxResourceHandle_t handle = nvtxDomainResourceCreate(domain, attribs);
* \endcode
*
* \sa
* ::nvtxDomainResourceCreate
*/
typedef struct nvtxResourceAttributes_v0
{
    /**
    * \brief Version flag of the structure.
    *
    * Needs to be set to NVTX_VERSION to indicate the version of NVTX APIs
    * supported in this header file. This can optionally be overridden to
    * another version of the tools extension library.
    */
    uint16_t version;

    /**
    * \brief Size of the structure.
    *
    * Needs to be set to the size in bytes of this attribute
    * structure.
    */
    uint16_t size;

    /**
    * \brief Identifier type specifies how to interpret the identifier field
    *
    * Defines the identifier format of the attribute structure's \ref RESOURCE_IDENTIFIER_FIELD
    * "identifier" field.
    *
    * Default Value is NVTX_RESOURCE_TYPE_UNKNOWN
    */
    int32_t identifierType;            /* values from enums following the pattern nvtxResource[name]Type_t */

    /**
    * \brief Identifier for the resource. 
    * \anchor RESOURCE_IDENTIFIER_FIELD
    *
    * An identifier may be a pointer or a handle to an OS or middleware API object.
    * The resource type will assist in avoiding collisions where handles values may collide.
    */
    union identifier_t
    {
        const void* pValue;
        uint64_t ullValue;
    } identifier;

    /** \brief Message type specified in this attribute structure.
    *
    * Defines the message format of the attribute structure's \ref RESOURCE_MESSAGE_FIELD
    * "message" field.
    *
    * Default Value is NVTX_MESSAGE_UNKNOWN
    */
    int32_t messageType;            /* nvtxMessageType_t */

    /** \brief Message assigned to this attribute structure. \anchor RESOURCE_MESSAGE_FIELD
    *
    * The text message that is attached to a resource.
    */
    nvtxMessageValue_t message;

} nvtxResourceAttributes_v0;

typedef struct nvtxResourceAttributes_v0 nvtxResourceAttributes_t;

/* \cond SHOW_HIDDEN 
* \version \NVTX_VERSION_2
*/
#define NVTX_RESOURCE_ATTRIB_STRUCT_SIZE ( (uint16_t)( sizeof(nvtxResourceAttributes_v0) ) )
typedef struct nvtxResourceHandle* nvtxResourceHandle_t;
/** \endcond */



/* ------------------------------------------------------------------------- */
/** \brief Create a resource object to track and associate data with OS and middleware objects
*
* Allows users to associate an API handle or pointer with a user-provided name.
* 
*
* \param domain - Domain to own the resource object
* \param attribs - Attributes to be associated with the resource
*
* \return A handle that represents the newly created resource object.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("example domain");
* nvtxResourceAttributes_t attribs = {0};
* attribs.version = NVTX_VERSION;
* attribs.size = NVTX_RESOURCE_ATTRIB_STRUCT_SIZE;
* attribs.identifierType = NVTX_RESOURCE_TYPE_GENERIC_POINTER;
* attribs.identifier.pValue = (const void*)pMutex;
* attribs.messageType = NVTX_MESSAGE_TYPE_ASCII;
* attribs.message.ascii = "Single thread access to database.";
* nvtxResourceHandle_t handle = nvtxDomainResourceCreate(domain, attribs);
* \endcode
*
* \sa
* ::nvtxResourceAttributes_t
* ::nvtxDomainResourceDestroy
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC nvtxResourceHandle_t NVTX_API nvtxDomainResourceCreate(nvtxDomainHandle_t domain, nvtxResourceAttributes_t* attribs);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Destroy a resource object to track and associate data with OS and middleware objects
*
* Allows users to associate an API handle or pointer with a user-provided name.
*
* \param resource - Handle to the resource in which to operate.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("example domain");
* nvtxResourceAttributes_t attribs = {0};
* attribs.version = NVTX_VERSION;
* attribs.size = NVTX_RESOURCE_ATTRIB_STRUCT_SIZE;
* attribs.identifierType = NVTX_RESOURCE_TYPE_GENERIC_POINTER;
* attribs.identifier.pValue = (const void*)pMutex;
* attribs.messageType = NVTX_MESSAGE_TYPE_ASCII;
* attribs.message.ascii = "Single thread access to database.";
* nvtxResourceHandle_t handle = nvtxDomainResourceCreate(domain, attribs);
* nvtxDomainResourceDestroy(handle);
* \endcode
*
* \sa
* ::nvtxDomainResourceCreate
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxDomainResourceDestroy(nvtxResourceHandle_t resource);
/** @} */


/** \name Functions for NVTX Category Naming*/

/* ------------------------------------------------------------------------- */
/**
* \brief Annotate an NVTX category used within a domain.
*
* Categories are used to group sets of events. Each category is identified
* through a unique ID and that ID is passed into any of the marker/range
* events to assign that event to a specific category. The nvtxDomainNameCategory
* function calls allow the user to assign a name to a category ID that is
* specific to the domain.
*
* nvtxDomainNameCategory(NULL, category, name) is equivalent to calling
* nvtxNameCategory(category, name).
*
* \param domain    - The domain of scoping the category.
* \param category  - The category ID to name.
* \param name      - The name of the category.
*
* \remarks The category names are tracked per domain.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("example");
* nvtxDomainNameCategoryA(domain, 1, "Memory Allocation");
* nvtxDomainNameCategoryW(domain, 2, L"Memory Transfer");
* \endcode
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxDomainNameCategoryA(nvtxDomainHandle_t domain, uint32_t category, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxDomainNameCategoryW(nvtxDomainHandle_t domain, uint32_t category, const wchar_t* name);
/** @} */

/** \brief Annotate an NVTX category.
 *
 * Categories are used to group sets of events. Each category is identified
 * through a unique ID and that ID is passed into any of the marker/range
 * events to assign that event to a specific category. The nvtxNameCategory
 * function calls allow the user to assign a name to a category ID.
 *
 * \param category - The category ID to name.
 * \param name     - The name of the category.
 *
 * \remarks The category names are tracked per process.
 *
 * \par Example:
 * \code
 * nvtxNameCategory(1, "Memory Allocation");
 * nvtxNameCategory(2, "Memory Transfer");
 * nvtxNameCategory(3, "Memory Object Lifetime");
 * \endcode
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameCategoryA(uint32_t category, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameCategoryW(uint32_t category, const wchar_t* name);
/** @} */

/** \name Functions for OS Threads Naming*/

/* ------------------------------------------------------------------------- */
/** \brief Annotate an OS thread.
 *
 * Allows the user to name an active thread of the current process. If an
 * invalid thread ID is provided or a thread ID from a different process is
 * used the behavior of the tool is implementation dependent.
 *
 * Tools expect thread ID to be a number that uniquely identifies the thread
 * at the time of the call. Note that a thread's ID can be reused after
 * it is destroyed. Tools may choose how to handle aliasing of thread IDs.
 *
 * POSIX pthread_t type returned by pthread_self() may not comply with these
 * expectations. Please use OS-specific thread ID instead of pthread_t.
 *
 * The thread name is associated to the default domain.  To support domains 
 * use resource objects via ::nvtxDomainResourceCreate.
 *
 * \param threadId - The ID of the thread to name.
 * \param name     - The name of the thread.
 *
 * \par Examples:
 * MS Windows:
 * \code
 * #include <windows.h>
 * nvtxNameOsThread(GetCurrentThreadId(), "Current thread");
 * nvtxNameOsThread(GetThreadId(SomeThreadHandle), "Other thread");
 * \endcode
 *
 * Android:
 * \code
 * #include <unistd.h>
 * nvtxNameOsThreadA(gettid(), "Current thread");
 * nvtxNameOsThreadA(getpid(), "Main thread");
 * \endcode
 *
 * Linux:
 * \code
 * #include <sys/syscall.h>
 * nvtxNameOsThreadA(syscall(SYS_gettid), "Current thread");
 * \endcode
 * \code
 * #include <unistd.h>
 * nvtxNameOsThreadA(getpid(), "Main thread");
 * \endcode
 *
 * OS X:
 * \code
 * #include <sys/syscall.h>
 * nvtxNameOsThreadA(syscall(SYS_thread_selfid), "Current thread");
 * \endcode
 * \code
 * #include <pthread.h>
 * __uint64_t id;
 * pthread_threadid_np(pthread_self(), &id);
 * nvtxNameOsThreadA(id, "Current thread");
 * pthread_threadid_np(somePThreadId, &id);
 * nvtxNameOsThreadA(id, "Other thread");
 * \endcode
 *
 * \version \NVTX_VERSION_1
 * @{ */
NVTX_DECLSPEC void NVTX_API nvtxNameOsThreadA(uint32_t threadId, const char* name);
NVTX_DECLSPEC void NVTX_API nvtxNameOsThreadW(uint32_t threadId, const wchar_t* name);
/** @} */


/** @} */ /*END defgroup*/
/* ========================================================================= */
/** \defgroup STRING_REGISTRATION String Registration
*
* Registered strings are intended to increase performance by lowering instrumentation
* overhead.  String may be registered once and the handle may be passed in place of
* a string where an the APIs may allow.
*
* See \ref STRING_REGISTRATION for more details
*
* @{
*/

/* ------------------------------------------------------------------------- */
/** \brief Register a string.

* Registers an immutable string with NVTX. Once registered the pointer used
* to register the domain name can be used in nvtxEventAttributes_t
* \ref MESSAGE_FIELD. This allows NVTX implementation to skip copying the
* contents of the message on each event invocation.
*
* String registration is an optimization. It is recommended to use string
* registration if the string will be passed to an event many times.
*
* String are not unregistered, except that by unregistering the entire domain
*
* \param domain  - Domain handle. If NULL then the global domain is used.
* \param string    - A unique pointer to a sequence of characters.
*
* \return A handle representing the registered string.
*
* \par Example:
* \code
* nvtxDomainCreateA("com.nvidia.nvtx.example");
* nvtxStringHandle_t message = nvtxDomainRegisterStringA(domain, "registered string");
* nvtxEventAttributes_t eventAttrib = {0};
* eventAttrib.version = NVTX_VERSION;
* eventAttrib.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib.messageType = NVTX_MESSAGE_TYPE_REGISTERED;
* eventAttrib.message.registered = message;
* \endcode
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC nvtxStringHandle_t NVTX_API nvtxDomainRegisterStringA(nvtxDomainHandle_t domain, const char* string);
NVTX_DECLSPEC nvtxStringHandle_t NVTX_API nvtxDomainRegisterStringW(nvtxDomainHandle_t domain, const wchar_t* string);
/** @} */

/** @} */ /*END defgroup*/
/* ========================================================================= */
/** \defgroup DOMAINS Domains
*
* Domains are used to group events to a developer defined scope. Middleware
* vendors may also scope their own events to avoid collisions with the
* the application developer's events, so that the application developer may
* inspect both parts and easily differentiate or filter them.  By default
* all events are scoped to a global domain where NULL is provided or when
* using APIs provided b versions of NVTX below v2
*
* Domains are intended to be typically long lived objects with the intention
* of logically separating events of large modules from each other such as
* middleware libraries from each other and the main application.
*
* See \ref DOMAINS for more details
*
* @{
*/

/* ------------------------------------------------------------------------- */
/** \brief Register a NVTX domain.
*
* Domains are used to scope annotations. All NVTX_VERSION_0 and NVTX_VERSION_1
* annotations are scoped to the global domain. The function nvtxDomainCreate
* creates a new named domain.
*
* Each domain maintains its own nvtxRangePush and nvtxRangePop stack.
*
* \param name - A unique string representing the domain.
*
* \return A handle representing the domain.
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("com.nvidia.nvtx.example");
*
* nvtxMarkA("nvtxMarkA to global domain");
*
* nvtxEventAttributes_t eventAttrib1 = {0};
* eventAttrib1.version = NVTX_VERSION;
* eventAttrib1.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib1.message.ascii = "nvtxDomainMarkEx to global domain";
* nvtxDomainMarkEx(NULL, &eventAttrib1);
*
* nvtxEventAttributes_t eventAttrib2 = {0};
* eventAttrib2.version = NVTX_VERSION;
* eventAttrib2.size = NVTX_EVENT_ATTRIB_STRUCT_SIZE;
* eventAttrib2.message.ascii = "nvtxDomainMarkEx to com.nvidia.nvtx.example";
* nvtxDomainMarkEx(domain, &eventAttrib2);
* nvtxDomainDestroy(domain);
* \endcode
*
* \sa
* ::nvtxDomainDestroy
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC nvtxDomainHandle_t NVTX_API nvtxDomainCreateA(const char* name);
NVTX_DECLSPEC nvtxDomainHandle_t NVTX_API nvtxDomainCreateW(const wchar_t* name);
/** @} */

/* ------------------------------------------------------------------------- */
/** \brief Unregister a NVTX domain.
*
* Unregisters the domain handle and frees all domain specific resources.
*
* \param domain    - the domain handle
*
* \par Example:
* \code
* nvtxDomainHandle_t domain = nvtxDomainCreateA("com.nvidia.nvtx.example");
* nvtxDomainDestroy(domain);
* \endcode
*
* \sa
* ::nvtxDomainCreateA
* ::nvtxDomainCreateW
*
* \version \NVTX_VERSION_2
* @{ */
NVTX_DECLSPEC void NVTX_API nvtxDomainDestroy(nvtxDomainHandle_t domain);
/** @} */


/** @} */ /*END defgroup*/
/* ========================================================================= */
/** \cond SHOW_HIDDEN */

#ifdef UNICODE
    #define nvtxMark            nvtxMarkW
    #define nvtxRangeStart      nvtxRangeStartW
    #define nvtxRangePush       nvtxRangePushW
    #define nvtxNameCategory    nvtxNameCategoryW
    #define nvtxNameOsThread    nvtxNameOsThreadW
    /* NVTX_VERSION_2 */
    #define nvtxDomainCreate         nvtxDomainCreateW
    #define nvtxDomainRegisterString nvtxDomainRegisterStringW
    #define nvtxDomainNameCategory   nvtxDomainNameCategoryW
#else
    #define nvtxMark            nvtxMarkA
    #define nvtxRangeStart      nvtxRangeStartA
    #define nvtxRangePush       nvtxRangePushA
    #define nvtxNameCategory    nvtxNameCategoryA
    #define nvtxNameOsThread    nvtxNameOsThreadA
    /* NVTX_VERSION_2 */
    #define nvtxDomainCreate         nvtxDomainCreateA
    #define nvtxDomainRegisterString nvtxDomainRegisterStringA
    #define nvtxDomainNameCategory   nvtxDomainNameCategoryA
#endif

/** \endcond */

#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */

#define NVTX_IMPL_GUARD /* Ensure other headers cannot included directly */

#include "nvtxDetail/nvtxTypes.h"

#ifndef NVTX_NO_IMPL
#include "nvtxDetail/nvtxImpl.h"
#endif /*NVTX_NO_IMPL*/

#undef NVTX_IMPL_GUARD

#endif /* !defined(NVTX_VERSION) */
