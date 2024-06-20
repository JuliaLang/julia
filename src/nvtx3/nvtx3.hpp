/*
 *  Copyright (c) 2020-2022, NVIDIA CORPORATION.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/* Temporary helper #defines, #undef'ed at end of header */
#define NVTX3_CPP_VERSION_MAJOR 1
#define NVTX3_CPP_VERSION_MINOR 0

/* This section handles the decision of whether to provide unversioned symbols.
 * If NVTX3_CPP_REQUIRE_EXPLICIT_VERSION is #defined, unversioned symbols are
 * not provided, and explicit-version symbols such as nvtx3::v1::scoped_range
 * and NVTX3_V1_FUNC_RANGE must be used.  By default, the first #include of this
 * header will define the unversioned symbols such as nvtx3::scoped_range and
 * NVTX3_FUNC_RANGE.  Subsequently including a different major version of this
 * header without #defining NVTX3_CPP_REQUIRE_EXPLICIT_VERSION triggers an error
 * since the symbols would conflict.  Subsequently including of a different
 * minor version within the same major version is allowed. Functionality of
 * minor versions is cumulative, regardless of include order.
 *
 * Since NVTX3_CPP_REQUIRE_EXPLICIT_VERSION allows all combinations of versions
 * to coexist without problems within a translation unit, the recommended best
 * practice for instrumenting header-based libraries with NVTX C++ Wrappers is
 * is to #define NVTX3_CPP_REQUIRE_EXPLICIT_VERSION before including nvtx3.hpp,
 * #undef it afterward, and only use explicit-version symbols.  This is not
 * necessary in common cases, such as instrumenting a standalone application, or
 * static/shared libraries in .cpp files or headers private to those projects.
 */
/* clang-format off */
#if !defined(NVTX3_CPP_REQUIRE_EXPLICIT_VERSION)
  /* Define macro used by all definitions in this header to indicate the
   * unversioned symbols should be defined in addition to the versioned ones.
   */
  #define NVTX3_INLINE_THIS_VERSION

  #if !defined(NVTX3_CPP_INLINED_VERSION_MAJOR)
    /* First occurrence of this header in the translation unit.  Define macros
     * indicating which version shall be used for unversioned symbols.
     */

    /**
     * @brief Semantic major version number for NVTX C++ wrappers of unversioned symbols
     *
     * Breaking changes may occur between major versions, and different major versions
     * cannot provide unversioned symbols in the same translation unit (.cpp file).
     *
     * Note: If NVTX3_CPP_REQUIRE_EXPLICIT_VERSION is defined, this macro is not defined.
     *
     * Not to be confused with the version number of the NVTX core library.
     */
    #define NVTX3_CPP_INLINED_VERSION_MAJOR 1  // NVTX3_CPP_VERSION_MAJOR

    /**
     * @brief Semantic minor version number for NVTX C++ wrappers of unversioned symbols
     *
     * No breaking changes occur between minor versions -- minor version changes within
     * a major version are purely additive.
     *
     * Note: If NVTX3_CPP_REQUIRE_EXPLICIT_VERSION is defined, this macro is not defined.
     *
     * Not to be confused with the version number of the NVTX core library.
     */
    #define NVTX3_CPP_INLINED_VERSION_MINOR 0  // NVTX3_CPP_VERSION_MINOR
  #elif NVTX3_CPP_INLINED_VERSION_MAJOR != NVTX3_CPP_VERSION_MAJOR
    /* Unsupported case -- cannot define unversioned symbols for different major versions
     * in the same translation unit.
     */
    #error \
      "Two different major versions of the NVTX C++ Wrappers are being included in a single .cpp file, with unversioned symbols enabled in both.  Only one major version can enable unversioned symbols in a .cpp file.  To disable unversioned symbols, #define NVTX3_CPP_REQUIRE_EXPLICIT_VERSION before #including nvtx3.hpp, and use the explicit-version symbols instead -- this is the preferred way to use nvtx3.hpp from a header file."
  #elif (NVTX3_CPP_INLINED_VERSION_MAJOR == NVTX3_CPP_VERSION_MAJOR) && \
    (NVTX3_CPP_INLINED_VERSION_MINOR < NVTX3_CPP_VERSION_MINOR)
    /* An older minor version of the same major version already defined unversioned
     * symbols.  The new features provided in this header will be inlined
     * redefine the minor version macro to this header's version.
     */
    #undef NVTX3_CPP_INLINED_VERSION_MINOR
    #define NVTX3_CPP_INLINED_VERSION_MINOR 0  // NVTX3_CPP_VERSION_MINOR
    // else, already have this version or newer, nothing to do
  #endif
#endif
/* clang-format on */

/**
 * @file nvtx3.hpp
 *
 * @brief Provides C++ constructs making the NVTX library safer and easier to
 * use with zero overhead.
 */

/**
 * \mainpage
 * \tableofcontents
 *
 * \section QUICK_START Quick Start
 *
 * To add NVTX ranges to your code, use the `nvtx3::scoped_range` RAII object. A
 * range begins when the object is created, and ends when the object is
 * destroyed.
 *
 * \code{.cpp}
 * #include "nvtx3.hpp"
 * void some_function() {
 *    // Begins a NVTX range with the messsage "some_function"
 *    // The range ends when some_function() returns and `r` is destroyed
 *    nvtx3::scoped_range r{"some_function"};
 *
 *    for(int i = 0; i < 6; ++i) {
 *       nvtx3::scoped_range loop{"loop range"};
 *       std::this_thread::sleep_for(std::chrono::seconds{1});
 *    }
 * } // Range ends when `r` is destroyed
 * \endcode
 *
 * The example code above generates the following timeline view in Nsight
 * Systems:
 *
 * \image html
 * https://raw.githubusercontent.com/NVIDIA/NVTX/release-v3/docs/images/example_range.png
 *
 * Alternatively, use the \ref MACROS like `NVTX3_FUNC_RANGE()` to add
 * ranges to your code that automatically use the name of the enclosing function
 * as the range's message.
 *
 * \code{.cpp}
 * #include "nvtx3.hpp"
 * void some_function() {
 *    // Creates a range with a message "some_function" that ends when the
 *    // enclosing function returns
 *    NVTX3_FUNC_RANGE();
 *    ...
 * }
 * \endcode
 *
 *
 * \section Overview
 *
 * The NVTX library provides a set of functions for users to annotate their code
 * to aid in performance profiling and optimization. These annotations provide
 * information to tools like Nsight Systems to improve visualization of
 * application timelines.
 *
 * \ref RANGES are one of the most commonly used NVTX constructs for annotating
 * a span of time. For example, imagine a user wanted to see every time a
 * function, `my_function`, is called and how long it takes to execute. This can
 * be accomplished with an NVTX range created on the entry to the function and
 * terminated on return from `my_function` using the push/pop C APIs:
 *
 * \code{.cpp}
 * void my_function(...) {
 *    nvtxRangePushA("my_function"); // Begins NVTX range
 *    // do work
 *    nvtxRangePop(); // Ends NVTX range
 * }
 * \endcode
 *
 * One of the challenges with using the NVTX C API is that it requires manually
 * terminating the end of the range with `nvtxRangePop`. This can be challenging
 * if `my_function()` has multiple returns or can throw exceptions as it
 * requires calling `nvtxRangePop()` before all possible return points.
 *
 * NVTX C++ solves this inconvenience through the "RAII" technique by providing
 * a `nvtx3::scoped_range` class that begins a range at construction and ends
 * the range on destruction. The above example then becomes:
 *
 * \code{.cpp}
 * void my_function(...) {
 *    nvtx3::scoped_range r{"my_function"}; // Begins NVTX range
 *    // do work
 * } // Range ends on exit from `my_function` when `r` is destroyed
 * \endcode
 *
 * The range object `r` is deterministically destroyed whenever `my_function`
 * returns---ending the NVTX range without manual intervention. For more
 * information, see \ref RANGES and `nvtx3::scoped_range_in`.
 *
 * Another inconvenience of the NVTX C APIs are the several constructs where the
 * user is expected to initialize an object at the beginning of an application
 * and reuse that object throughout the lifetime of the application. For example
 * see domains, categories, and registered messages.
 *
 * Example:
 * \code{.cpp}
 * nvtxDomainHandle_t D = nvtxDomainCreateA("my domain");
 * // Reuse `D` throughout the rest of the application
 * \endcode
 *
 * This can be problematic if the user application or library does not have an
 * explicit initialization function called before all other functions to
 * ensure that these long-lived objects are initialized before being used.
 *
 * NVTX C++ makes use of the "construct on first use" technique to alleviate
 * this inconvenience. In short, a function local static object is constructed
 * upon the first invocation of a function and returns a reference to that
 * object on all future invocations. See the documentation for `nvtx3::domain`,
 * `nvtx3::named_category`, `nvtx3::registered_string`, and
 * https://isocpp.org/wiki/faq/ctors#static-init-order-on-first-use for more
 * information.
 *
 * Using construct on first use, the above example becomes:
 * \code{.cpp}
 * struct my_domain{ static constexpr char const* name{"my domain"}; };
 *
 * // The first invocation of `domain::get` for the type `my_domain` will
 * // construct a `nvtx3::domain` object and return a reference to it. Future
 * // invocations simply return a reference.
 * nvtx3::domain const& D = nvtx3::domain::get<my_domain>();
 * \endcode
 * For more information about NVTX and how it can be used, see
 * https://docs.nvidia.com/cuda/profiler-users-guide/index.html#nvtx and
 * https://devblogs.nvidia.com/cuda-pro-tip-generate-custom-application-profile-timelines-nvtx/
 * for more information.
 *
 * \section RANGES Ranges
 *
 * Ranges are used to describe a span of time during the execution of an
 * application. Common examples are using ranges to annotate the time it takes
 * to execute a function or an iteration of a loop.
 *
 * NVTX C++ uses RAII to automate the generation of ranges that are tied to the
 * lifetime of objects. Similar to `std::lock_guard` in the C++ Standard
 * Template Library.
 *
 * \subsection scoped_range Scoped Range
 *
 * `nvtx3::scoped_range_in` is a class that begins a range upon construction
 * and ends the range at destruction. This is one of the most commonly used
 * constructs in NVTX C++ and is useful for annotating spans of time on a
 * particular thread. These ranges can be nested to arbitrary depths.
 *
 * `nvtx3::scoped_range` is an alias for a `nvtx3::scoped_range_in` in the
 * global NVTX domain. For more information about Domains, see \ref DOMAINS.
 *
 * Various attributes of a range can be configured constructing a
 * `nvtx3::scoped_range_in` with a `nvtx3::event_attributes` object. For
 * more information, see \ref ATTRIBUTES.
 *
 * Example:
 *
 * \code{.cpp}
 * void some_function() {
 *    // Creates a range for the duration of `some_function`
 *    nvtx3::scoped_range r{};
 *
 *    while(true) {
 *       // Creates a range for every loop iteration
 *       // `loop_range` is nested inside `r`
 *       nvtx3::scoped_range loop_range{};
 *    }
 * }
 * \endcode
 *
 * \subsection unique_range Unique Range
 *
 * `nvtx3::unique_range` is similar to `nvtx3::scoped_range`, with a few key differences:
 * - `unique_range` objects can be destroyed in any order whereas `scoped_range` objects must be
 *    destroyed in exact reverse creation order
 * - `unique_range` can start and end on different threads
 * - `unique_range` is moveable
 * - `unique_range` objects can be constructed as heap objects
 *
 * There is extra overhead associated with `unique_range` constructs and therefore use of
 * `nvtx3::scoped_range_in` should be preferred.
 *
 * \section MARKS Marks
 *
 * `nvtx3::mark` annotates an instantaneous point in time with a "marker".
 *
 * Unlike a "range" which has a beginning and an end, a marker is a single event
 * in an application, such as detecting a problem:
 *
 * \code{.cpp}
 * bool success = do_operation(...);
 * if (!success) {
 *    nvtx3::mark("operation failed!");
 * }
 * \endcode
 *
 * \section DOMAINS Domains
 *
 * Similar to C++ namespaces, domains allow for scoping NVTX events. By default,
 * all NVTX events belong to the "global" domain. Libraries and applications
 * should scope their events to use a custom domain to differentiate where the
 * events originate from.
 *
 * It is common for a library or application to have only a single domain and
 * for the name of that domain to be known at compile time. Therefore, Domains
 * in NVTX C++ are represented by _tag types_.
 *
 * For example, to define a custom domain, simply define a new concrete type
 * (a `class` or `struct`) with a `static` member called `name` that contains
 * the desired name of the domain.
 *
 * \code{.cpp}
 * struct my_domain{ static constexpr char const* name{"my domain"}; };
 * \endcode
 *
 * For any NVTX C++ construct that can be scoped to a domain, the type
 * `my_domain` can be passed as an explicit template argument to scope it to
 * the custom domain.
 *
 * The tag type `nvtx3::domain::global` represents the global NVTX domain.
 *
 * \code{.cpp}
 * // By default, `scoped_range_in` belongs to the global domain
 * nvtx3::scoped_range_in<> r0{};
 *
 * // Alias for a `scoped_range_in` in the global domain
 * nvtx3::scoped_range r1{};
 *
 * // `r` belongs to the custom domain
 * nvtx3::scoped_range_in<my_domain> r{};
 * \endcode
 *
 * When using a custom domain, it is recommended to define type aliases for NVTX
 * constructs in the custom domain.
 * \code{.cpp}
 * using my_scoped_range = nvtx3::scoped_range_in<my_domain>;
 * using my_registered_string = nvtx3::registered_string_in<my_domain>;
 * using my_named_category = nvtx3::named_category_in<my_domain>;
 * \endcode
 *
 * See `nvtx3::domain` for more information.
 *
 * \section ATTRIBUTES Event Attributes
 *
 * NVTX events can be customized with various attributes to provide additional
 * information (such as a custom message) or to control visualization of the
 * event (such as the color used). These attributes can be specified per-event
 * via arguments to a `nvtx3::event_attributes` object.
 *
 * NVTX events can be customized via four "attributes":
 * - \ref COLOR : color used to visualize the event in tools.
 * - \ref MESSAGES :  Custom message string.
 * - \ref PAYLOAD :  User-defined numerical value.
 * - \ref CATEGORY : Intra-domain grouping.
 *
 * It is possible to construct a `nvtx3::event_attributes` from any number of
 * attribute objects (nvtx3::color, nvtx3::message, nvtx3::payload,
 * nvtx3::category) in any order. If an attribute is not specified, a tool
 * specific default value is used. See `nvtx3::event_attributes` for more
 * information.
 *
 * \code{.cpp}
 * // Set message, same as passing nvtx3::message{"message"}
 * nvtx3::event_attributes attr{"message"};
 *
 * // Set message and color
 * nvtx3::event_attributes attr{"message", nvtx3::rgb{127, 255, 0}};
 *
 * // Set message, color, payload, category
 * nvtx3::event_attributes attr{"message",
 *                              nvtx3::rgb{127, 255, 0},
 *                              nvtx3::payload{42},
 *                              nvtx3::category{1}};
 *
 * // Same as above -- can use any order of arguments
 * nvtx3::event_attributes attr{nvtx3::payload{42},
 *                              nvtx3::category{1},
 *                              "message",
 *                              nvtx3::rgb{127, 255, 0}};
 *
 * // Multiple arguments of the same type are allowed, but only the first is
 * // used -- in this example, payload is set to 42:
 * nvtx3::event_attributes attr{ nvtx3::payload{42}, nvtx3::payload{7} };
 *
 * // Using the nvtx3 namespace in a local scope makes the syntax more succinct:
 * using namespace nvtx3;
 * event_attributes attr{"message", rgb{127, 255, 0}, payload{42}, category{1}};
 * \endcode
 *
 * \subsection MESSAGES message
 *
 * `nvtx3::message` sets the message string for an NVTX event.
 *
 * Example:
 * \code{.cpp}
 * // Create an `event_attributes` with the message "my message"
 * nvtx3::event_attributes attr{nvtx3::message{"my message"}};
 *
 * // strings and string literals implicitly assumed to be a `nvtx3::message`
 * nvtx3::event_attributes attr{"my message"};
 * \endcode
 *
 * \subsubsection REGISTERED_MESSAGE Registered Messages
 *
 * Associating a `nvtx3::message` with an event requires copying the contents of
 * the message every time the message is used, i.e., copying the entire message
 * string. This may cause non-trivial overhead in performance sensitive code.
 *
 * To eliminate this overhead, NVTX allows registering a message string,
 * yielding a "handle" that is inexpensive to copy that may be used in place of
 * a message string. When visualizing the events, tools such as Nsight Systems
 * will take care of mapping the message handle to its string.
 *
 * A message should be registered once and the handle reused throughout the rest
 * of the application. This can be done by either explicitly creating static
 * `nvtx3::registered_string` objects, or using the
 * `nvtx3::registered_string::get` construct on first use helper (recommended).
 *
 * Similar to \ref DOMAINS, `nvtx3::registered_string::get` requires defining a
 * custom tag type with a static `message` member whose value will be the
 * contents of the registered string.
 *
 * Example:
 * \code{.cpp}
 * // Explicitly constructed, static `registered_string` in my_domain:
 * static registered_string_in<my_domain> static_message{"my message"};
 *
 * // Or use construct on first use:
 * // Define a tag type with a `message` member string to register
 * struct my_message{ static constexpr char const* message{ "my message" }; };
 *
 * // Uses construct on first use to register the contents of
 * // `my_message::message`
 * auto& msg = nvtx3::registered_string_in<my_domain>::get<my_message>();
 * \endcode
 *
 * \subsection COLOR color
 *
 * Associating a `nvtx3::color` with an event allows controlling how the event
 * is visualized in a tool such as Nsight Systems. This is a convenient way to
 * visually differentiate among different events.
 *
 * \code{.cpp}
 * // Define a color via rgb color values
 * nvtx3::color c{nvtx3::rgb{127, 255, 0}};
 * nvtx3::event_attributes attr{c};
 *
 * // rgb color values can be passed directly to an `event_attributes`
 * nvtx3::event_attributes attr1{nvtx3::rgb{127,255,0}};
 * \endcode
 *
 * \subsection CATEGORY category
 *
 * A `nvtx3::category` is simply an integer id that allows for fine-grain
 * grouping of NVTX events. For example, one might use separate categories for
 * IO, memory allocation, compute, etc.
 *
 * \code{.cpp}
 * nvtx3::event_attributes{nvtx3::category{1}};
 * \endcode
 *
 * \subsubsection NAMED_CATEGORIES Named Categories
 *
 * Associates a `name` string with a category `id` to help differentiate among
 * categories.
 *
 * For any given category id `Id`, a `named_category{Id, "name"}` should only
 * be constructed once and reused throughout an application. This can be done by
 * either explicitly creating static `nvtx3::named_category` objects, or using
 * the `nvtx3::named_category::get` construct on first use helper (recommended).
 *
 * Similar to \ref DOMAINS, `nvtx3::named_category::get` requires defining a
 * custom tag type with static `name` and `id` members.
 *
 * \code{.cpp}
 * // Explicitly constructed, static `named_category` in my_domain:
 * static nvtx3::named_category_in<my_domain> static_category{42, "my category"};
 *
 * // Or use construct on first use:
 * // Define a tag type with `name` and `id` members
 * struct my_category {
 *    static constexpr char const* name{"my category"}; // category name
 *    static constexpr uint32_t id{42}; // category id
 * };
 *
 * // Use construct on first use to name the category id `42`
 * // with name "my category":
 * auto& cat = named_category_in<my_domain>::get<my_category>();
 *
 * // Range `r` associated with category id `42`
 * nvtx3::event_attributes attr{cat};
 * \endcode
 *
 * \subsection PAYLOAD payload
 *
 * Allows associating a user-defined numerical value with an event.
 *
 * \code{.cpp}
 * // Constructs a payload from the `int32_t` value 42
 * nvtx3:: event_attributes attr{nvtx3::payload{42}};
 * \endcode
 *
 *
 * \section EXAMPLE Example
 *
 * Putting it all together:
 * \code{.cpp}
 * // Define a custom domain tag type
 * struct my_domain{ static constexpr char const* name{"my domain"}; };
 *
 * // Define a named category tag type
 * struct my_category{
 *    static constexpr char const* name{"my category"};
 *    static constexpr uint32_t id{42};
 * };
 *
 * // Define a registered string tag type
 * struct my_message{ static constexpr char const* message{"my message"}; };
 *
 * // For convenience, use aliases for domain scoped objects
 * using my_scoped_range = nvtx3::scoped_range_in<my_domain>;
 * using my_registered_string = nvtx3::registered_string_in<my_domain>;
 * using my_named_category = nvtx3::named_category_in<my_domain>;
 *
 * // Default values for all attributes
 * nvtx3::event_attributes attr{};
 * my_scoped_range r0{attr};
 *
 * // Custom (unregistered) message, and unnamed category
 * nvtx3::event_attributes attr1{"message", nvtx3::category{2}};
 * my_scoped_range r1{attr1};
 *
 * // Alternatively, pass arguments of `event_attributes` ctor directly to
 * // `my_scoped_range`
 * my_scoped_range r2{"message", nvtx3::category{2}};
 *
 * // construct on first use a registered string
 * auto& msg = my_registered_string::get<my_message>();
 *
 * // construct on first use a named category
 * auto& cat = my_named_category::get<my_category>();
 *
 * // Use registered string and named category with a custom payload
 * my_scoped_range r3{msg, cat, nvtx3::payload{42}};
 *
 * // Any number of arguments in any order
 * my_scoped_range r{nvtx3::rgb{127, 255,0}, msg};
 *
 * \endcode
 * \section MACROS Convenience Macros
 *
 * Oftentimes users want to quickly and easily add NVTX ranges to their library
 * or application to aid in profiling and optimization.
 *
 * A convenient way to do this is to use the \ref NVTX3_FUNC_RANGE and
 * \ref NVTX3_FUNC_RANGE_IN macros. These macros take care of constructing an
 * `nvtx3::scoped_range_in` with the name of the enclosing function as the
 * range's message.
 *
 * \code{.cpp}
 * void some_function() {
 *    // Automatically generates an NVTX range for the duration of the function
 *    // using "some_function" as the event's message.
 *    NVTX3_FUNC_RANGE();
 * }
 * \endcode
 *
 */

/* Temporary helper #defines, removed with #undef at end of header */

#if !defined(NVTX3_USE_CHECKED_OVERLOADS_FOR_GET)
#if defined(_MSC_VER) && _MSC_VER < 1914
/* Microsoft's compiler prior to VS2017 Update 7 (15.7) uses an older parser
 * that does not work with domain::get's specialization for domain::global,
 * and would require extra conditions to make SFINAE work for the overloaded
 * get() functions.  This macro disables use of overloaded get() in order to
 * work with VS2015 and versions of VS2017 below 15.7, without penalizing
 * users of newer compilers.  Building with this flag set to 0 means errors
 * when defining tag structs (see documentation for domain, named_category,
 * and registered_string) will have more complex compiler error messages
 * instead of the clear static_assert messages from the get() overloads.
 */
#define NVTX3_USE_CHECKED_OVERLOADS_FOR_GET 0
#else
#define NVTX3_USE_CHECKED_OVERLOADS_FOR_GET 1
#endif
#define NVTX3_USE_CHECKED_OVERLOADS_FOR_GET_DEFINED_HERE
#endif

/* Within this header, nvtx3::NVTX3_VERSION_NAMESPACE resolves to nvtx3::vX,
 * where "X" is the major version number. */
#define NVTX3_CONCAT(A, B) A##B
#define NVTX3_NAMESPACE_FOR(VERSION) NVTX3_CONCAT(v, VERSION)
#define NVTX3_VERSION_NAMESPACE NVTX3_NAMESPACE_FOR(NVTX3_CPP_VERSION_MAJOR)

/* Avoid duplicating #if defined(NVTX3_INLINE_THIS_VERSION) for namespaces
 * in each minor version by making a macro to use unconditionally, which
 * resolves to "inline" or nothing as appropriate. */
#if defined(NVTX3_INLINE_THIS_VERSION)
#define NVTX3_INLINE_IF_REQUESTED inline
#else
#define NVTX3_INLINE_IF_REQUESTED
#endif

/* Enables the use of constexpr when support for C++14 constexpr is present.
 *
 * Initialization of a class member that is a union to a specific union member
 * can only be done in the body of a constructor, not in a member initializer
 * list.  A constexpr constructor must have an empty body until C++14, so there
 * is no way to make an initializer of a member union constexpr in C++11.  This
 * macro allows making functions constexpr in C++14 or newer, but non-constexpr
 * in C++11 compilation.  It is used here on constructors that initialize their
 * member unions.
 */
#if __cpp_constexpr >= 201304L
#define NVTX3_CONSTEXPR_IF_CPP14 constexpr
#else
#define NVTX3_CONSTEXPR_IF_CPP14
#endif

 /* Use a macro for static asserts, which defaults to static_assert, but that
  * testing tools can replace with a logging function.  For example:
  * #define NVTX3_STATIC_ASSERT(c, m) \
  *   do { if (!(c)) printf("static_assert would fail: %s\n", m); } while (0)
  */
#if !defined(NVTX3_STATIC_ASSERT)
#define NVTX3_STATIC_ASSERT(condition, message) static_assert(condition, message);
#define NVTX3_STATIC_ASSERT_DEFINED_HERE
#endif

/* Implementation sections, enclosed in guard macros for each minor version */

#ifndef NVTX3_CPP_DEFINITIONS_V1_0
#define NVTX3_CPP_DEFINITIONS_V1_0

#include "nvToolsExt.h"

#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <cstddef>

namespace nvtx3 {

NVTX3_INLINE_IF_REQUESTED namespace NVTX3_VERSION_NAMESPACE
{

namespace detail {

template <typename Unused>
struct always_false : std::false_type {};

template <typename T, typename = void>
struct has_name : std::false_type {};
template <typename T>
struct has_name<T, decltype((void)T::name, void())> : std::true_type {};

template <typename T, typename = void>
struct has_id : std::false_type {};
template <typename T>
struct has_id<T, decltype((void)T::id, void())> : std::true_type {};

template <typename T, typename = void>
struct has_message : std::false_type {};
template <typename T>
struct has_message<T, decltype((void)T::message, void())> : std::true_type {};

template <typename T, typename = void>
struct is_c_string : std::false_type {};
template <typename T>
struct is_c_string<T, typename std::enable_if<
  std::is_convertible<T, char const*   >::value ||
  std::is_convertible<T, wchar_t const*>::value
>::type> : std::true_type {};

template <typename T>
using is_uint32 = std::is_same<typename std::decay<T>::type, uint32_t>;

}  // namespace detail

/**
 * @brief `domain`s allow for grouping NVTX events into a single scope to
 * differentiate them from events in other `domain`s.
 *
 * By default, all NVTX constructs are placed in the "global" NVTX domain.
 *
 * A custom `domain` may be used in order to differentiate a library's or
 * application's NVTX events from other events.
 *
 * `domain`s are expected to be long-lived and unique to a library or
 * application. As such, it is assumed a domain's name is known at compile
 * time. Therefore, all NVTX constructs that can be associated with a domain
 * require the domain to be specified via a *type* `D` passed as an
 * explicit template parameter.
 *
 * The type `domain::global` may be used to indicate that the global NVTX
 * domain should be used.
 *
 * None of the C++ NVTX constructs require the user to manually construct a
 * `domain` object. Instead, if a custom domain is desired, the user is
 * expected to define a type `D` that contains a member
 * `D::name` which resolves to either a `char const*` or `wchar_t
 * const*`. The value of `D::name` is used to name and uniquely
 * identify the custom domain.
 *
 * Upon the first use of an NVTX construct associated with the type
 * `D`, the "construct on first use" pattern is used to construct a
 * function local static `domain` object. All future NVTX constructs
 * associated with `D` will use a reference to the previously
 * constructed `domain` object. See `domain::get`.
 *
 * Example:
 * \code{.cpp}
 * // The type `my_domain` defines a `name` member used to name and identify
 * // the `domain` object identified by `my_domain`.
 * struct my_domain{ static constexpr char const* name{"my_domain"}; };
 *
 * // The NVTX range `r` will be grouped with all other NVTX constructs
 * // associated with  `my_domain`.
 * nvtx3::scoped_range_in<my_domain> r{};
 *
 * // An alias can be created for a `scoped_range_in` in the custom domain
 * using my_scoped_range = nvtx3::scoped_range_in<my_domain>;
 * my_scoped_range my_range{};
 *
 * // `domain::global` indicates that the global NVTX domain is used
 * nvtx3::scoped_range_in<domain::global> r2{};
 *
 * // For convenience, `nvtx3::scoped_range` is an alias for a range in the
 * // global domain
 * nvtx3::scoped_range r3{};
 * \endcode
 */
class domain {
 public:
  domain(domain const&) = delete;
  domain& operator=(domain const&) = delete;
  domain(domain&&) = delete;
  domain& operator=(domain&&) = delete;

  /**
   * @brief Tag type for the "global" NVTX domain.
   *
   * This type may be passed as a template argument to any function/class
   * expecting a type to identify a domain to indicate that the global domain
   * should be used.
   *
   * All NVTX events in the global domain across all libraries and
   * applications will be grouped together.
   *
   */
  struct global {
  };

#if NVTX3_USE_CHECKED_OVERLOADS_FOR_GET
  /**
   * @brief Returns reference to an instance of a function local static
   * `domain` object.
   *
   * Uses the "construct on first use" idiom to safely ensure the `domain`
   * object is initialized exactly once upon first invocation of
   * `domain::get<D>()`. All following invocations will return a
   * reference to the previously constructed `domain` object. See
   * https://isocpp.org/wiki/faq/ctors#static-init-order-on-first-use
   *
   * None of the constructs in this header require the user to directly invoke
   * `domain::get`. It is automatically invoked when constructing objects like
   * a `scoped_range_in` or `category`. Advanced users may wish to use
   * `domain::get` for the convenience of the "construct on first use" idiom
   * when using domains with their own use of the NVTX C API.
   *
   * This function is threadsafe as of C++11. If two or more threads call
   * `domain::get<D>` concurrently, exactly one of them is guaranteed
   * to construct the `domain` object and the other(s) will receive a
   * reference to the object after it is fully constructed.
   *
   * The domain's name is specified via the type `D` pass as an
   * explicit template parameter. `D` is required to contain a
   * member `D::name` that resolves to either a `char const*` or
   * `wchar_t const*`. The value of `D::name` is used to name and
   * uniquely identify the `domain`.
   *
   * Example:
   * \code{.cpp}
   * // The type `my_domain` defines a `name` member used to name and identify
   * // the `domain` object identified by `my_domain`.
   * struct my_domain{ static constexpr char const* name{"my domain"}; };
   *
   * auto& D1 = domain::get<my_domain>(); // First invocation constructs a
   *                                      // `domain` with the name "my domain"
   *
   * auto& D2 = domain::get<my_domain>(); // Quickly returns reference to
   *                                      // previously constructed `domain`.
   * \endcode
   *
   * @tparam D Type that contains a `D::name` member used to
   * name the `domain` object.
   * @return Reference to the `domain` corresponding to the type `D`.
   */
  template <typename D = global,
    typename std::enable_if<
      detail::is_c_string<decltype(D::name)>::value
    , int>::type = 0>
  static domain const& get() noexcept
  {
    static domain const d(D::name);
    return d;
  }

  /**
   * @brief Overload of `domain::get` to provide a clear compile error when
   * `D` has a `name` member that is not directly convertible to either
   * `char const*` or `wchar_t const*`.
   */
  template <typename D = global,
    typename std::enable_if<
      !detail::is_c_string<decltype(D::name)>::value
    , int>::type = 0>
  static domain const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::always_false<D>::value,
      "Type used to identify an NVTX domain must contain a static constexpr member "
      "called 'name' of type const char* or const wchar_t* -- 'name' member is not "
      "convertible to either of those types");
    static domain const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }

  /**
   * @brief Overload of `domain::get` to provide a clear compile error when
   * `D` does not have a `name` member.
   */
  template <typename D = global,
    typename std::enable_if<
      !detail::has_name<D>::value
    , int>::type = 0>
  static domain const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::always_false<D>::value,
      "Type used to identify an NVTX domain must contain a static constexpr member "
      "called 'name' of type const char* or const wchar_t* -- 'name' member is missing");
    static domain const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }
#else
  template <typename D = global>
  static domain const& get() noexcept
  {
    static domain const d(D::name);
    return d;
  }
#endif

  /**
   * @brief Conversion operator to `nvtxDomainHandle_t`.
   *
   * Allows transparently passing a domain object into an API expecting a
   * native `nvtxDomainHandle_t` object.
   */
  operator nvtxDomainHandle_t() const noexcept { return _domain; }

 private:
  /**
   * @brief Construct a new domain with the specified `name`.
   *
   * This constructor is private as it is intended that `domain` objects only
   * be created through the `domain::get` function.
   *
   * @param name A unique name identifying the domain
   */
  explicit domain(char const* name) noexcept : _domain{nvtxDomainCreateA(name)} {}

  /**
   * @brief Construct a new domain with the specified `name`.
   *
   * This constructor is private as it is intended that `domain` objects only
   * be created through the `domain::get` function.
   *
   * @param name A unique name identifying the domain
   */
  explicit domain(wchar_t const* name) noexcept : _domain{nvtxDomainCreateW(name)} {}

  /**
   * @brief Construct a new domain with the specified `name`.
   *
   * This constructor is private as it is intended that `domain` objects only
   * be created through the `domain::get` function.
   *
   * @param name A unique name identifying the domain
   */
  explicit domain(std::string const& name) noexcept : domain{name.c_str()} {}

  /**
   * @brief Construct a new domain with the specified `name`.
   *
   * This constructor is private as it is intended that `domain` objects only
   * be created through the `domain::get` function.
   *
   * @param name A unique name identifying the domain
   */
  explicit domain(std::wstring const& name) noexcept : domain{name.c_str()} {}

  /**
   * @brief Default constructor creates a `domain` representing the
   * "global" NVTX domain.
   *
   * All events not associated with a custom `domain` are grouped in the
   * "global" NVTX domain.
   *
   */
  domain() noexcept {}

  /**
   * @brief Intentionally avoid calling nvtxDomainDestroy on the `domain` object.
   *
   * No currently-available tools attempt to free domain resources when the
   * nvtxDomainDestroy function is called, due to the thread-safety and
   * efficiency challenges of freeing thread-local storage for other threads.
   * Since libraries may be disallowed from introducing static destructors,
   * and destroying the domain is likely to have no effect, the destructor
   * for `domain` intentionally chooses to not destroy the domain.
   *
   * In a situation where domain destruction is necessary, either manually
   * call nvtxDomainDestroy on the domain's handle, or make a class that
   * derives from `domain` and calls nvtxDomainDestroy in its destructor.
   */
  ~domain() = default;

 private:
  nvtxDomainHandle_t const _domain{};  ///< The `domain`s NVTX handle
};

/**
 * @brief Returns reference to the `domain` object that represents the global
 * NVTX domain.
 *
 * This specialization for `domain::global` returns a default constructed,
 * `domain` object for use when the "global" domain is desired.
 *
 * All NVTX events in the global domain across all libraries and applications
 * will be grouped together.
 *
 * @return Reference to the `domain` corresponding to the global NVTX domain.
 *
 */
template <>
inline domain const& domain::get<domain::global>() noexcept
{
  static domain const d{};
  return d;
}

/**
 * @brief Indicates the values of the red, green, and blue color channels for
 * an RGB color to use as an event attribute (assumes no transparency).
 *
 */
struct rgb {
  /// Type used for component values
  using component_type = uint8_t;

  /**
   * @brief Construct a rgb with red, green, and blue channels
   * specified by `red_`, `green_`, and `blue_`, respectively.
   *
   * Valid values are in the range `[0,255]`.
   *
   * @param red_ Value of the red channel
   * @param green_ Value of the green channel
   * @param blue_ Value of the blue channel
   */
  constexpr rgb(
    component_type red_,
    component_type green_,
    component_type blue_) noexcept
    : red{red_}, green{green_}, blue{blue_}
  {
  }

  component_type red{};    ///< Red channel value
  component_type green{};  ///< Green channel value
  component_type blue{};   ///< Blue channel value
};

/**
 * @brief Indicates the value of the alpha, red, green, and blue color
 * channels for an ARGB color to use as an event attribute.
 *
 */
struct argb final : rgb {
  /**
   * @brief Construct an argb with alpha, red, green, and blue channels
   * specified by `alpha_`, `red_`, `green_`, and `blue_`, respectively.
   *
   * Valid values are in the range `[0,255]`.
   *
   * @param alpha_  Value of the alpha channel (opacity)
   * @param red_  Value of the red channel
   * @param green_  Value of the green channel
   * @param blue_  Value of the blue channel
   *
   */
  constexpr argb(
    component_type alpha_,
    component_type red_,
    component_type green_,
    component_type blue_) noexcept
    : rgb{red_, green_, blue_}, alpha{alpha_}
  {
  }

  component_type alpha{};  ///< Alpha channel value
};

/**
 * @brief Represents a custom color that can be associated with an NVTX event
 * via it's `event_attributes`.
 *
 * Specifying colors for NVTX events is a convenient way to visually
 * differentiate among different events in a visualization tool such as Nsight
 * Systems.
 *
 */
class color {
 public:
  /// Type used for the color's value
  using value_type = uint32_t;

  /**
   * @brief Constructs a `color` using the value provided by `hex_code`.
   *
   * `hex_code` is expected to be a 4 byte argb hex code.
   *
   * The most significant byte indicates the value of the alpha channel
   * (opacity) (0-255)
   *
   * The next byte indicates the value of the red channel (0-255)
   *
   * The next byte indicates the value of the green channel (0-255)
   *
   * The least significant byte indicates the value of the blue channel
   * (0-255)
   *
   * @param hex_code The hex code used to construct the `color`
   */
  constexpr explicit color(value_type hex_code) noexcept : _value{hex_code} {}

  /**
   * @brief Construct a `color` using the alpha, red, green, blue components
   * in `argb`.
   *
   * @param argb The alpha, red, green, blue components of the desired `color`
   */
  constexpr color(argb argb_) noexcept
    : color{from_bytes_msb_to_lsb(argb_.alpha, argb_.red, argb_.green, argb_.blue)}
  {
  }

  /**
   * @brief Construct a `color` using the red, green, blue components in
   * `rgb`.
   *
   * Uses maximum value for the alpha channel (opacity) of the `color`.
   *
   * @param rgb The red, green, blue components of the desired `color`
   */
  constexpr color(rgb rgb_) noexcept
    : color{from_bytes_msb_to_lsb(0xFF, rgb_.red, rgb_.green, rgb_.blue)}
  {
  }

  /**
   * @brief Returns the `color`s argb hex code
   *
   */
  constexpr value_type get_value() const noexcept { return _value; }

  /**
   * @brief Return the NVTX color type of the color.
   *
   */
  constexpr nvtxColorType_t get_type() const noexcept { return _type; }

  color() = delete;
  ~color() = default;
  color(color const&) = default;
  color& operator=(color const&) = default;
  color(color&&) = default;
  color& operator=(color&&) = default;

 private:
  /**
   * @brief Constructs an unsigned, 4B integer from the component bytes in
   * most to least significant byte order.
   *
   */
  constexpr static value_type from_bytes_msb_to_lsb(
    uint8_t byte3,
    uint8_t byte2,
    uint8_t byte1,
    uint8_t byte0) noexcept
  {
    return uint32_t{byte3} << 24 | uint32_t{byte2} << 16 | uint32_t{byte1} << 8 | uint32_t{byte0};
  }

  value_type _value{};                     ///< color's argb color code
  nvtxColorType_t _type{NVTX_COLOR_ARGB};  ///< NVTX color type code
};

/**
 * @brief Object for intra-domain grouping of NVTX events.
 *
 * A `category` is simply an integer id that allows for fine-grain grouping of
 * NVTX events. For example, one might use separate categories for IO, memory
 * allocation, compute, etc.
 *
 * Example:
 * \code{.cpp}
 * nvtx3::category cat1{1};
 *
 * // Range `r1` belongs to the category identified by the value `1`.
 * nvtx3::scoped_range r1{cat1};
 *
 * // Range `r2` belongs to the same category as `r1`
 * nvtx3::scoped_range r2{nvtx3::category{1}};
 * \endcode
 *
 * To associate a name string with a category id, see `named_category`.
 *
 */
class category {
 public:
  /// Type used for `category`s integer id.
  using id_type = uint32_t;

  /**
   * @brief Construct a `category` with the specified `id`.
   *
   * The `category` will be unnamed and identified only by its `id` value.
   *
   * All `category`s in a domain sharing the same `id` are equivalent.
   *
   * @param[in] id The `category`'s identifying value
   */
  constexpr explicit category(id_type id) noexcept : id_{id} {}

  /**
   * @brief Returns the id of the category.
   *
   */
  constexpr id_type get_id() const noexcept { return id_; }

  category() = delete;
  ~category() = default;
  category(category const&) = default;
  category& operator=(category const&) = default;
  category(category&&) = default;
  category& operator=(category&&) = default;

 private:
  id_type id_{};  ///< category's unique identifier
};

/**
 * @brief A `category` with an associated name string.
 *
 * Associates a `name` string with a category `id` to help differentiate among
 * categories.
 *
 * For any given category id `Id`, a `named_category(Id, "name")` should only
 * be constructed once and reused throughout an application. This can be done
 * by either explicitly creating static `named_category` objects, or using the
 * `named_category::get` construct on first use helper (recommended).
 *
 * Creating two or more `named_category` objects with the same value for `id`
 * in the same domain results in undefined behavior.
 *
 * Similarly, behavior is undefined when a `named_category` and `category`
 * share the same value of `id`.
 *
 * Example:
 * \code{.cpp}
 * // Explicitly constructed, static `named_category` in global domain:
 * static nvtx3::named_category static_category{42, "my category"};
 *
 * // Range `r` associated with category id `42`
 * nvtx3::scoped_range r{static_category};
 *
 * // OR use construct on first use:
 *
 * // Define a type with `name` and `id` members
 * struct my_category {
 *    static constexpr char const* name{"my category"}; // category name
 *    static constexpr uint32_t id{42}; // category id
 * };
 *
 * // Use construct on first use to name the category id `42`
 * // with name "my category"
 * auto& cat = named_category_in<my_domain>::get<my_category>();
 *
 * // Range `r` associated with category id `42`
 * nvtx3::scoped_range r{cat};
 * \endcode
 *
 * `named_category_in<D>`'s association of a name to a category id is local to
 * the domain specified by the type `D`. An id may have a different name in
 * another domain.
 *
 * @tparam D Type containing `name` member used to identify the `domain` to
 * which the `named_category_in` belongs. Else, `domain::global` to indicate
 * that the global NVTX domain should be used.
 */
template <typename D = domain::global>
class named_category_in final : public category {
 public:
#if NVTX3_USE_CHECKED_OVERLOADS_FOR_GET
  /**
   * @brief Returns a global instance of a `named_category_in` as a
   * function-local static.
   *
   * Creates a `named_category_in<D>` with name and id specified by the contents
   * of a type `C`. `C::name` determines the name and `C::id` determines the
   * category id.
   *
   * This function is useful for constructing a named `category` exactly once
   * and reusing the same instance throughout an application.
   *
   * Example:
   * \code{.cpp}
   * // Define a type with `name` and `id` members
   * struct my_category {
   *    static constexpr char const* name{"my category"}; // category name
   *    static constexpr uint32_t id{42}; // category id
   * };
   *
   * // Use construct on first use to name the category id `42`
   * // with name "my category"
   * auto& cat = named_category_in<my_domain>::get<my_category>();
   *
   * // Range `r` associated with category id `42`
   * nvtx3::scoped_range r{cat};
   * \endcode
   *
   * Uses the "construct on first use" idiom to safely ensure the `category`
   * object is initialized exactly once. See
   * https://isocpp.org/wiki/faq/ctors#static-init-order-on-first-use
   *
   * @tparam C Type containing a member `C::name` that resolves to either a
   * `char const*` or `wchar_t const*` and `C::id`.
   */
  template <typename C,
    typename std::enable_if<
      detail::is_c_string<decltype(C::name)>::value &&
      detail::is_uint32<decltype(C::id)>::value
    , int>::type = 0>
  static named_category_in const& get() noexcept
  {
    static named_category_in const cat(C::id, C::name);
    return cat;
  }

  /**
   * @brief Overload of `named_category_in::get` to provide a clear compile error
   * when `C` has the required `name` and `id` members, but they are not the
   * required types.  `name` must be directly convertible to `char const*` or
   * `wchar_t const*`, and `id` must be `uint32_t`.
   */
  template <typename C,
    typename std::enable_if<
      !detail::is_c_string<decltype(C::name)>::value ||
      !detail::is_uint32<decltype(C::id)>::value
    , int>::type = 0>
  static named_category_in const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::is_c_string<decltype(C::name)>::value,
      "Type used to name an NVTX category must contain a static constexpr member "
      "called 'name' of type const char* or const wchar_t* -- 'name' member is not "
      "convertible to either of those types");
    NVTX3_STATIC_ASSERT(detail::is_uint32<decltype(C::id)>::value,
      "Type used to name an NVTX category must contain a static constexpr member "
      "called 'id' of type uint32_t -- 'id' member is the wrong type");
    static named_category_in const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }

  /**
   * @brief Overload of `named_category_in::get` to provide a clear compile error
   * when `C` does not have the required `name` and `id` members.
   */
  template <typename C,
    typename std::enable_if<
      !detail::has_name<C>::value ||
      !detail::has_id<C>::value
    , int>::type = 0>
  static named_category_in const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::has_name<C>::value,
      "Type used to name an NVTX category must contain a static constexpr member "
      "called 'name' of type const char* or const wchar_t* -- 'name' member is missing");
    NVTX3_STATIC_ASSERT(detail::has_id<C>::value,
      "Type used to name an NVTX category must contain a static constexpr member "
      "called 'id' of type uint32_t -- 'id' member is missing");
    static named_category_in const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }
#else
  template <typename C>
  static named_category_in const& get() noexcept
  {
    static named_category_in const cat(C::id, C::name);
    return cat;
  }
#endif

 private:
  // Default constructor is only used internally for static_assert(false) cases.
  named_category_in() noexcept : category{0} {}

 public:
  /**
   * @brief Construct a `named_category_in` with the specified `id` and `name`.
   *
   * The name `name` will be registered with `id`.
   *
   * Every unique value of `id` should only be named once.
   *
   * @param[in] id The category id to name
   * @param[in] name The name to associated with `id`
   */
  named_category_in(id_type id, char const* name) noexcept : category{id}
  {
#ifndef NVTX_DISABLE
    nvtxDomainNameCategoryA(domain::get<D>(), get_id(), name);
#else
    (void)id;
    (void)name;
#endif
  };

  /**
   * @brief Construct a `named_category_in` with the specified `id` and `name`.
   *
   * The name `name` will be registered with `id`.
   *
   * Every unique value of `id` should only be named once.
   *
   * @param[in] id The category id to name
   * @param[in] name The name to associated with `id`
   */
  named_category_in(id_type id, wchar_t const* name) noexcept : category{id}
  {
#ifndef NVTX_DISABLE
    nvtxDomainNameCategoryW(domain::get<D>(), get_id(), name);
#else
    (void)id;
    (void)name;
#endif
  };
};

/**
 * @brief Alias for a `named_category_in` in the global NVTX domain.
 *
 */
using named_category = named_category_in<domain::global>;

/**
 * @brief A message registered with NVTX.
 *
 * Normally, associating a `message` with an NVTX event requires copying the
 * contents of the message string. This may cause non-trivial overhead in
 * highly performance sensitive regions of code.
 *
 * message registration is an optimization to lower the overhead of
 * associating a message with an NVTX event. Registering a message yields a
 * handle that is inexpensive to copy that may be used in place of a message
 * string.
 *
 * A particular message should only be registered once and the handle
 * reused throughout the rest of the application. This can be done by either
 * explicitly creating static `registered_string_in` objects, or using the
 * `registered_string_in::get` construct on first use helper (recommended).
 *
 * Example:
 * \code{.cpp}
 * // Explicitly constructed, static `registered_string` in my_domain:
 * static registered_string_in<my_domain> static_message{"message"};
 *
 * // "message" is associated with the range `r`
 * nvtx3::scoped_range r{static_message};
 *
 * // Or use construct on first use:
 *
 * // Define a type with a `message` member that defines the contents of the
 * // registered string
 * struct my_message{ static constexpr char const* message{ "my message" }; };
 *
 * // Uses construct on first use to register the contents of
 * // `my_message::message`
 * auto& msg = registered_string_in<my_domain>::get<my_message>();
 *
 * // "my message" is associated with the range `r`
 * nvtx3::scoped_range r{msg};
 * \endcode
 *
 * `registered_string_in`s are local to a particular domain specified via
 * the type `D`.
 *
 * @tparam D Type containing `name` member used to identify the `domain` to
 * which the `registered_string_in` belongs. Else, `domain::global` to indicate
 * that the global NVTX domain should be used.
 */
template <typename D = domain::global>
class registered_string_in {
 public:
#if NVTX3_USE_CHECKED_OVERLOADS_FOR_GET
  /**
   * @brief Returns a global instance of a `registered_string_in` as a function
   * local static.
   *
   * Provides a convenient way to register a message with NVTX without having
   * to explicitly register the message.
   *
   * Upon first invocation, constructs a `registered_string_in` whose contents
   * are specified by `message::message`.
   *
   * All future invocations will return a reference to the object constructed
   * in the first invocation.
   *
   * Example:
   * \code{.cpp}
   * // Define a type with a `message` member that defines the contents of the
   * // registered string
   * struct my_message{ static constexpr char const* message{ "my message" };
   * };
   *
   * // Uses construct on first use to register the contents of
   * // `my_message::message`
   * auto& msg = registered_string_in<my_domain>::get<my_message>();
   *
   * // "my message" is associated with the range `r`
   * nvtx3::scoped_range r{msg};
   * \endcode
   *
   * @tparam M Type required to contain a member `M::message` that
   * resolves to either a `char const*` or `wchar_t const*` used as the
   * registered string's contents.
   * @return Reference to a `registered_string_in` associated with the type `M`.
   */
  template <typename M,
    typename std::enable_if<
      detail::is_c_string<decltype(M::message)>::value
    , int>::type = 0>
  static registered_string_in const& get() noexcept
  {
    static registered_string_in const regstr(M::message);
    return regstr;
  }

  /**
   * @brief Overload of `registered_string_in::get` to provide a clear compile error
   * when `M` has a `message` member that is not directly convertible to either
   * `char const*` or `wchar_t const*`.
   */
  template <typename M,
    typename std::enable_if<
      !detail::is_c_string<decltype(M::message)>::value
    , int>::type = 0>
  static registered_string_in const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::always_false<M>::value,
      "Type used to register an NVTX string must contain a static constexpr member "
      "called 'message' of type const char* or const wchar_t* -- 'message' member is "
      "not convertible to either of those types");
    static registered_string_in const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }

  /**
   * @brief Overload of `registered_string_in::get` to provide a clear compile error when
   * `M` does not have a `message` member.
   */
  template <typename M,
    typename std::enable_if<
      !detail::has_message<M>::value
    , int>::type = 0>
  static registered_string_in const& get() noexcept
  {
    NVTX3_STATIC_ASSERT(detail::always_false<M>::value,
      "Type used to register an NVTX string must contain a static constexpr member "
      "called 'message' of type const char* or const wchar_t* -- 'message' member "
      "is missing");
    static registered_string_in const unused;
    return unused;  // Function must compile for static_assert to be triggered
  }
#else
  template <typename M>
  static registered_string_in const& get() noexcept
  {
    static registered_string_in const regstr(M::message);
    return regstr;
  }
#endif

  /**
   * @brief Constructs a `registered_string_in` from the specified `msg` string.
   *
   * Registers `msg` with NVTX and associates a handle with the registered
   * message.
   *
   * A particular message should should only be registered once and the handle
   * reused throughout the rest of the application.
   *
   * @param msg The contents of the message
   */
  explicit registered_string_in(char const* msg) noexcept
    : handle_{nvtxDomainRegisterStringA(domain::get<D>(), msg)}
  {
  }

  /**
   * @brief Constructs a `registered_string_in` from the specified `msg` string.
   *
   * Registers `msg` with NVTX and associates a handle with the registered
   * message.
   *
   * A particular message should should only be registered once and the handle
   * reused throughout the rest of the application.
   *
   * @param msg The contents of the message
   */
  explicit registered_string_in(std::string const& msg) noexcept
    : registered_string_in{msg.c_str()} {}

  /**
   * @brief Constructs a `registered_string_in` from the specified `msg` string.
   *
   * Registers `msg` with NVTX and associates a handle with the registered
   * message.
   *
   * A particular message should should only be registered once and the handle
   * reused throughout the rest of the application.
   *
   * @param msg The contents of the message
   */
  explicit registered_string_in(wchar_t const* msg) noexcept
    : handle_{nvtxDomainRegisterStringW(domain::get<D>(), msg)}
  {
  }

  /**
   * @brief Constructs a `registered_string_in` from the specified `msg` string.
   *
   * Registers `msg` with NVTX and associates a handle with the registered
   * message.
   *
   * A particular message should only be registered once and the handle
   * reused throughout the rest of the application.
   *
   * @param msg The contents of the message
   */
  explicit registered_string_in(std::wstring const& msg) noexcept
    : registered_string_in{msg.c_str()} {}

  /**
   * @brief Returns the registered string's handle
   *
   */
  nvtxStringHandle_t get_handle() const noexcept { return handle_; }

private:
  // Default constructor is only used internally for static_assert(false) cases.
  registered_string_in() noexcept {};
public:
  ~registered_string_in() = default;
  registered_string_in(registered_string_in const&) = default;
  registered_string_in& operator=(registered_string_in const&) = default;
  registered_string_in(registered_string_in&&) = default;
  registered_string_in& operator=(registered_string_in&&) = default;

 private:
  nvtxStringHandle_t handle_{};  ///< The handle returned from
                                 ///< registering the message with NVTX
};

/**
 * @brief Alias for a `registered_string_in` in the global NVTX domain.
 *
 */
using registered_string = registered_string_in<domain::global>;

/**
 * @brief Allows associating a message string with an NVTX event via
 * its `EventAttribute`s.
 *
 * Associating a `message` with an NVTX event through its `event_attributes`
 * allows for naming events to easily differentiate them from other events.
 *
 * Every time an NVTX event is created with an associated `message`, the
 * contents of the message string must be copied.  This may cause non-trivial
 * overhead in highly performance sensitive sections of code. Use of a
 * `nvtx3::registered_string` is recommended in these situations.
 *
 * Example:
 * \code{.cpp}
 * // Creates an `event_attributes` with message "message 0"
 * nvtx3::event_attributes attr0{nvtx3::message{"message 0"}};
 *
 * // `range0` contains message "message 0"
 * nvtx3::scoped_range range0{attr0};
 *
 * // `std::string` and string literals are implicitly assumed to be
 * // the contents of an `nvtx3::message`
 * // Creates an `event_attributes` with message "message 1"
 * nvtx3::event_attributes attr1{"message 1"};
 *
 * // `range1` contains message "message 1"
 * nvtx3::scoped_range range1{attr1};
 *
 * // `range2` contains message "message 2"
 * nvtx3::scoped_range range2{nvtx3::Mesage{"message 2"}};
 *
 * // `std::string` and string literals are implicitly assumed to be
 * // the contents of an `nvtx3::message`
 * // `range3` contains message "message 3"
 * nvtx3::scoped_range range3{"message 3"};
 * \endcode
 */
class message {
 public:
  using value_type = nvtxMessageValue_t;

  /**
   * @brief Construct a `message` whose contents are specified by `msg`.
   *
   * @param msg The contents of the message
   */
  NVTX3_CONSTEXPR_IF_CPP14 message(char const* msg) noexcept : type_{NVTX_MESSAGE_TYPE_ASCII}
  {
    value_.ascii = msg;
  }

  /**
   * @brief Construct a `message` whose contents are specified by `msg`.
   *
   * @param msg The contents of the message
   */
  message(std::string const& msg) noexcept : message{msg.c_str()} {}

  /**
   * @brief Disallow construction for `std::string` r-value
   *
   * `message` is a non-owning type and therefore cannot take ownership of an
   * r-value. Therefore, constructing from an r-value is disallowed to prevent
   * a dangling pointer.
   *
   */
  message(std::string&&) = delete;

  /**
   * @brief Construct a `message` whose contents are specified by `msg`.
   *
   * @param msg The contents of the message
   */
  NVTX3_CONSTEXPR_IF_CPP14 message(wchar_t const* msg) noexcept : type_{NVTX_MESSAGE_TYPE_UNICODE}
  {
    value_.unicode = msg;
  }

  /**
   * @brief Construct a `message` whose contents are specified by `msg`.
   *
   * @param msg The contents of the message
   */
  message(std::wstring const& msg) noexcept : message{msg.c_str()} {}

  /**
   * @brief Disallow construction for `std::wstring` r-value
   *
   * `message` is a non-owning type and therefore cannot take ownership of an
   * r-value. Therefore, constructing from an r-value is disallowed to prevent
   * a dangling pointer.
   *
   */
  message(std::wstring&&) = delete;

  /**
   * @brief Construct a `message` from a `registered_string_in`.
   *
   * @tparam D Type containing `name` member used to identify the `domain`
   * to which the `registered_string_in` belongs. Else, `domain::global` to
   * indicate that the global NVTX domain should be used.
   * @param msg The message that has already been registered with NVTX.
   */
  template <typename D>
  NVTX3_CONSTEXPR_IF_CPP14 message(registered_string_in<D> const& msg) noexcept
    : type_{NVTX_MESSAGE_TYPE_REGISTERED}
  {
    value_.registered = msg.get_handle();
  }

  /**
   * @brief Construct a `message` from NVTX C API type and value.
   *
   * @param type nvtxMessageType_t enum value indicating type of the payload
   * @param value nvtxMessageValue_t union containing message
   */
  constexpr message(
    nvtxMessageType_t const& type,
    nvtxMessageValue_t const& value) noexcept
    : type_{type}, value_(value)
  {
  }

  /**
   * @brief Construct a `message` from NVTX C API registered string handle.
   *
   * @param handle nvtxStringHandle_t value of registered string handle
   */
  NVTX3_CONSTEXPR_IF_CPP14 message(nvtxStringHandle_t handle) noexcept
    : type_{NVTX_MESSAGE_TYPE_REGISTERED}
  {
    value_.registered = handle;
  }

  /**
   * @brief Return the union holding the value of the message.
   *
   */
  constexpr value_type get_value() const noexcept { return value_; }

  /**
   * @brief Return the type information about the value the union holds.
   *
   */
  constexpr nvtxMessageType_t get_type() const noexcept { return type_; }

 private:
  nvtxMessageType_t type_{};    ///< message type
  nvtxMessageValue_t value_{};  ///< message contents
};

/**
 * @brief A numerical value that can be associated with an NVTX event via
 * its `event_attributes`.
 *
 * Example:
 * \code{.cpp}
 * // Constructs a payload from the int32_t value 42
 * nvtx3:: event_attributes attr{nvtx3::payload{42}};
 *
 * // `range0` will have an int32_t payload of 42
 * nvtx3::scoped_range range0{attr};
 *
 * // range1 has double payload of 3.14
 * nvtx3::scoped_range range1{nvtx3::payload{3.14}};
 * \endcode
 */
class payload {
 public:
  using value_type = typename nvtxEventAttributes_v2::payload_t;

  /**
   * @brief Construct a `payload` from a signed, 8 byte integer.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(int64_t value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_INT64}, value_{}
  {
    value_.llValue = value;
  }

  /**
   * @brief Construct a `payload` from a signed, 4 byte integer.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(int32_t value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_INT32}, value_{}
  {
    value_.iValue = value;
  }

  /**
   * @brief Construct a `payload` from an unsigned, 8 byte integer.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(uint64_t value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_UNSIGNED_INT64}, value_{}
  {
    value_.ullValue = value;
  }

  /**
   * @brief Construct a `payload` from an unsigned, 4 byte integer.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(uint32_t value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_UNSIGNED_INT32}, value_{}
  {
    value_.uiValue = value;
  }

  /**
   * @brief Construct a `payload` from a single-precision floating point
   * value.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(float value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_FLOAT}, value_{}
  {
    value_.fValue = value;
  }

  /**
   * @brief Construct a `payload` from a double-precision floating point
   * value.
   *
   * @param value Value to use as contents of the payload
   */
  NVTX3_CONSTEXPR_IF_CPP14 explicit payload(double value) noexcept
    : type_{NVTX_PAYLOAD_TYPE_DOUBLE}, value_{}
  {
    value_.dValue = value;
  }

  /**
   * @brief Construct a `payload` from NVTX C API type and value.
   *
   * @param type nvtxPayloadType_t enum value indicating type of the payload
   * @param value nvtxEventAttributes_t::payload_t union containing payload
   */
  constexpr payload(
    nvtxPayloadType_t const& type,
    value_type const& value) noexcept
    : type_{type}, value_(value)
  {
  }

  /**
   * @brief Return the union holding the value of the payload
   *
   */
  constexpr value_type get_value() const noexcept { return value_; }

  /**
   * @brief Return the information about the type the union holds.
   *
   */
  constexpr nvtxPayloadType_t get_type() const noexcept { return type_; }

 private:
  nvtxPayloadType_t type_;  ///< Type of the payload value
  value_type value_;        ///< Union holding the payload value
};

/**
 * @brief Describes the attributes of a NVTX event.
 *
 * NVTX events can be customized via four "attributes":
 *
 * - color:    color used to visualize the event in tools such as Nsight
 *             Systems. See `color`.
 * - message:  Custom message string. See `message`.
 * - payload:  User-defined numerical value. See `payload`.
 * - category: Intra-domain grouping. See `category`.
 *
 * These component attributes are specified via an `event_attributes` object.
 * See `nvtx3::color`, `nvtx3::message`, `nvtx3::payload`, and
 * `nvtx3::category` for how these individual attributes are constructed.
 *
 * While it is possible to specify all four attributes, it is common to want
 * to only specify a subset of attributes and use default values for the
 * others. For convenience, `event_attributes` can be constructed from any
 * number of attribute components in any order.
 *
 * Example:
 * \code{.cpp}
 * // Set message, same as using nvtx3::message{"message"}
 * event_attributes attr{"message"};
 *
 * // Set message and color
 * event_attributes attr{"message", nvtx3::rgb{127, 255, 0}};
 *
 * // Set message, color, payload, category
 * event_attributes attr{"message",
 *                       nvtx3::rgb{127, 255, 0},
 *                       nvtx3::payload{42},
 *                       nvtx3::category{1}};
 *
 * // Same as above -- can use any order of arguments
 * event_attributes attr{nvtx3::payload{42},
 *                       nvtx3::category{1},
 *                       "message",
 *                       nvtx3::rgb{127, 255, 0}};
 *
 * // Multiple arguments of the same type are allowed, but only the first is
 * // used -- in this example, payload is set to 42:
 * event_attributes attr{ nvtx3::payload{42}, nvtx3::payload{7} };
 *
 * // Range `r` will be customized according the attributes in `attr`
 * nvtx3::scoped_range r{attr};
 *
 * // For convenience, `event_attributes` constructor arguments may be passed
 * // to the `scoped_range_in` contructor -- they are forwarded to the
 * // `event_attributes` constructor
 * nvtx3::scoped_range r{nvtx3::payload{42}, nvtx3::category{1}, "message"};
 *
 * // Using the nvtx3 namespace in a local scope makes the syntax more succinct:
 * using namespace nvtx3;
 * scoped_range r{payload{42}, category{1}, "message"};
 * \endcode
 *
 */
class event_attributes {
 public:
  using value_type = nvtxEventAttributes_t;

  /**
   * @brief Default constructor creates an `event_attributes` with no
   * category, color, payload, nor message.
   */
  constexpr event_attributes() noexcept
    : attributes_{
        NVTX_VERSION,                   // version
        sizeof(nvtxEventAttributes_t),  // size
        0,                              // category
        NVTX_COLOR_UNKNOWN,             // color type
        0,                              // color value
        NVTX_PAYLOAD_UNKNOWN,           // payload type
        0,                              // reserved 4B
        0,                              // payload value (union)
        NVTX_MESSAGE_UNKNOWN,           // message type
        0                               // message value (union)
      }
  {
  }

  /**
   * @brief Variadic constructor where the first argument is a `category`.
   *
   * Sets the value of the `EventAttribute`s category based on `c` and
   * forwards the remaining variadic parameter pack to the next constructor.
   *
   */
  template <typename... Args>
  NVTX3_CONSTEXPR_IF_CPP14 explicit event_attributes(category const& c, Args const&... args) noexcept
    : event_attributes(args...)
  {
    attributes_.category = c.get_id();
  }

  /**
   * @brief Variadic constructor where the first argument is a `color`.
   *
   * Sets the value of the `EventAttribute`s color based on `c` and forwards
   * the remaining variadic parameter pack to the next constructor.
   *
   */
  template <typename... Args>
  NVTX3_CONSTEXPR_IF_CPP14 explicit event_attributes(color const& c, Args const&... args) noexcept
    : event_attributes(args...)
  {
    attributes_.color     = c.get_value();
    attributes_.colorType = c.get_type();
  }

  /**
   * @brief Variadic constructor where the first argument is a `payload`.
   *
   * Sets the value of the `EventAttribute`s payload based on `p` and forwards
   * the remaining variadic parameter pack to the next constructor.
   *
   */
  template <typename... Args>
  NVTX3_CONSTEXPR_IF_CPP14 explicit event_attributes(payload const& p, Args const&... args) noexcept
    : event_attributes(args...)
  {
    attributes_.payload     = p.get_value();
    attributes_.payloadType = p.get_type();
  }

  /**
   * @brief Variadic constructor where the first argument is a `message`.
   *
   * Sets the value of the `EventAttribute`s message based on `m` and forwards
   * the remaining variadic parameter pack to the next constructor.
   *
   */
  template <typename... Args>
  NVTX3_CONSTEXPR_IF_CPP14 explicit event_attributes(message const& m, Args const&... args) noexcept
    : event_attributes(args...)
  {
    attributes_.message     = m.get_value();
    attributes_.messageType = m.get_type();
  }

  ~event_attributes() = default;
  event_attributes(event_attributes const&) = default;
  event_attributes& operator=(event_attributes const&) = default;
  event_attributes(event_attributes&&) = default;
  event_attributes& operator=(event_attributes&&) = default;

  /**
   * @brief Get raw pointer to underlying NVTX attributes object.
   *
   */
  constexpr value_type const* get() const noexcept { return &attributes_; }

 private:
  value_type attributes_{};  ///< The NVTX attributes structure
};

/**
 * @brief A RAII object for creating a NVTX range local to a thread within a
 * domain.
 *
 * When constructed, begins a nested NVTX range on the calling thread in the
 * specified domain. Upon destruction, ends the NVTX range.
 *
 * Behavior is undefined if a `scoped_range_in` object is
 * created/destroyed on different threads.
 *
 * `scoped_range_in` is neither moveable nor copyable.
 *
 * `scoped_range_in`s may be nested within other ranges.
 *
 * The domain of the range is specified by the template type parameter `D`.
 * By default, the `domain::global` is used, which scopes the range to the
 * global NVTX domain. The convenience alias `scoped_range` is provided for
 * ranges scoped to the global domain.
 *
 * A custom domain can be defined by creating a type, `D`, with a static
 * member `D::name` whose value is used to name the domain associated with
 * `D`. `D::name` must resolve to either `char const*` or `wchar_t const*`
 *
 * Example:
 * \code{.cpp}
 * // Define a type `my_domain` with a member `name` used to name the domain
 * // associated with the type `my_domain`.
 * struct my_domain{
 *    static constexpr char const* name{"my domain"};
 * };
 * \endcode
 *
 * Usage:
 * \code{.cpp}
 * nvtx3::scoped_range_in<my_domain> r1{"range 1"}; // Range in my domain
 *
 * // Three equivalent ways to make a range in the global domain:
 * nvtx3::scoped_range_in<nvtx3::domain::global> r2{"range 2"};
 * nvtx3::scoped_range_in<> r3{"range 3"};
 * nvtx3::scoped_range r4{"range 4"};
 *
 * // Create an alias to succinctly make ranges in my domain:
 * using my_scoped_range = nvtx3::scoped_range_in<my_domain>;
 *
 * my_scoped_range r3{"range 3"};
 * \endcode
 */
template <class D = domain::global>
class scoped_range_in {
 public:
  /**
   * @brief Construct a `scoped_range_in` with the specified
   * `event_attributes`
   *
   * Example:
   * \code{cpp}
   * nvtx3::event_attributes attr{"msg", nvtx3::rgb{127,255,0}};
   * nvtx3::scoped_range range{attr}; // Creates a range with message contents
   *                                  // "msg" and green color
   * \endcode
   *
   * @param[in] attr `event_attributes` that describes the desired attributes
   * of the range.
   */
  explicit scoped_range_in(event_attributes const& attr) noexcept
  {
#ifndef NVTX_DISABLE
    nvtxDomainRangePushEx(domain::get<D>(), attr.get());
#else
    (void)attr;
#endif
  }

  /**
   * @brief Constructs a `scoped_range_in` from the constructor arguments
   * of an `event_attributes`.
   *
   * Forwards the arguments `args...` to construct an
   * `event_attributes` object. The `event_attributes` object is then
   * associated with the `scoped_range_in`.
   *
   * For more detail, see `event_attributes` documentation.
   *
   * Example:
   * \code{cpp}
   * // Creates a range with message "message" and green color
   * nvtx3::scoped_range r{"message", nvtx3::rgb{127,255,0}};
   * \endcode
   *
   * @param[in] args Arguments to used to construct an `event_attributes` associated with this
   * range.
   *
   */
  template <typename... Args>
  explicit scoped_range_in(Args const&... args) noexcept
    : scoped_range_in{event_attributes{args...}}
  {
  }

  /**
   * @brief Default constructor creates a `scoped_range_in` with no
   * message, color, payload, nor category.
   *
   */
  scoped_range_in() noexcept : scoped_range_in{event_attributes{}} {}

  /**
   * @brief Delete `operator new` to disallow heap allocated objects.
   *
   * `scoped_range_in` must follow RAII semantics to guarantee proper push/pop semantics.
   *
   */
  void* operator new(std::size_t) = delete;

  scoped_range_in(scoped_range_in const&) = delete;
  scoped_range_in& operator=(scoped_range_in const&) = delete;
  scoped_range_in(scoped_range_in&&) = delete;
  scoped_range_in& operator=(scoped_range_in&&) = delete;

  /**
   * @brief Destroy the scoped_range_in, ending the NVTX range event.
   */
  ~scoped_range_in() noexcept
  {
#ifndef NVTX_DISABLE
    nvtxDomainRangePop(domain::get<D>());
#endif
  }
};

/**
 * @brief Alias for a `scoped_range_in` in the global NVTX domain.
 *
 */
using scoped_range = scoped_range_in<domain::global>;

namespace detail {

/// @cond internal
template <typename D = domain::global>
class optional_scoped_range_in
{
public:
  optional_scoped_range_in() = default;

  void begin(event_attributes const& attr) noexcept
  {
#ifndef NVTX_DISABLE
    // This class is not meant to be part of the public NVTX C++ API and should
    // only be used in the `NVTX3_FUNC_RANGE_IF` and `NVTX3_FUNC_RANGE_IF_IN`
    // macros. However, to prevent developers from misusing this class, make
    // sure to not start multiple ranges.
    if (initialized) { return; }

    nvtxDomainRangePushEx(domain::get<D>(), attr.get());
    initialized = true;
#endif
  }

  ~optional_scoped_range_in() noexcept
  {
#ifndef NVTX_DISABLE
    if (initialized) { nvtxDomainRangePop(domain::get<D>()); }
#endif
  }

  void* operator new(std::size_t) = delete;
  optional_scoped_range_in(optional_scoped_range_in const&) = delete;
  optional_scoped_range_in& operator=(optional_scoped_range_in const&) = delete;
  optional_scoped_range_in(optional_scoped_range_in&&) = delete;
  optional_scoped_range_in& operator=(optional_scoped_range_in&&) = delete;

private:
#ifndef NVTX_DISABLE
  bool initialized = false;
#endif
};
/// @endcond

} // namespace detail

/**
 * @brief Handle used for correlating explicit range start and end events.
 *
 * A handle is "null" if it does not correspond to any range.
 *
 */
struct range_handle {
  /// Type used for the handle's value
  using value_type = nvtxRangeId_t;


  /**
   * @brief Construct a `range_handle` from the given id.
   *
   */
  constexpr explicit range_handle(value_type id) noexcept : _range_id{id} {}

  /**
   * @brief Constructs a null range handle.
   *
   * A null range_handle corresponds to no range. Calling `end_range` on a
   * null handle is undefined behavior when a tool is active.
   *
   */
  constexpr range_handle() noexcept = default;

  /**
   * @brief Checks whether this handle is null
   *
   * Provides contextual conversion to `bool`.
   *
   * \code{cpp}
   * range_handle handle{};
   * if (handle) {...}
   * \endcode
   *
   */
  constexpr explicit operator bool() const noexcept { return get_value() != null_range_id; };

  /**
   * @brief Implicit conversion from `nullptr` constructs a null handle.
   *
   * Satisfies the "NullablePointer" requirement to make `range_handle` comparable with `nullptr`.
   *
   */
  constexpr range_handle(std::nullptr_t) noexcept {}

  /**
   * @brief Returns the `range_handle`'s value
   *
   * @return value_type The handle's value
   */
  constexpr value_type get_value() const noexcept { return _range_id; }

 private:
  /// Sentinel value for a null handle that corresponds to no range
  static constexpr value_type null_range_id = nvtxRangeId_t{0};

  value_type _range_id{null_range_id};  ///< The underlying NVTX range id
};

/**
 * @brief Compares two range_handles for equality
 *
 * @param lhs The first range_handle to compare
 * @param rhs The second range_handle to compare
 */
inline constexpr bool operator==(range_handle lhs, range_handle rhs) noexcept
{
  return lhs.get_value() == rhs.get_value();
}

/**
 * @brief Compares two range_handles for inequality
 *
 * @param lhs The first range_handle to compare
 * @param rhs The second range_handle to compare
 */
inline constexpr bool operator!=(range_handle lhs, range_handle rhs) noexcept { return !(lhs == rhs); }

/**
 * @brief Manually begin an NVTX range.
 *
 * Explicitly begins an NVTX range and returns a unique handle. To end the
 * range, pass the handle to `end_range_in<D>()`.
 *
 * `nvtx3::start_range(...)` is equivalent to `nvtx3::start_range_in<>(...)` and
 * `nvtx3::start_range_in<nvtx3::domain::global>(...)`.
 *
 * `start_range_in/end_range_in` are the most explicit and lowest level APIs
 * provided for creating ranges.  Use of `nvtx3::unique_range_in` should be
 * preferred unless one is unable to tie the range to the lifetime of an object.
 *
 * Example:
 * \code{.cpp}
 * nvtx3::event_attributes attr{"msg", nvtx3::rgb{127,255,0}};
 * // Manually begin a range
 * nvtx3::range_handle h = nvtx3::start_range_in<my_domain>(attr);
 * ...
 * nvtx3::end_range_in<my_domain>(h); // End the range
 * \endcode
 *
 * @tparam D Type containing `name` member used to identify the `domain`
 * to which the range belongs. Else, `domain::global` to indicate that the
 * global NVTX domain should be used.
 * @param[in] attr `event_attributes` that describes the desired attributes
 * of the range.
 * @return Unique handle to be passed to `end_range_in` to end the range.
 */
template <typename D = domain::global>
inline range_handle start_range_in(event_attributes const& attr) noexcept
{
#ifndef NVTX_DISABLE
  return range_handle{nvtxDomainRangeStartEx(domain::get<D>(), attr.get())};
#else
  (void)attr;
  return {};
#endif
}

/**
 * @brief Manually begin an NVTX range.
 *
 * Explicitly begins an NVTX range and returns a unique handle. To end the
 * range, pass the handle to `end_range_in<D>()`.
 *
 * `nvtx3::start_range(...)` is equivalent to `nvtx3::start_range_in<>(...)` and
 * `nvtx3::start_range_in<nvtx3::domain::global>(...)`.
 *
 * `start_range_in/end_range_in` are the most explicit and lowest level APIs
 * provided for creating ranges.  Use of `nvtx3::unique_range_in` should be
 * preferred unless one is unable to tie the range to the lifetime of an object.
 *
 * This overload uses `args...` to construct an  `event_attributes` to
 * associate with the range.  For more detail, see `event_attributes`.
 *
 * Example:
 * \code{cpp}
 * // Manually begin a range
 * nvtx3::range_handle h = nvtx3::start_range_in<D>("msg", nvtx3::rgb{127,255,0});
 * ...
 * nvtx3::end_range_in<D>(h); // Ends the range
 * \endcode
 *
 * @tparam D Type containing `name` member used to identify the `domain`
 * to which the range belongs. Else, `domain::global` to indicate that the
 * global NVTX domain should be used.
 * @param args[in] Variadic parameter pack of the arguments for an `event_attributes`.
 * @return Unique handle to be passed to `end_range` to end the range.
 */
template <typename D = domain::global, typename... Args>
inline range_handle start_range_in(Args const&... args) noexcept
{
#ifndef NVTX_DISABLE
  return start_range_in<D>(event_attributes{args...});
#else
  return {};
#endif
}

/**
 * @brief Manually begin an NVTX range in the global domain.
 *
 * Explicitly begins an NVTX range and returns a unique handle. To end the
 * range, pass the handle to `end_range()`.
 *
 * `nvtx3::start_range(...)` is equivalent to `nvtx3::start_range_in<>(...)` and
 * `nvtx3::start_range_in<nvtx3::domain::global>(...)`.
 *
 * `start_range/end_range` are the most explicit and lowest level APIs
 * provided for creating ranges.  Use of `nvtx3::unique_range` should be
 * preferred unless one is unable to tie the range to the lifetime of an object.
 *
 * Example:
 * \code{.cpp}
 * nvtx3::event_attributes attr{"msg", nvtx3::rgb{127,255,0}};
 * // Manually begin a range
 * nvtx3::range_handle h = nvtx3::start_range(attr);
 * ...
 * nvtx3::end_range(h); // End the range
 * \endcode
 *
 * @param[in] attr `event_attributes` that describes the desired attributes
 * of the range.
 * @return Unique handle to be passed to `end_range_in` to end the range.
 */
inline range_handle start_range(event_attributes const& attr) noexcept
{
#ifndef NVTX_DISABLE
  return start_range_in<domain::global>(attr);
#else
  (void)attr;
  return {};
#endif
}

/**
 * @brief Manually begin an NVTX range in the global domain.
 *
 * Explicitly begins an NVTX range and returns a unique handle. To end the
 * range, pass the handle to `end_range_in<D>()`.
 *
 * `nvtx3::start_range(...)` is equivalent to `nvtx3::start_range_in<>(...)` and
 * `nvtx3::start_range_in<nvtx3::domain::global>(...)`.
 *
 * `start_range_in/end_range_in` are the most explicit and lowest level APIs
 * provided for creating ranges.  Use of `nvtx3::unique_range_in` should be
 * preferred unless one is unable to tie the range to the lifetime of an object.
 *
 * This overload uses `args...` to construct an  `event_attributes` to
 * associate with the range.  For more detail, see `event_attributes`.
 *
 * Example:
 * \code{cpp}
 * // Manually begin a range
 * nvtx3::range_handle h = nvtx3::start_range("msg", nvtx3::rgb{127,255,0});
 * ...
 * nvtx3::end_range(h); // Ends the range
 * \endcode
 *
 * @param args[in] Variadic parameter pack of the arguments for an `event_attributes`.
 * @return Unique handle to be passed to `end_range` to end the range.
 */
template <typename... Args>
inline range_handle start_range(Args const&... args) noexcept
{
#ifndef NVTX_DISABLE
  return start_range_in<domain::global>(args...);
#else
  return {};
#endif
}

/**
 * @brief Manually end the range associated with the handle `r` in domain `D`.
 *
 * Explicitly ends the NVTX range indicated by the handle `r` returned from a
 * prior call to `start_range_in<D>`. The range may end on a different thread
 * from where it began.
 *
 * @tparam D Type containing `name` member used to identify the `domain` to
 * which the range belongs. Else, `domain::global` to indicate that the global
 * NVTX domain should be used.
 * @param r Handle to a range started by a prior call to `start_range_in`.
 *
 * @warning The domain type specified as template parameter to this function
 * must be the same that was specified on the associated `start_range_in` call.
 */
template <typename D = domain::global>
inline void end_range_in(range_handle r) noexcept
{
#ifndef NVTX_DISABLE
  nvtxDomainRangeEnd(domain::get<D>(), r.get_value());
#else
  (void)r;
#endif
}

/**
 * @brief Manually end the range associated with the handle `r` in the global
 * domain.
 *
 * Explicitly ends the NVTX range indicated by the handle `r` returned from a
 * prior call to `start_range`. The range may end on a different thread from
 * where it began.
 *
 * @param r Handle to a range started by a prior call to `start_range`.
 *
 * @warning The domain type specified as template parameter to this function
 * must be the same that was specified on the associated `start_range` call.
 */
inline void end_range(range_handle r) noexcept
{
#ifndef NVTX_DISABLE
  end_range_in<domain::global>(r);
#else
  (void)r;
#endif
}

/**
 * @brief A RAII object for creating a NVTX range within a domain that can
 * be created and destroyed on different threads.
 *
 * When constructed, begins a NVTX range in the specified domain. Upon
 * destruction, ends the NVTX range.
 *
 * Similar to `nvtx3::scoped_range_in`, with a few key differences:
 * - `unique_range` objects can be destroyed in an order whereas `scoped_range` objects must be
 *    destroyed in exact reverse creation order
 * - `unique_range` can start and end on different threads
 * - `unique_range` is moveable
 * - `unique_range` objects can be constructed as heap objects
 *
 * There is extra overhead associated with `unique_range` constructs and therefore use of
 * `nvtx3::scoped_range_in` should be preferred.
 *
 * @tparam D Type containing `name` member used to identify the `domain`
 * to which the `unique_range_in` belongs. Else, `domain::global` to
 * indicate that the global NVTX domain should be used.
 */
template <typename D = domain::global>
class unique_range_in {
 public:
  /**
   * @brief Construct a new unique_range_in object with the specified event attributes
   *
   * Example:
   * \code{cpp}
   * nvtx3::event_attributes attr{"msg", nvtx3::rgb{127,255,0}};
   * nvtx3::unique_range_in<my_domain> range{attr}; // Creates a range with message contents
   *                                            // "msg" and green color
   * \endcode
   *
   * @param[in] attr `event_attributes` that describes the desired attributes
   * of the range.
   */
  explicit unique_range_in(event_attributes const& attr) noexcept
    : handle_{start_range_in<D>(attr)}
  {
  }

  /**
   * @brief Constructs a `unique_range_in` from the constructor arguments
   * of an `event_attributes`.
   *
   * Forwards the arguments `args...` to construct an
   * `event_attributes` object. The `event_attributes` object is then
   * associated with the `unique_range_in`.
   *
   * For more detail, see `event_attributes` documentation.
   *
   * Example:
   * \code{.cpp}
   * // Creates a range with message "message" and green color
   * nvtx3::unique_range_in<> r{"message", nvtx3::rgb{127,255,0}};
   * \endcode
   *
   * @param[in] args Variadic parameter pack of arguments to construct an `event_attributes`
   * associated with this range.
   */
  template <typename... Args>
  explicit unique_range_in(Args const&... args) noexcept
    : unique_range_in{event_attributes{args...}}
  {
  }

  /**
   * @brief Default constructor creates a `unique_range_in` with no
   * message, color, payload, nor category.
   *
   */
  constexpr unique_range_in() noexcept : unique_range_in{event_attributes{}} {}

  /**
   * @brief Destroy the `unique_range_in` ending the range.
   *
   */
  ~unique_range_in() noexcept = default;

  /**
   * @brief Move constructor allows taking ownership of the NVTX range from
   * another `unique_range_in`.
   *
   * @param other The range to take ownership of
   */
  unique_range_in(unique_range_in&& other) noexcept = default;

  /**
   * @brief Move assignment operator allows taking ownership of an NVTX range
   * from another `unique_range_in`.
   *
   * @param other The range to take ownership of
   */
  unique_range_in& operator=(unique_range_in&& other) noexcept = default;

  /// Copy construction is not allowed to prevent multiple objects from owning
  /// the same range handle
  unique_range_in(unique_range_in const&) = delete;

  /// Copy assignment is not allowed to prevent multiple objects from owning the
  /// same range handle
  unique_range_in& operator=(unique_range_in const&) = delete;

 private:

  struct end_range_handle {
    using pointer = range_handle;  /// Override the pointer type of the unique_ptr
    void operator()(range_handle h) const noexcept { end_range_in<D>(h); }
  };

  /// Range handle used to correlate the start/end of the range
  std::unique_ptr<range_handle, end_range_handle> handle_;
};

/**
 * @brief Alias for a `unique_range_in` in the global NVTX domain.
 *
 */
using unique_range = unique_range_in<domain::global>;

/**
 * @brief Annotates an instantaneous point in time with a "marker", using the
 * attributes specified by `attr`.
 *
 * Unlike a "range" which has a beginning and an end, a marker is a single event
 * in an application, such as detecting a problem:
 *
 * \code{.cpp}
 * bool success = do_operation(...);
 * if (!success) {
 *    nvtx3::event_attributes attr{"operation failed!", nvtx3::rgb{255,0,0}};
 *    nvtx3::mark_in<my_domain>(attr);
 * }
 * \endcode
 *
 * Note that nvtx3::mark_in<D> is a function, not a class like scoped_range_in<D>.
 *
 * @tparam D Type containing `name` member used to identify the `domain`
 * to which the `unique_range_in` belongs. Else, `domain::global` to
 * indicate that the global NVTX domain should be used.
 * @param[in] attr `event_attributes` that describes the desired attributes
 * of the mark.
 */
template <typename D = domain::global>
inline void mark_in(event_attributes const& attr) noexcept
{
#ifndef NVTX_DISABLE
  nvtxDomainMarkEx(domain::get<D>(), attr.get());
#else
  (void)(attr);
#endif
}

/**
 * @brief Annotates an instantaneous point in time with a "marker", using the
 * arguments to construct an `event_attributes`.
 *
 * Unlike a "range" which has a beginning and an end, a marker is a single event
 * in an application, such as detecting a problem:
 *
 * \code{.cpp}
 * bool success = do_operation(...);
 * if (!success) {
 *    nvtx3::mark_in<my_domain>("operation failed!", nvtx3::rgb{255,0,0});
 * }
 * \endcode
 *
 * Note that nvtx3::mark_in<D> is a function, not a class like scoped_range_in<D>.
 *
 * Forwards the arguments `args...` to construct an `event_attributes` object.
 * The attributes are then associated with the marker. For more detail, see
 * the `event_attributes` documentation.
 *
 * @tparam D Type containing `name` member used to identify the `domain`
 * to which the `unique_range_in` belongs. Else `domain::global` to
 * indicate that the global NVTX domain should be used.
 * @param[in] args Variadic parameter pack of arguments to construct an `event_attributes`
 * associated with this range.
 *
 */
template <typename D = domain::global, typename... Args>
inline void mark_in(Args const&... args) noexcept
{
#ifndef NVTX_DISABLE
  mark_in<D>(event_attributes{args...});
#endif
}

/**
 * @brief Annotates an instantaneous point in time with a "marker", using the
 * attributes specified by `attr`, in the global domain.
 *
 * Unlike a "range" which has a beginning and an end, a marker is a single event
 * in an application, such as detecting a problem:
 *
 * \code{.cpp}
 * bool success = do_operation(...);
 * if (!success) {
 *    nvtx3::event_attributes attr{"operation failed!", nvtx3::rgb{255,0,0}};
 *    nvtx3::mark(attr);
 * }
 * \endcode
 *
 * Note that nvtx3::mark is a function, not a class like scoped_range.
 *
 * @param[in] attr `event_attributes` that describes the desired attributes
 * of the mark.
 */
inline void mark(event_attributes const& attr) noexcept
{
#ifndef NVTX_DISABLE
  mark_in<domain::global>(attr);
#endif
}

/**
 * @brief Annotates an instantaneous point in time with a "marker", using the
 * arguments to construct an `event_attributes`, in the global domain.
 *
 * Unlike a "range" which has a beginning and an end, a marker is a single event
 * in an application, such as detecting a problem:
 *
 * \code{.cpp}
 * bool success = do_operation(...);
 * if (!success) {
 *    nvtx3::mark("operation failed!", nvtx3::rgb{255,0,0});
 * }
 * \endcode
 *
 * Note that nvtx3::mark is a function, not a class like scoped_range.
 *
 * Forwards the arguments `args...` to construct an `event_attributes` object.
 * The attributes are then associated with the marker. For more detail, see
 * the `event_attributes` documentation.
 *
 * @param[in] args Variadic parameter pack of arguments to construct an
 * `event_attributes` associated with this range.
 *
 */
template <typename... Args>
inline void mark(Args const&... args) noexcept
{
#ifndef NVTX_DISABLE
  mark_in<domain::global>(args...);
#endif
}

}  // namespace NVTX3_VERSION_NAMESPACE

}  // namespace nvtx3

#ifndef NVTX_DISABLE
/**
 * @brief Convenience macro for generating a range in the specified `domain`
 * from the lifetime of a function
 *
 * This macro is useful for generating an NVTX range in `domain` from
 * the entry point of a function to its exit. It is intended to be the first
 * line of the function.
 *
 * Constructs a static `registered_string_in` using the name of the immediately
 * enclosing function returned by `__func__` and constructs a
 * `nvtx3::scoped_range` using the registered function name as the range's
 * message.
 *
 * Example:
 * \code{.cpp}
 * struct my_domain{static constexpr char const* name{"my_domain"};};
 *
 * void foo(...) {
 *    NVTX3_FUNC_RANGE_IN(my_domain); // Range begins on entry to foo()
 *    // do stuff
 *    ...
 * } // Range ends on return from foo()
 * \endcode
 *
 * @param[in] D Type containing `name` member used to identify the
 * `domain` to which the `registered_string_in` belongs. Else,
 * `domain::global` to  indicate that the global NVTX domain should be used.
 */
#define NVTX3_V1_FUNC_RANGE_IN(D)                                                  \
  static ::nvtx3::v1::registered_string_in<D> const nvtx3_func_name__{__func__};   \
  static ::nvtx3::v1::event_attributes const nvtx3_func_attr__{nvtx3_func_name__}; \
  ::nvtx3::v1::scoped_range_in<D> const nvtx3_range__{nvtx3_func_attr__};

/**
 * @brief Convenience macro for generating a range in the specified `domain`
 * from the lifetime of a function if the given boolean expression evaluates
 * to true.
 *
 * Similar to `NVTX3_V1_FUNC_RANGE_IN(D)`, the only difference being that
 * `NVTX3_V1_FUNC_RANGE_IF_IN(D, C)` only generates a range if the given boolean
 * expression evaluates to true.
 *
 * @param[in] D Type containing `name` member used to identify the
 * `domain` to which the `registered_string_in` belongs. Else,
 * `domain::global` to indicate that the global NVTX domain should be used.
 *
 * @param[in] C Boolean expression used to determine if a range should be
 * generated.
 */
#define NVTX3_V1_FUNC_RANGE_IF_IN(D, C) \
  ::nvtx3::v1::detail::optional_scoped_range_in<D> optional_nvtx3_range__;           \
  if (C) {                                                                           \
    static ::nvtx3::v1::registered_string_in<D> const nvtx3_func_name__{__func__};   \
    static ::nvtx3::v1::event_attributes const nvtx3_func_attr__{nvtx3_func_name__}; \
    optional_nvtx3_range__.begin(nvtx3_func_attr__);                                 \
  }
#else
#define NVTX3_V1_FUNC_RANGE_IN(D)
#define NVTX3_V1_FUNC_RANGE_IF_IN(D, C)
#endif  // NVTX_DISABLE

/**
 * @brief Convenience macro for generating a range in the global domain from the
 * lifetime of a function.
 *
 * This macro is useful for generating an NVTX range in the global domain from
 * the entry point of a function to its exit. It is intended to be the first
 * line of the function.
 *
 * Constructs a static `registered_string_in` using the name of the immediately
 * enclosing function returned by `__func__` and constructs a
 * `nvtx3::scoped_range` using the registered function name as the range's
 * message.
 *
 * Example:
 * \code{.cpp}
 * void foo(...) {
 *    NVTX3_FUNC_RANGE(); // Range begins on entry to foo()
 *    // do stuff
 *    ...
 * } // Range ends on return from foo()
 * \endcode
 */
#define NVTX3_V1_FUNC_RANGE() NVTX3_V1_FUNC_RANGE_IN(::nvtx3::v1::domain::global)

/**
 * @brief Convenience macro for generating a range in the global domain from the
 * lifetime of a function if the given boolean expression evaluates to true.
 *
 * Similar to `NVTX3_V1_FUNC_RANGE()`, the only difference being that
 * `NVTX3_V1_FUNC_RANGE_IF(C)` only generates a range if the given boolean
 * expression evaluates to true.
 *
 * @param[in] C Boolean expression used to determine if a range should be
 * generated.
 */
#define NVTX3_V1_FUNC_RANGE_IF(C) NVTX3_V1_FUNC_RANGE_IF_IN(::nvtx3::v1::domain::global, C)

/* When inlining this version, versioned macros must have unversioned aliases.
 * For each NVTX3_Vx_ #define, make an NVTX3_ alias of it here.*/
#if defined(NVTX3_INLINE_THIS_VERSION)
/* clang format off */
#define NVTX3_FUNC_RANGE       NVTX3_V1_FUNC_RANGE
#define NVTX3_FUNC_RANGE_IF    NVTX3_V1_FUNC_RANGE_IF
#define NVTX3_FUNC_RANGE_IN    NVTX3_V1_FUNC_RANGE_IN
#define NVTX3_FUNC_RANGE_IF_IN NVTX3_V1_FUNC_RANGE_IF_IN
/* clang format on */
#endif

#endif  // NVTX3_CPP_DEFINITIONS_V1_0

/* Add functionality for new minor versions here, by copying the above section enclosed
 * in #ifndef NVTX3_CPP_DEFINITIONS_Vx_y, and incrementing the minor version.  This code
 * is an example of how additions for version 1.2 would look, indented for clarity.  Note
 * that the versioned symbols and macros are always provided, and the unversioned symbols
 * are only provided if NVTX3_INLINE_THIS_VERSION was defined at the top of this header.
 *
 * \code{.cpp}
 * #ifndef NVTX3_CPP_DEFINITIONS_V1_2
 * #define NVTX3_CPP_DEFINITIONS_V1_2
 *     namespace nvtx3 {
 *         NVTX3_INLINE_IF_REQUESTED namespace NVTX3_VERSION_NAMESPACE {
 *             class new_class {};
 *             inline void new_function() {}
 *         }
 *     }
 *
 *     // Macros must have the major version in their names:
 *     #define NVTX3_V1_NEW_MACRO_A() ...
 *     #define NVTX3_V1_NEW_MACRO_B() ...
 *
 *     // If inlining, make aliases for the macros with the version number omitted
 *     #if defined(NVTX3_INLINE_THIS_VERSION)
 *         #define NVTX3_NEW_MACRO_A NVTX3_V1_NEW_MACRO_A
 *         #define NVTX3_NEW_MACRO_B NVTX3_V1_NEW_MACRO_B
 *     #endif
 * #endif // NVTX3_CPP_DEFINITIONS_V1_2
 * \endcode
 */

/* Undefine all temporarily-defined unversioned macros, which would conflict with
 * subsequent includes of different versions of this header. */
#undef NVTX3_CPP_VERSION_MAJOR
#undef NVTX3_CPP_VERSION_MINOR
#undef NVTX3_CONCAT
#undef NVTX3_NAMESPACE_FOR
#undef NVTX3_VERSION_NAMESPACE
#undef NVTX3_INLINE_IF_REQUESTED
#undef NVTX3_CONSTEXPR_IF_CPP14

#if defined(NVTX3_INLINE_THIS_VERSION)
#undef NVTX3_INLINE_THIS_VERSION
#endif

#if defined(NVTX3_USE_CHECKED_OVERLOADS_FOR_GET_DEFINED_HERE)
#undef NVTX3_USE_CHECKED_OVERLOADS_FOR_GET_DEFINED_HERE
#undef NVTX3_USE_CHECKED_OVERLOADS_FOR_GET
#endif

#if defined(NVTX3_STATIC_ASSERT_DEFINED_HERE)
#undef NVTX3_STATIC_ASSERT_DEFINED_HERE
#undef NVTX3_STATIC_ASSERT
#endif
