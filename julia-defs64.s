; -*- llvm -*-

; definitions from julia.h for use by llvm code

%struct._jl_type_t = type { %struct._jl_type_t* }
%struct._jl_value_t = type { %struct._jl_type_t* }
%jl_callable_t = type %struct._jl_value_t* (%struct._jl_value_t*, %struct._jl_value_t**, i32)
%jl_fptr_t = type %jl_callable_t*
%0 = type { i64, i8**, [32 x i8*] }
%1 = type { i64, i64, i8**, [29 x i8*] }
%2 = type { %struct._jl_type_t*, %struct._jl_sym_t*, %struct._jl_typector_t* }
%3 = type { %struct._jl_type_t*, i64, [1 x %struct._jl_type_t*] }
%jl_tuple_t = type { %struct._jl_type_t*, i64, [1 x %struct._jl_value_t*] }
%jl_buffer_t = type { %struct._jl_type_t*, i64, i8* }
%4 = type { %struct._jl_type_t*, %jl_fptr_t, %struct._jl_type_t*, %5* }
%jl_function_t = type %4
%5 = type { %struct._jl_type_t*, %jl_fptr_t, %struct._jl_value_t*, %jl_tuple_t* }
%jl_lambda_info = type %5
%6 = type { %struct._jl_type_t*, %struct._jl_type_t*, %struct._jl_type_t* }
%7 = type { %struct._jl_type_t*, %2*, %struct._jl_tag_type_t*, %3*, %4*, i64, i64 }
%8 = type { %struct._jl_type_t*, %3* }
%9 = type { %struct._jl_type_t*, %struct._jl_sym_t*, %struct._jl_type_t*, %struct._jl_type_t* }
%10 = type { %struct._jl_type_t*, i64, i8* }
%struct._jl_module_t = type { %struct._jl_sym_t*, %0, %0, %1 }
%struct._jl_sym_t = type { %struct._jl_type_t*, %struct._jl_sym_t*, %struct._jl_sym_t*, i64, %union.anon }
%struct._jl_tag_type_t = type { %struct._jl_type_t*, %2*, %struct._jl_tag_type_t*, %3* }
%struct._jl_typector_t = type { %struct._jl_type_t*, %3*, %struct._jl_type_t* }
%struct._typekey_stack_t = type { %struct._jl_type_t**, i64, %struct._jl_type_t*, %struct._typekey_stack_t* }
%struct.anon = type { %struct._jl_type_t*, %2*, %struct._jl_tag_type_t*, %3*, %3*, %3*, %4*, %4*, i64 }
%union.anon = type { i8* }
