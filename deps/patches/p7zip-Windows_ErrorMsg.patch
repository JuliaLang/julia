This fixes the build with Clang 6.0:

 ../../../../CPP/Windows/ErrorMsg.cpp:24:10: error: case value evaluates to -2147024809, which cannot be narrowed to type 'DWORD' (aka  'unsigned int') [-Wc++11-narrowing]
     case E_INVALIDARG          : txt = "E_INVALIDARG"; break ;
          ^
 ../../../../CPP/Common/MyWindows.h:89:22: note: expanded from macro 'E_INVALIDARG'
 #define E_INVALIDARG ((HRESULT)0x80070057L)
                      ^

The HRESULT cast in the macro causes the value to be read as signed int.
--- CPP/Windows/ErrorMsg.cpp.orig	2015-01-18 18:20:28 UTC
+++ CPP/Windows/ErrorMsg.cpp
@@ -15,13 +15,13 @@ UString MyFormatMessage(DWORD errorCode)
 
   switch(errorCode) {
     case ERROR_NO_MORE_FILES   : txt = "No more files"; break ;
-    case E_NOTIMPL             : txt = "E_NOTIMPL"; break ;
-    case E_NOINTERFACE         : txt = "E_NOINTERFACE"; break ;
-    case E_ABORT               : txt = "E_ABORT"; break ;
-    case E_FAIL                : txt = "E_FAIL"; break ;
-    case STG_E_INVALIDFUNCTION : txt = "STG_E_INVALIDFUNCTION"; break ;
-    case E_OUTOFMEMORY         : txt = "E_OUTOFMEMORY"; break ;
-    case E_INVALIDARG          : txt = "E_INVALIDARG"; break ;
+    case (DWORD)(E_NOTIMPL)             : txt = "E_NOTIMPL"; break ;
+    case (DWORD)(E_NOINTERFACE)         : txt = "E_NOINTERFACE"; break ;
+    case (DWORD)(E_ABORT)               : txt = "E_ABORT"; break ;
+    case (DWORD)(E_FAIL)                : txt = "E_FAIL"; break ;
+    case (DWORD)(STG_E_INVALIDFUNCTION) : txt = "STG_E_INVALIDFUNCTION"; break ;
+    case (DWORD)(E_OUTOFMEMORY)         : txt = "E_OUTOFMEMORY"; break ;
+    case (DWORD)(E_INVALIDARG)          : txt = "E_INVALIDARG"; break ;
     case ERROR_DIRECTORY          : txt = "Error Directory"; break ;
     default:
       txt = strerror(errorCode);
