#if defined(__linux__) || defined(__FreeBSD__) || defined(__ELF__)
.size CNAME, . - CNAME
#else
#ifndef _MSC_VER
.end
#else
CNAME endp
end
#endif
#endif

#undef CNAME
#undef HIDENAME
#undef STR
#undef XSTR
#undef _START_ENTRY
#ifndef __APPLE__
#undef EXT_
#undef EXT
#endif
