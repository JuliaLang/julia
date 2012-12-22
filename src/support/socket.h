#ifndef LLTSOCKET_H
#define LLTSOCKET_H

#ifdef WIN32
#include <winsock2.h>
#else
#include <netinet/in.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <ifaddrs.h>
#include <netinet/in.h> 
#include <arpa/inet.h>
#endif

int open_tcp_port(short portno);
DLLEXPORT int open_any_tcp_port(short *portno);
DLLEXPORT int open_any_udp_port(short *portno);
DLLEXPORT int connect_to_host(char *hostname, short portno);
DLLEXPORT void getlocalip(char *buf, size_t len);
int connect_to_addr(struct sockaddr_in *host_addr);

#ifdef WIN32
void bzero(void *s, size_t n);
#endif
#ifndef WIN32
void closesocket(int fd);
#endif

#endif
