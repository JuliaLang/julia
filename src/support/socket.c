#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>

#include "dtypes.h"

#if defined(__APPLE__)
#include <sys/time.h>
#include <sys/select.h>
#include <sys/types.h>
#endif

#include "socket.h"


int mysocket(int domain, int type, int protocol)
{
    int val;
    int s = socket(domain, type, protocol);
    if (s < 0)
        return s;
    val = 131072;
    setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&val, sizeof(int));
    val = 131072;
    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char*)&val, sizeof(int));
    return s;
}

#ifdef WIN32
void bzero(void *s, size_t n)
{
    memset(s, 0, n);
}
#endif

/* returns a socket on which to accept() connections */
int open_tcp_port(short portno)
{
    int sockfd;
    //int val;
    struct sockaddr_in serv_addr;

    sockfd = mysocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sockfd < 0)
        return -1;
    //val = 1;
    //setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(portno);
    if (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        return -1;
    }

    listen(sockfd, 4);
    return sockfd;
}

/* returns a socket on which to accept() connections, finding some
   available port (portno is value-return) */
int open_any_tcp_port(short *portno)

{
    int sockfd;
    //int val;
    struct sockaddr_in serv_addr;

    sockfd = mysocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sockfd < 0)
        return -1;
    //val = 1;
    //setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(*portno);
    while (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        (*portno)++;
        serv_addr.sin_port = htons(*portno);
    }

    listen(sockfd, 4);
    return sockfd;
}

/* returns a socket on which to accept() connections, finding some
   available port (portno is value-return) */
int open_any_udp_port(short *portno)
{
    int sockfd;
    struct sockaddr_in serv_addr;

    sockfd = mysocket(PF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0)
        return -1;
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(*portno);
    while (bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        (*portno)++;
        serv_addr.sin_port = htons(*portno);
    }

    return sockfd;
}

#ifndef WIN32
void closesocket(int fd)
{
    close(fd);
}
#endif

/* returns a socket to use to send data to the given address */
int connect_to_host(char *hostname, short portno)
{
    struct hostent *host_info;
    int sockfd, yes=1;
    struct sockaddr_in host_addr;

    host_info = gethostbyname(hostname);
    if (host_info == NULL) {
        return -1;
    }

    sockfd = mysocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sockfd < 0) {
        return -1;
    }
    (void)setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
    memset((char*)&host_addr, 0, sizeof(host_addr));
    host_addr.sin_family = host_info->h_addrtype;
    memcpy((char*)&host_addr.sin_addr, host_info->h_addr,
           host_info->h_length);

    host_addr.sin_port = htons(portno);

    if (connect(sockfd, (struct sockaddr*)&host_addr,
                sizeof(struct sockaddr_in)) != 0) {
        closesocket(sockfd);
        return -1;
    }

    return sockfd;
}

int connect_to_addr(struct sockaddr_in *host_addr)
{
    int sockfd, yes=1;

    sockfd = mysocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sockfd < 0) {
        return -1;
    }
    (void)setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

    if (connect(sockfd, (struct sockaddr*)host_addr,
                sizeof(struct sockaddr_in)) != 0) {
        closesocket(sockfd);
        return -1;
    }

    return sockfd;
}

void getlocalip(char *buf, size_t len)
{
    struct ifaddrs * ifAddrStruct=NULL;
    struct ifaddrs * ifa=NULL;
    void * tmpAddrPtr=NULL;
    buf[0] = '\0';

    getifaddrs(&ifAddrStruct);

    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr && ifa->ifa_addr->sa_family==AF_INET) { // check it is IP4
            // is a valid IP4 Address
            tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
            inet_ntop(AF_INET, tmpAddrPtr, buf, len);
            if (strcmp(buf,"127.0.0.1"))
                break;
            //printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer); 
        }
        /*
        else if (ifa->ifa_addr && ifa->ifa_addr->sa_family==AF_INET6) { // check it is IP6
            // is a valid IP6 Address
            tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
            char addressBuffer[INET6_ADDRSTRLEN];
            inet_ntop(AF_INET6, tmpAddrPtr, addressBuffer, INET6_ADDRSTRLEN);
            printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer); 
        }
        */
    }
    if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);
}
