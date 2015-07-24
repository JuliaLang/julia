// This file is a part of Julia. License is MIT: http://julialang.org/license

// This is a compatibility implementation of the unix "socketpair"
// function using the Windows Sockets 2 API.
// Note that the return value is a WSA SOCKET (void*),
// not a fd (int) since the msvcrt handling of
// sockets-as-file-descriptors is generally buggy,
// and not a Win32 HANDLE (void*) either,
// since this is not a Win32 API call
// (despite MSDN documentation to the contrary,
// it will not work with ReadFile/WriteFile)

#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <io.h>
#define AF_UNIX 1

__declspec(dllexport) int wsasocketpair(int domain, int type, int protocol, SOCKET socket_vector[2]) {
    SOCKET server, client0, client1;
    SOCKADDR_IN name;
    int namelen;
    int err;
    if (domain != AF_UNIX) {
        WSASetLastError(WSAEAFNOSUPPORT);
        return -1;
    }
    server = socket(AF_INET, type, protocol);
    if (server == INVALID_SOCKET) {
        return -1;
    }
    name.sin_family = AF_INET;
    name.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    name.sin_port = 0;
    err = bind(server, (SOCKADDR*)&name, sizeof(name));
    if (err == SOCKET_ERROR) {
        int lasterr = WSAGetLastError();
        closesocket(server);
        WSASetLastError(lasterr);
        return -1;
    }
    err = listen(server, 1);
    if (err == SOCKET_ERROR) {
        int lasterr = WSAGetLastError();
        closesocket(server);
        WSASetLastError(lasterr);
        return -1;
    }
    namelen = sizeof(name);
    err = getsockname(server, (SOCKADDR*)&name, &namelen);
    if (err == SOCKET_ERROR) {
        int lasterr = WSAGetLastError();
        closesocket(server);
        WSASetLastError(lasterr);
        return -1;
    }
    client0 = socket(AF_INET, type, protocol);
    if (client0 == INVALID_SOCKET) {
        int lasterr = WSAGetLastError();
        closesocket(server);
        WSASetLastError(lasterr);
        return -1;
    }
    err = connect(client0, (SOCKADDR*)&name, sizeof(name));
    if (err == SOCKET_ERROR) {
        int lasterr = WSAGetLastError();
        closesocket(client0);
        closesocket(server);
        WSASetLastError(lasterr);
        return -1;
    }
    client1 = accept(server, NULL, NULL);
    if (err == INVALID_SOCKET) {
        int lasterr = WSAGetLastError();
        closesocket(server);
        closesocket(client0);
        WSASetLastError(lasterr);
        return -1;
    }
    closesocket(server);
    socket_vector[0] = client0;
    socket_vector[1] = client1;
    return 0;
}
