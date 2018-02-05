#ifdef WIN32
#include <winsock2.h>
#include <winsock.h>
#include <ws2tcpip.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <unistd.h>
#endif

#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>

int _socket(int domain, int type, int protocol)
{
#ifdef WIN32
  WSADATA wsadata;
  if (WSAStartup(MAKEWORD(1, 1), &wsadata) == SOCKET_ERROR)
  {
    printf("WSAStartup() fail\n");
    exit(0);
  }
#endif
  return socket(domain, type, protocol);
}

int _bind(int socket, int family, char *addr, int port)
{
  struct sockaddr_in serv_addr;
  memset(&serv_addr, 0, sizeof(serv_addr));    //每个字节都用0填充
  serv_addr.sin_family = family;               //使用IPv4地址
  serv_addr.sin_addr.s_addr = inet_addr(addr); //具体的IP地址
  serv_addr.sin_port = htons(port);            //端口
  return bind(socket, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
}

int _listen(int socket, int backlog)
{
  return listen(socket, backlog);
}

int _accept(int socket)
{
  struct sockaddr_un sun;
  int length;
  length = sizeof(sun.sun_family) + sizeof(sun.sun_path);
  return accept(socket, &sun, &length);
}

int _connect(int socket, int family, char *addr, int port)
{
  struct sockaddr_in serv_addr;
  memset(&serv_addr, 0, sizeof(serv_addr));    //每个字节都用0填充
  serv_addr.sin_family = family;               //使用IPv4地址
  serv_addr.sin_addr.s_addr = inet_addr(addr); //具体的IP地址
  serv_addr.sin_port = htons(port);            //端口
  return connect(socket, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
}

int _write(int fd, char *buf, ssize_t start, ssize_t n)
{
  ssize_t i, m;

  buf += start;
  m = n;
  while (m > 0)
  {
#ifdef WIN32
    if ((i = send(fd, buf, m, 0)) < 0)
    {
#else
    if ((i = write(fd, buf, m)) < 0)
    {
#endif
      if (errno != EAGAIN && errno != EINTR)
        return i;
    }
    else
    {
      m -= i;
      buf += i;
    }
  }
  return n;
}

/* c_read pushes through interrupts and socket delays */
int _read(int fd, char *buf, size_t start, size_t n)
{
  int i;

  buf += start;
  for (;;)
  {
#ifdef WIN32
    i = recv(fd, buf, n, 0);
#else
    i = read(fd, buf, n);
#endif
    if (i >= 0)
      return i;
    if (errno != EAGAIN && errno != EINTR)
      return -1;
  }
}

int _close(int fd)
{
  return close(fd);
}

int _shutdown(int socket, int how){
  return shutdown(socket,how);
}

int _cleanup(){
  int i = 0;
  #ifdef WIN32
    i = WSACleanup();
  #endif
  return i;
}

/* get_error returns the operating system's error status */
char *get_error(void)
{
  extern int errno;
  return strerror(errno);
}

// size_t _strlen(const char *s)
// {
//   return strlen(s);
// }

// ssize_t _write_all(int fd, const char *buf, size_t nbyte)
// {
//   ssize_t i, m;
//   m = nbyte;
//   while (m > 0)
//   {
// #ifdef WIN32
//     if ((i = send(fd, buf, m, 0)) < 0)
//     {
// #else
//     if ((i = write(fd, buf, m)) < 0)
//     {
// #endif
//       if (errno != EAGAIN && errno != EINTR)
//         return i;
//     }
//     else
//     {
//       m -= i;
//       buf += i;
//     }
//   }
//   return nbyte;
// }

// FILE *_fdopen(int fildes, const char *mode)
// {
//   return fdopen(fildes, mode);
// }

// int _getpeername(int socket, struct sockaddr *address,
//                  socklen_t *address_len)
// {
//   return getpeername(socket, address, address_len);
// }

// int _getsockname(int socket, struct sockaddr *address,
//                  socklen_t *address_len)
// {
//   return getsockname(socket, address, address_len);
// }

// int _getsockopt(int socket, int level, int option_name,
//                 void *option_value, socklen_t *option_len)
// {
//   return getsockopt(socket, level, option_name, option_value, option_len);
// }

// ssize_t _recv(int socket, void *buffer, size_t length, int flags)
// {
//   return recv(socket, buffer, length, flags);
// }

// ssize_t _recvfrom(int socket, void *buffer, size_t length,
//                   int flags, struct sockaddr *address, socklen_t *address_len)
// {
//   return recvfrom(socket, buffer, length,
//                   flags, address, address_len);
// }

// ssize_t _recvmsg(int socket, struct msghdr *message, int flags)
// {
// #ifdef WIN32

// #else
//   return recvmsg(socket, message, flags);
// #endif
// }

// ssize_t _send(int socket, const void *message, size_t length, int flags)
// {
//   return send(socket, message, length, flags);
// }

// ssize_t _sendmsg(int socket, const struct msghdr *message, int flags)
// {
// #ifdef WIN32
// #else
//   return sendmsg(socket, message, flags);
// #endif
// }

// ssize_t _sendto(int socket, const void *message, size_t length, int flags,
//                 const struct sockaddr *dest_addr, socklen_t dest_len)
// {
//   return sendto(socket, message, length, flags,
//                 dest_addr, dest_len);
// }

// int _setsockopt(int socket, int level, int option_name,
//                 const void *option_value, socklen_t option_len)
// {
//   return setsockopt(socket, level, option_name,
//                     option_value, option_len);
// }

// int _shutdown(int socket, int how)
// {
// #ifdef WIN32
//   WSACleanup();
// #endif
//   return shutdown(socket, how);
// }

// int _socketpair(int domain, int type, int protocol,
//                 int socket_vector[2])
// {
// #ifdef WIN32
// #else
//   return socketpair(domain, type, protocol, socket_vector);
// #endif
// }
