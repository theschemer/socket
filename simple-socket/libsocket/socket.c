#ifdef __CYGWIN__
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
#ifdef __CYGWIN__
  WSADATA wsadata;
  if (WSAStartup(MAKEWORD(2, 2), &wsadata) == SOCKET_ERROR)
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
  struct sockaddr_in clnt_addr;
  socklen_t clnt_addr_size = sizeof(clnt_addr);
  return accept(socket, (struct sockaddr *)&clnt_addr, &clnt_addr_size);
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
#ifdef __CYGWIN__
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
#ifdef __CYGWIN__
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
#ifdef __CYGWIN__
  return closesocket(fd);
#else
  return close(fd);
#endif
}

int _shutdown(int socket, int how)
{
  return shutdown(socket, how);
}

int _cleanup()
{
  int i = 0;
#ifdef __CYGWIN__
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
