##########################################################
# Copyright 2016-2080 evilbinary.
#作者:evilbinary on 12/24/16.
#邮箱:rootdebug@163.com
##########################################################


TARGET = csocket.so
OBJS = csocket.o
PLATFORM=
BITS =$(shell getconf LONG_BIT)


LIBS =
CFLAGS = -Wall -Wformat

CXXFLAGS= -Wall -Wformat

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Linux) #LINUX
	ECHO_MESSAGE = "Linux"
	LIBS += 
	CXXFLAGS += 
	CXXFLAGS += 
	CFLAGS +=  -fPIC
	ifeq ($(BITS), 32)
		PLATFORM=i3le
	else
		PLATFORM=a6le
	endif
endif

ifeq ($(UNAME_S), Darwin) #APPLE
	ECHO_MESSAGE = "Mac OS X"
	LIBS += 
	CXXFLAGS += 
	CXXFLAGS += 
	CFLAGS +=
	ifeq ($(BITS), 32)
		PLATFORM=i3osx
	else
		PLATFORM=a6osx
	endif
#	CXXFLAGS += -D__APPLE__
endif

ifeq ($(UNAME_S), MINGW32_NT-6.1)
   	ECHO_MESSAGE = "Windows"
	LIBS +=  -lws2_32 -lwsock32 
	CXXFLAGS += 
	CXXFLAGS += 
	CFLAGS += 
	PLATFORM=i3nt
	TARGET = libsocket.dll
endif

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<
all: $(TARGET)
	@echo Build complete for $(ECHO_MESSAGE)

$(TARGET): $(OBJS)
	$(CC)  $(CFLAGS) $(OBJS) -shared  -o $(TARGET)   $(LIBS)

clean:
	rm -rf $(TARGET) $(OBJS)

