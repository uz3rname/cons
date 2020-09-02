ROOT		= ..
include $(ROOT)/mk/config.mk

.SUFFIXES: .o .scm .c

TARGET			= $(COMPILER_PATH)
INSTALL_SRCS	= $(TARGET)
INSTALL_DIR		= bin
LDFLAGS			+= -lelf -lchicken

all: $(TARGET)

SRCS			!= echo *.scm
C_SRCS			!= echo *.c

include $(ROOT)/mk/install.mk

OBJS			= $(SRCS:%.scm=%.o) $(C_SRCS:%.c=%.o)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $@

clean:
	rm -f $(TARGET) $(OBJS) $(SRCS:%.scm=%.c)

include makefile.dep

