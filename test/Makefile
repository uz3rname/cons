ROOT	= ..
include $(ROOT)/mk/config.mk

SRCS			!= ls *.cons
TESTS			= $(SRCS:%.cons=%)
COMPILER_FLAGS	+= -toplevel main -lc -lcons

include $(ROOT)/mk/test.mk
include makefile.dep

