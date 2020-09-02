ROOT		= ..
include $(ROOT)/mk/config.mk

.SUFFIXES:	.cons .a

LIB_FILES	= $(MODULES:%=%.a)
SRCS		= $(MODULES:%=%.cons)
INSTALL_SRCS= $(LIB_FILES) libcons.a entry.o
INSTALL_DIR	= lib/cons/$(ARCH)

all: $(LIB_FILES)

include makefile.dep

.cons.a: $(COMPILER_PATH)
	$(COMPILER_PATH) -m $(COMPILER_FLAGS) $< -o $@

include $(ROOT)/mk/install.mk

clean:
	rm -f $(LIB_FILES)

