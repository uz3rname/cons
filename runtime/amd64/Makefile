ROOT	= ../..
include $(ROOT)/mk/config.mk

LIBCONS		= $(LIB_DIR)/libcons.a
ENTRY		= $(LIB_DIR)/entry.o
ENTRY_SRC	= entry/$(OS_TYPE).s
SRCS		!= ls *.s
OBJS		= $(SRCS:%.s=%.o)

.SUFFIXES:	.s .o

all: $(LIBCONS) $(ENTRY)

.s.o:
	$(AS) $(ASFLAGS) $< -o $@

$(LIBCONS): $(OBJS)
	$(AR) rcs $@ $(OBJS)

$(ENTRY): $(ENTRY_SRC)
	$(AS) $(ASFLAGS) $(ENTRY_SRC) -o $@

clean:
	rm -f $(OBJS) $(ENTRY) $(LIBCONS)

