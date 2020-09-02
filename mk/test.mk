.PHONY:		all
.SUFFIXES: .cons

all: .tested

.tested: $(TESTS) $(COMPILER_PATH)
	$(BIN_DIR)/runtests.sh $(TESTS) 2>/dev/null && touch .tested

.cons: $(COMPILER_PATH) $(LIB_DIR)/entry.o $(LIB_DIR)/libcons.a
	$(COMPILER_PATH) $< -o $@ $(COMPILER_FLAGS)

clean:
	rm -f $(TESTS) .tested

