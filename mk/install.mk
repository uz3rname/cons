INSTALL_MODE	?= 0755
INSTALL_PATH	= $(DESTDIR)$(prefix)/$(INSTALL_DIR)

.PHONY: install uninstall

install: all
	@mkdir -p $(INSTALL_PATH)
	@for f in $(INSTALL_SRCS); do\
		$(INSTALL) -v -m $(INSTALL_MODE) "$$f" $(INSTALL_PATH)/"$$f";\
	done

uninstall:
	@for f in $(INSTALL_SRCS); do\
		rm -v -f $(INSTALL_PATH)/"$$f";\
	done

