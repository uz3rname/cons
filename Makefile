include mk/config.mk

.PHONY: bootstrap test1 test2 test lib vim install uninstall runtime

TARGETS				+= bootstrap lib runtime
INSTALL_TARGETS		+= bootstrap lib

all: $(TARGETS)

bootstrap:
	@echo
	@echo '>>> Building compiler'
	@echo
	@$(MAKE) -C bootstrap

lib: bootstrap test1
	@echo
	@echo '>>> Building libraries'
	@echo
	@$(MAKE) -C lib

runtime:
	@echo
	@echo '>>> Building runtime'
	@echo
	@$(MAKE) -C runtime

vim:
	@echo
	@echo '>>> Making vim scripts'
	@echo
	@$(MAKE) -C vim

install: all
	@echo
	@echo ">>> Installing into $(DESTDIR)$(prefix)"
	@echo
	@for t in $(INSTALL_TARGETS); do\
		if test x != x"$(DESTDIR)"; then\
			mkdir -p "$(DESTDIR)";\
			_DESTDIR=`realpath "$(DESTDIR)"`;\
		fi;\
		$(MAKE) -C $$t install DESTDIR="$$_DESTDIR";\
	done

uninstall:
	@for t in $(INSTALL_TARGETS); do\
		if test x != x"$(DESTDIR)"; then\
			_DESTDIR=`realpath -q "$(DESTDIR)"`;\
		fi;\
		$(MAKE) -C $$t uninstall DESTDIR="$$_DESTDIR";\
	done

clean:
	for t in $(TARGETS) test test2; do\
		$(MAKE) -C $$t clean; done

distclean: clean
	rm -rf $(CONFIG_FILES) autom4te.cache/ config.log config.status
	find . -name makefile.dep |xargs rm -f

test: test1 test2

test1: bootstrap runtime
	@echo
	@echo '>>> Testing sanity of the compiler'
	@echo
	@$(MAKE) -C test

test2: lib runtime
	@echo
	@echo '>>> Testing libs'
	@echo
	@$(MAKE) -C test2

retest:
	find . -name .tested|xargs rm -f
	$(MAKE) all

