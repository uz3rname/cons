m4_define([AC_ASSERT_PROGS],
          [{
               AC_CHECK_PROGS($1, $2, [no])
               AS_VAR_IF($1,
                         "no",
                         [AC_MSG_ERROR([Required program was not found])])
           }])

AC_INIT([cons], [0.1])
if test x"$CFLAGS" == "x"; then
    CFLAGS='-Os'
fi

AC_ASSERT_PROGS([CC], [cc])
AC_CHECK_HEADERS([fcntl.h libelf.h gelf.h])
AC_CHECK_HEADER_STDBOOL
AC_TYPE_SIZE_T

AC_ASSERT_PROGS([CSC], [csc])
AC_ASSERT_PROGS([LD], [ld])
AC_ASSERT_PROGS([AS], [as])
AC_ASSERT_PROGS([AR], [ar])
AC_ASSERT_PROGS([NM], [nm])
AC_ASSERT_PROGS([PERL], [perl])
AC_ASSERT_PROGS([PCREGREP], [pcregrep])

scm_impl_version=$(echo chicken `${CSC} -version|grep Version`)
compiler_flags="${COMPILER_FLAGS}"
csc_flags="${CSCFLAGS}"
mem_manager=builtin

AC_ARG_WITH([boehm-gc],
            [AS_HELP_STRING([--without-boehm-gc], [disable boehm-gc])])
AS_IF([test x"$with_boehm_gc" != "xno"],
      [mem_manager="boehm-gc";
       AC_SEARCH_LIBS([GC_malloc],
                      [gc],
                      [],
                      [AC_MSG_ERROR([Couldnt find required library.])])])

AC_ARG_ENABLE(debug,
              [AS_HELP_STRING([--enable-debug], [enable generating of debug symbols])])
AS_VAR_IF(enable_debug,
          "yes",
          [compiler_flags="${compiler_flags} -g"])

AC_ARG_ENABLE(csc-debug,
              [AS_HELP_STRING([--disable-csc-debug], [disable CSC debug])])
AS_VAR_IF(enable_csc_debug,
          "no",
          [csc_flags="${csc_flags} \$(CSC_NODEBUG)"])

AC_ARG_ENABLE(csc-optimization,
              [AS_HELP_STRING([--enable-csc-optimization], [enable CSC optimization options])])
AS_VAR_IF(enable_csc_optimization,
          "yes",
          [csc_flags="${csc_flags} \$(CSC_OPTIMIZE)"])

targets=""
install_targets=""

AC_ARG_WITH(vim-syntax,
            [AS_HELP_STRING([--without-vim-syntax], [do not generate vim syntax file])])
AS_IF([test x"$with_vim_syntax" != "xno"],
      [
       {
           targets="${targets} vim"
           install_targets="${install_targets} vim"
           if test -z $(echo ${prefix} | grep '/home/.*'); then
               vim_dir="share/vim/vimfiles"
           else
               vim_dir=".vim"
           fi
       }])

AC_MSG_CHECKING([errno function name])
errno_func_name=$(echo '#include <errno.h>'\
                  |$CC -E -\
                  |sed '/^#/ d; /^[ ]*$/ d'\
                  |pcregrep -o1 -e 'int\s*\*\s*([[a-zA-Z0-9_]]+)\s*\('\
                  |head -n1)
if test x"$errno_func_name" == "x"; then
    AC_MSG_ERROR([Couldn't determine errno function.])
fi
AC_MSG_RESULT([$errno_func_name])

AC_SEARCH_LIBS([sqrt], [m])
AC_SEARCH_LIBS([elf_begin], [elf])

AC_MSG_CHECKING([OS type])
OS_TYPE=$(uname -s|tr '[[A-Z]]' '[[a-z]]')
AC_MSG_RESULT([$OS_TYPE])
AC_MSG_CHECKING([arch])
ARCH=$(uname -m|sed 's/x86_64/amd64/g')
AC_MSG_RESULT([$ARCH])
case $ARCH in
    amd64)
        ASFLAGS="${ASFLAGS} --64"
        ;;
    i386)
        ASFLAGS="${ASFLAGS} --32"
        ;;
esac

AC_MSG_CHECKING([double value precision])
dbl_dig=$(echo -e '#include <float.h>\nDBL_DIG\n'\
          |$CC -E -\
          |sed '/^#/ d; /^[ ]*$/ d'\
          |tail -n 1)
AC_MSG_RESULT([$dbl_dig])

AC_MSG_CHECKING([dynamic linker path])
ld_so_path=$(echo\
             |$CC -v -x c - -o - 2>&1\
             |pcregrep -o1 -e '-dynamic-linker\s+([[^\s]]+)')
AC_MSG_RESULT([$ld_so_path])

build_date=$(date)

CORE_SRCS=$(find lib/core/ -name '*.cons'|sed 's/^/\.\.\//'|tr $'\n' ' ')
AC_SUBST([OS_TYPE])
AC_SUBST([ARCH])
AC_SUBST([dbl_dig])
AC_SUBST([CORE_SRCS])
AC_SUBST([MEM_MANAGER], [$mem_manager])
AC_SUBST([VIM_DIR], [$vim_dir])
AC_SUBST([TARGETS], [$targets])
AC_SUBST([INSTALL_TARGETS], [$install_targets])
AC_SUBST([errno_func_name])
AC_SUBST([ld_so_path])
AC_SUBST([compiler_flags])
AC_SUBST([csc_flags])
AC_SUBST([ASFLAGS])

AC_SUBST([lib_gc], [$ac_cv_search_GC_malloc])
AC_SUBST([lib_m], [$ac_cv_search_sqrt])
AC_SUBST([lib_elf], [$ac_cv_search_elf_begin])
AC_SUBST([scm_impl_version])
AC_SUBST([build_date])

AC_CONFIG_FILES([vim/Makefile])
AC_CONFIG_FILES([mk/config.mk])
AC_CONFIG_FILES([bootstrap/config.scm])
AC_CONFIG_FILES([lib/core/config.cons])
AC_CONFIG_FILES([lib/math/config.cons])
AC_SUBST([CONFIG_FILES], [$ac_config_files])
AC_OUTPUT

echo
echo ">>> Generating dependencies"
echo

gendeps=$(realpath bin/gendeps.pl)
for f in test lib test2; do
    echo "  ${f}/makefile.dep"
    d="\$(ROOT)/lib"
    case "$f" in
        lib)
            d="."
            ;;
    esac
    cd "${f}"
    $gendeps\
        -I "\$(ROOT)/lib"\
        -L "\$(ROOT)/lib"\
        -d "$d" *.cons > makefile.dep
    cd ..
done

echo "  bootstrap/makefile.dep"
cd bootstrap
for f in *.scm; do
    echo "${f}: ${f}"|sed 's/\.scm/\.o/'
    echo -e "\t\$(CSC) \$(CSCFLAGS) $(head -n1 "$f"|cut -c2-) -c ${f} -o \$@"
done > makefile.dep
$CC -M *.c >> makefile.dep
cd ..

