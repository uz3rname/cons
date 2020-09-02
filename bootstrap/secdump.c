#include <stdlib.h>
#include <string.h>
#include <libelf.h>
#include <gelf.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "inc/decls.h"

static int fd = -1;
static size_t strndx = 0;
static Elf *elf;

int open_elf(const char *path)
{
    struct stat stats;

    if((fd = open(path, O_RDWR)) == -1) {
        return 0;
    }
    if((fstat(fd, &stats))) {
        return 0;
    }
    elf_version(EV_CURRENT);
    elf = elf_begin(fd, ELF_C_READ, NULL);
    elf_getshdrstrndx(elf, &strndx);

    return 1;
}

void close_elf(void)
{
    elf_end(elf);
    close(fd);
}

Elf_Scn * get_scn_by_name(const char *sec_name)
{
    GElf_Shdr shdr;
    Elf_Scn *scn = NULL;

    while((scn = elf_nextscn(elf, scn)) != 0)
    {
        gelf_getshdr(scn, &shdr);
        char *s_name = elf_strptr(elf, strndx, shdr.sh_name);

        if(!strcmp(s_name, sec_name)) {
            return scn;
        }
    }
    return NULL;
}

Elf_Scn * get_scn_by_type(int t)
{
    GElf_Shdr shdr;
    Elf_Scn *scn = NULL;

    while((scn = elf_nextscn(elf, scn)) != 0)
    {
        gelf_getshdr(scn, &shdr);
        if(shdr.sh_type == t) {
            return scn;
        }
    }
    return NULL;
}

char * dump_section(const char *file, const char *sec_name)
{
    Elf_Scn *scn;
    Elf_Data *data;

    if(!open_elf(file)) {
        return NULL;
    }

    scn = get_scn_by_name(sec_name);

    if(!scn) {
        close_elf();
        return NULL;
    }
    data = elf_getdata(scn, NULL);

    char *r = malloc(data->d_size + 1);
    memcpy(r, data->d_buf, data->d_size);
    *(r + data->d_size) = 0;

    close_elf();
    return r;
}

char * dump_symbol(const char *file, const char *sym_name)
{
    GElf_Shdr shdr;
    GElf_Sym sym;
    Elf_Data *data;
    size_t cnt;
    Elf_Scn *scn;
    char *r;

    if(!open_elf(file)) {
        return NULL;
    }

    scn = get_scn_by_type(SHT_SYMTAB);
    if(!scn) {
        close_elf();
        return NULL;
    }

    gelf_getshdr(scn, &shdr);
    cnt = shdr.sh_size / shdr.sh_entsize;
    data = elf_getdata(scn, NULL);

    for(unsigned int i = 0; i < cnt; i++) {
        gelf_getsym(data, i, &sym);
        char *s_name = elf_strptr(elf, shdr.sh_link, sym.st_name);
        if(!strcmp(s_name, sym_name)) {
            scn = elf_getscn(elf, sym.st_shndx);
            data = elf_getdata(scn, NULL);
            r = malloc(sym.st_size + 1);
            memcpy(r, (char*)data->d_buf + sym.st_value, sym.st_size);
            *(r + sym.st_size) = 0;

            close_elf();
            return r;
        }
    }
    close_elf();
    return NULL;
}

