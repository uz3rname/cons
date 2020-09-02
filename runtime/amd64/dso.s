    .text
    .align 16, 0x90
    .globl __dso_handle
    .type __dso_handle, @function
__dso_handle:
    .cfi_startproc
    ret
    .size __dso_handle, .-__dso_handle
    .cfi_endproc

