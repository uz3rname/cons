    .text
    .globl  _start
    .align  16, 0x90
    .type   _start, @function
_start:
    .cfi_startproc
    popq    %rdi
    movq    %rsp, %rsi
    callq   .L0
.L0:
    pushq   %rbp
	.cfi_def_cfa_offset 16
    movq    %rsp, %rbp
	.cfi_def_cfa_register %rbp
    subq    $8, %rsp
    callq   __init@PLT
    callq   main@PLT
    movq    %rax, %rdi
    callq   exit@PLT
    leave
    ret
    .size   _start, .-_start
    .cfi_endproc

	.section	.note.GNU-stack,"",@progbits
