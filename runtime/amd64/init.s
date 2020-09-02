    .text
    .align  16, 0x90
    .globl  __init
    .type   __init, @function
__init:
    .cfi_startproc
    movq    _stack_base@GOTPCREL(%rip), %rbx
    leaq    8(%rsp), %r11
    movq    %r11, (%rbx)
    ret
    .size   __init, .-__init
    .cfi_endproc

    .data
    .align  16
    .globl  _stack_base
    .type   _stack_base, @object
    .size   _stack_base, 8
_stack_base:
    .quad   0

	.section	.note.GNU-stack,"",@progbits
