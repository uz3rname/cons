    .text
    .align  16, 0x90
    .globl  _values
    .type   _values, @function
_values:
    .cfi_startproc
    movq    %rdi, %rax
    ret
    .size   _values, .-_values
    .cfi_endproc

    .globl  _call_with_values
    .type   _call_with_values, @function
_call_with_values:
    .cfi_startproc
    push    %rbp
	.cfi_def_cfa_offset 16
    movq    %rsp, %rbp
	.cfi_def_cfa_register %rbp
    subq    $16, %rsp
    movq    %rdi, -8(%rbp)
    movq    %rsi, -16(%rbp)
    leaq    8(%rdi), %r10
    movq    (%rdi), %rax
    callq   *%rax
    movq    -16(%rbp), %rax
    leaq    8(%rax), %r10
    movq    (%rax), %rax
    callq   *%rax
    leave
    ret
    .size   _call_with_values, .-_call_with_values
    .cfi_endproc

	.section	.note.GNU-stack,"",@progbits
