    .text
    .align  16, 0x90
    .globl  _call_cc
    .type   _call_cc, @function
_call_cc:
    .cfi_startproc
    pushq   %rbp
	.cfi_def_cfa_offset 16
    movq    %rsp, %rbp
	.cfi_def_cfa_register %rbp
    subq    $32, %rsp
    pushfq
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %r11
    pushq   %r10
    pushq   %r9
    pushq   %r8
    pushq   %rsi
    pushq   %rdi
    pushq   %rdx
    pushq   %rcx
    pushq   %rbx
    movq    %rdi, -8(%rbp)
    movq    _stack_base@GOTPCREL(%rip), %rdi
    movq    (%rdi), %rdi
    subq    %rbp, %rdi
    movq    %rdi, -16(%rbp)
    callq   mem_alloc@PLT
    movq    %rax, -24(%rbp)
    movq    %rax, %rdi
    movq    %rbp, %rsi
    movq    -16(%rbp), %rdx
    callq   memcpy@PLT
    movq    $144, %rdi
    callq   mem_alloc@PLT
    movq    %rax, %rdi
    popq    32(%rdi)
    popq    40(%rdi)
    popq    48(%rdi)
    popq    56(%rdi)
    popq    64(%rdi)
    popq    72(%rdi)
    popq    80(%rdi)
    popq    88(%rdi)
    popq    96(%rdi)
    popq    104(%rdi)
    popq    112(%rdi)
    popq    120(%rdi)
    popq    128(%rdi)
    popq    136(%rdi)
    movq    _cont@GOTPCREL(%rip), %rax
    movq    %rax, (%rdi)
    movq    -16(%rbp), %r11
    movq    %r11, 8(%rdi)
    movq    -24(%rbp), %r11
    movq    %r11, 16(%rdi)
    movq    %rbp, 24(%rdi)
    movq    -8(%rbp), %rax
    leaq    8(%rax), %r10
    movq    (%rax), %rax
    callq   *%rax
    leave
    ret
    .size   _call_cc, .-_call_cc
    .cfi_endproc

    .globl  _cont
    .type   _cont, @function
_cont:
    .cfi_startproc
    pushq   %rbp
	.cfi_def_cfa_offset 16
    movq    %rsp, %rbp
	.cfi_def_cfa_register %rbp
    movq    %rdi, %rax
    movq    16(%r10), %rbp
    movq    (%r10), %rdx
    movq    8(%r10), %rsi
    movq    %rbp, %rdi
.L0:
    cmp     $0, %rdx
    je      .L1
    movq    (%rsi), %r11
    movq    %r11, (%rdi)
    subq    $8, %rdx
    addq    $8, %rdi
    addq    $8, %rsi
    jmp     .L0
.L1:
    movq    %rbp, %rsp
    pushq   128(%r10)
    popfq
    movq    24(%r10), %rbx
    movq    32(%r10), %rcx
    movq    40(%r10), %rdx
    movq    48(%r10), %rdi
    movq    56(%r10), %rsi
    movq    64(%r10), %r8
    movq    72(%r10), %r9
    movq    88(%r10), %r11
    movq    96(%r10), %r12
    movq    104(%r10), %r13
    movq    112(%r10), %r14
    movq    120(%r10), %r15
    movq    80(%r10), %r10
    leave
    ret
    .size   _cont, .-_cont
    .cfi_endproc

	.section	.note.GNU-stack,"",@progbits
