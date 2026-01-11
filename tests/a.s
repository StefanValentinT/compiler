.globl _main
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    add sp, sp, #16
    ldp x29, x30, [sp], #16
    ret
