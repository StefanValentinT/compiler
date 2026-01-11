.globl _main
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    mov w10, #1
    str w10, [x29, #-16]
    mov w10, #2
    str w10, [x29, #-20]
    ldr w10, [x29, #-16]
    str w10, [x29, #-24]
    ldr w10, [x29, #-24]
    ldr w11, [x29, #-20]
    add w10, w10, w11
    str w10, [x29, #-24]
    ldr w0, [x29, #-24]
    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret
