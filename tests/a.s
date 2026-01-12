.globl _main
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    movz w10, #2
    str w10, [x29, #-16]
    movz w10, #3
    str w10, [x29, #-20]
    ldr w10, [x29, #-16]
    str w10, [x29, #-24]
    ldr w11, [x29, #-24]
    ldr w10, [x29, #-20]
    add w11, w11, w10
    str w11, [x29, #-24]
    movz w11, #4
    ldr w10, [x29, #-24]
    cmp w10, w11
    movz w10, #0
    str w10, [x29, #-28]
    cset w10, gt
    str w10, [x29, #-28]
    ldr w11, [x29, #-28]
    movz w10, #0
    cmp w10, w11
    b.eq cond_else6
    movz w10, #2
    str w10, [x29, #-32]
    b cond_end7
cond_else6:
    movz w10, #3
    str w10, [x29, #-32]
cond_end7:
    ldr w10, [x29, #-32]
    str w10, [x29, #-36]
    ldr w10, [x29, #-36]
    str w10, [x29, #-40]
    ldr w11, [x29, #-40]
    ldr w10, [x29, #-20]
    add w11, w11, w10
    str w11, [x29, #-40]
    ldr w0, [x29, #-40]
    b __func_exit
    movz w0, #0
    b __func_exit
__func_exit:
    add sp, sp, #48
    ldp x29, x30, [sp], #16
    ret
