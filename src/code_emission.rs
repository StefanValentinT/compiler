use crate::asm_gen::*;
use std::{
    fmt::Write,
    process::{Command, exit},
};

pub fn emit_assembly(program: &AsmProgram) -> String {
    let mut output = String::new();

    match program {
        AsmProgram::Program(func) => emit_function(func, &mut output),
    }

    output
}

fn emit_function(func: &AsmFuncDef, out: &mut String) {
    let AsmFuncDef::Function { name, instructions } = func;

    let asm_name = if cfg!(target_os = "macos") {
        format!("_{}", name)
    } else {
        name.clone()
    };

    writeln!(out, ".globl {}", asm_name).unwrap();
    writeln!(out, "{}:", asm_name).unwrap();

    writeln!(out, "    stp x29, x30, [sp, #-16]!").unwrap();
    writeln!(out, "    mov x29, sp").unwrap();

    let mut stack_alloc = 0;

    for instr in instructions {
        if let AsmInstruction::AllocateStack(bytes) = instr {
            stack_alloc = *bytes;
        }
        emit_instruction(instr, out);
    }

    if stack_alloc > 0 {
        writeln!(out, "    add sp, sp, #{}", stack_alloc).unwrap();
    }

    writeln!(out, "    ldp x29, x30, [sp], #16").unwrap();
    writeln!(out, "    ret").unwrap();
}

fn emit_instruction(instr: &AsmInstruction, out: &mut String) {
    match instr {
        AsmInstruction::AllocateStack(bytes) => {
            let aligned = ((bytes + 15) / 16) * 16;
            if aligned > 0 {
                writeln!(out, "    sub sp, sp, #{}", aligned).unwrap();
            }
        }
        AsmInstruction::Mov { src, dest } => match (src, dest) {
            (AsmOperand::Imm(val), AsmOperand::Stack(offset)) => {
                writeln!(out, "    mov w10, #{}", val).unwrap();
                writeln!(out, "    str w10, [x29, #{}]", offset).unwrap();
            }

            (AsmOperand::Imm(val), AsmOperand::Reg(dst_r)) => {
                writeln!(out, "    mov {}, #{}", reg_name(dst_r), val).unwrap();
            }

            (AsmOperand::Reg(r), AsmOperand::Stack(offset)) => {
                writeln!(out, "    str {}, [x29, #{}]", reg_name(r), offset).unwrap();
            }

            (AsmOperand::Stack(offset), AsmOperand::Reg(r)) => {
                writeln!(out, "    ldr {}, [x29, #{}]", reg_name(r), offset).unwrap();
            }

            (AsmOperand::Reg(src_r), AsmOperand::Reg(dst_r)) => {
                writeln!(out, "    mov {}, {}", reg_name(dst_r), reg_name(src_r)).unwrap();
            }

            _ => panic!("Unsupported mov combination: {:?} -> {:?}", src, dest),
        },
        AsmInstruction::Unary(op, operand) => match operand {
            AsmOperand::Reg(r) => {
                let instr = match op {
                    AsmUnaryOp::Neg => "neg",
                    AsmUnaryOp::Not => "mvn",
                };
                writeln!(out, "    {} {}, {}", instr, reg_name(r), reg_name(r)).unwrap();
            }

            AsmOperand::Stack(offset) => {
                writeln!(out, "    ldr w10, [x29, #{}]", offset).unwrap();
                let instr = match op {
                    AsmUnaryOp::Neg => "neg",
                    AsmUnaryOp::Not => "mvn",
                };
                writeln!(out, "    {} w10, w10", instr).unwrap();
                writeln!(out, "    str w10, [x29, #{}]", offset).unwrap();
            }

            _ => panic!("Unary operand must be register or stack"),
        },
        AsmInstruction::Ret => (),
        AsmInstruction::Binary(op, src, dest) => {
            let instr = match op {
                AsmBinaryOp::Add => "add",
                AsmBinaryOp::Subtract => "sub",
                AsmBinaryOp::Multiply => "mul",
            };

            match (src, dest) {
                (AsmOperand::Reg(rs), AsmOperand::Reg(rd)) => {
                    writeln!(
                        out,
                        "    {} {}, {}, {}",
                        instr,
                        reg_name(rd),
                        reg_name(rd),
                        reg_name(rs)
                    )
                    .unwrap();
                }

                (AsmOperand::Stack(src_off), AsmOperand::Stack(dst_off)) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", dst_off).unwrap();
                    writeln!(out, "    ldr w11, [x29, #{}]", src_off).unwrap();

                    writeln!(out, "    {} w10, w10, w11", instr).unwrap();

                    writeln!(out, "    str w10, [x29, #{}]", dst_off).unwrap();
                }

                (src, AsmOperand::Stack(offset)) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", offset).unwrap();

                    match src {
                        AsmOperand::Reg(r) => {
                            writeln!(out, "    {} w10, w10, {}", instr, reg_name(r)).unwrap();
                        }
                        AsmOperand::Imm(val) => {
                            writeln!(out, "    mov w11, #{}", val).unwrap();
                            writeln!(out, "    {} w10, w10, w11", instr).unwrap();
                        }
                        _ => panic!("Invalid binary source"),
                    }

                    writeln!(out, "    str w10, [x29, #{}]", offset).unwrap();
                }

                _ => panic!("Unsupported binary operands"),
            }
        }
        AsmInstruction::Idiv(operand) => {
            match operand {
                AsmOperand::Reg(r) => {
                    writeln!(out, "    sdiv w2, w0, {}", reg_name(r)).unwrap();
                    writeln!(out, "    mul  w3, w2, {}", reg_name(r)).unwrap();
                    writeln!(out, "    sub  w1, w0, w3").unwrap(); // remainder in w1
                    writeln!(out, "    mov  w0, w2").unwrap(); // quotient in w0
                }
                AsmOperand::Stack(offset) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", offset).unwrap();
                    writeln!(out, "    sdiv w2, w0, w10").unwrap();
                    writeln!(out, "    mul  w3, w2, w10").unwrap();
                    writeln!(out, "    sub  w1, w0, w3").unwrap();
                    writeln!(out, "    mov  w0, w2").unwrap();
                }
                _ => panic!("Invalid idiv operand"),
            }
        }

        AsmInstruction::Cdq => {}
        AsmInstruction::Cmp(lhs, rhs) => {
            match (lhs, rhs) {
                // reg, reg
                (AsmOperand::Reg(l), AsmOperand::Reg(r)) => {
                    writeln!(out, "    cmp {}, {}", reg_name(l), reg_name(r)).unwrap();
                }

                // reg, imm
                (AsmOperand::Reg(l), AsmOperand::Imm(val)) => {
                    writeln!(out, "    cmp {}, #{}", reg_name(l), val).unwrap();
                }

                // imm, reg
                (AsmOperand::Imm(val), AsmOperand::Reg(r)) => {
                    writeln!(out, "    cmp {}, #{}", reg_name(r), val).unwrap();
                }

                // stack, reg
                (AsmOperand::Stack(off), AsmOperand::Reg(r)) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap();
                    writeln!(out, "    cmp w10, {}", reg_name(r)).unwrap();
                }

                // reg, stack  <-- fix: load stack into scratch reg
                (AsmOperand::Reg(r), AsmOperand::Stack(off)) => {
                    writeln!(out, "    ldr w11, [x29, #{}]", off).unwrap(); // load memory
                    writeln!(out, "    cmp {}, w11", reg_name(r)).unwrap(); // cmp reg vs reg
                }

                // stack, imm
                (AsmOperand::Stack(off), AsmOperand::Imm(val)) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap();
                    writeln!(out, "    cmp w10, #{}", val).unwrap();
                }

                // imm, stack
                (AsmOperand::Imm(val), AsmOperand::Stack(off)) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap();
                    writeln!(out, "    cmp w10, #{}", val).unwrap();
                }

                _ => panic!("Invalid cmp operands: {:?}, {:?}", lhs, rhs),
            }
        }

        AsmInstruction::SetCC(cc, operand) => match operand {
            AsmOperand::Reg(r) => {
                writeln!(out, "    cset {}, {}", reg_name(r), cond_code_name(cc)).unwrap();
            }

            AsmOperand::Stack(off) => {
                writeln!(out, "    cset w10, {}", cond_code_name(cc)).unwrap();
                writeln!(out, "    str w10, [x29, #{}]", off).unwrap();
            }

            _ => panic!("Invalid setcc operand"),
        },

        AsmInstruction::Jmp(label) => {
            writeln!(out, "    b {}", label).unwrap();
        }

        AsmInstruction::JmpCC(cc, label) => {
            writeln!(out, "    b.{} {}", cond_code_name(cc), label).unwrap();
        }

        AsmInstruction::Label(name) => {
            writeln!(out, "{}:", name).unwrap();
        }
    }
}

fn reg_name(r: &AsmReg) -> &'static str {
    match r {
        AsmReg::Ax => "w0",
        AsmReg::Dx => "w1",
        AsmReg::R10 => "w10",
        AsmReg::R11 => "w11",
    }
}

fn cond_code_name(cc: &AsmCondCode) -> &'static str {
    match cc {
        AsmCondCode::E => "eq",
        AsmCondCode::NE => "ne",
        AsmCondCode::L => "lt",
        AsmCondCode::LE => "le",
        AsmCondCode::G => "gt",
        AsmCondCode::GE => "ge",
    }
}

pub fn assemble_and_link(asm_file: &str, exe_file: &str) {
    let status = Command::new("cc")
        .arg(asm_file)
        .arg("-o")
        .arg(exe_file)
        .status()
        .expect("Failed to invoke system compiler");

    if !status.success() {
        eprintln!("Assembler/linker failed");
        exit(1);
    }
}
