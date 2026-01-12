use crate::asm_gen::*;
use std::fmt::Write;
use std::process::{Command, exit};


pub fn emit_assembly(program: &AsmProgram) -> String {
    let mut out = String::new();
    let AsmProgram::Program(func) = program; 
    emit_function(func, &mut out);
    out
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

    let stack_size: i32 = instructions.iter().fold(0, |acc, instr| match instr {
        AsmInstruction::AllocateStack(bytes) => acc + bytes,
        _ => acc,
    });
    if stack_size > 0 {
        writeln!(out, "    sub sp, sp, #{}", stack_size).unwrap();
    }

    for instr in instructions {
        emit_instruction(instr, out);
    }

    if stack_size > 0 {
        writeln!(out, "    add sp, sp, #{}", stack_size).unwrap();
    }
    writeln!(out, "    ldp x29, x30, [sp], #16").unwrap();
    writeln!(out, "    ret").unwrap();
}

fn emit_instruction(instr: &AsmInstruction, out: &mut String) {
    match instr {
        AsmInstruction::AllocateStack(_) => {} // handled in emit_function

        AsmInstruction::Mov { src, dest } => match (src, dest) {
            (AsmOperand::Imm(val), AsmOperand::Reg(r)) => emit_load_imm32(out, reg_name(r), *val),
            (AsmOperand::Imm(val), AsmOperand::Stack(off)) => {
                emit_load_imm32(out, "w10", *val);
                writeln!(out, "    str w10, [x29, #{}]", off).unwrap();
            }
            (AsmOperand::Reg(r), AsmOperand::Stack(off)) => {
                writeln!(out, "    str {}, [x29, #{}]", reg_name(r), off).unwrap();
            }
            (AsmOperand::Stack(off), AsmOperand::Reg(r)) => {
                writeln!(out, "    ldr {}, [x29, #{}]", reg_name(r), off).unwrap();
            }
            (AsmOperand::Reg(src), AsmOperand::Reg(dst)) => {
                writeln!(out, "    mov {}, {}", reg_name(dst), reg_name(src)).unwrap();
            }
            (AsmOperand::Stack(src_off), AsmOperand::Stack(dst_off)) => {
                writeln!(out, "    ldr w10, [x29, #{}]", src_off).unwrap();
                writeln!(out, "    str w10, [x29, #{}]", dst_off).unwrap();
            }
            _ => panic!("Unsupported mov {:?} -> {:?}", src, dest),
        },

        AsmInstruction::Unary(op, operand) => {
            let instr_str = match op {
                AsmUnaryOp::Neg => "neg",
                AsmUnaryOp::Not => "mvn",
            };
            match operand {
                AsmOperand::Reg(r) => {
                    writeln!(out, "    {} {}, {}", instr_str, reg_name(r), reg_name(r)).unwrap()
                }
                AsmOperand::Stack(off) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap();
                    writeln!(out, "    {} w10, w10", instr_str).unwrap();
                    writeln!(out, "    str w10, [x29, #{}]", off).unwrap();
                }
                _ => panic!("Invalid unary operand"),
            }
        }

        AsmInstruction::Binary(op, src, dest) => {
            let instr_str = match op {
                AsmBinaryOp::Add => "add",
                AsmBinaryOp::Subtract => "sub",
                AsmBinaryOp::Multiply => "mul",
            };
            match dest {
                AsmOperand::Reg(rd) => match src {
                    AsmOperand::Imm(val) => {
                        emit_load_imm32(out, "w10", *val);
                        writeln!(
                            out,
                            "    {} {}, {}, w10",
                            instr_str,
                            reg_name(rd),
                            reg_name(rd)
                        )
                        .unwrap();
                    }
                    AsmOperand::Stack(off) => {
                        writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap();
                        writeln!(
                            out,
                            "    {} {}, {}, w10",
                            instr_str,
                            reg_name(rd),
                            reg_name(rd)
                        )
                        .unwrap();
                    }
                    AsmOperand::Reg(rs) => {
                        writeln!(
                            out,
                            "    {} {}, {}, {}",
                            instr_str,
                            reg_name(rd),
                            reg_name(rd),
                            reg_name(rs)
                        )
                        .unwrap();
                    }
                    _ => panic!("Invalid binary src"),
                },
                AsmOperand::Stack(off) => {
                    writeln!(out, "    ldr w11, [x29, #{}]", off).unwrap();
                    match src {
                        AsmOperand::Imm(val) => {
                            emit_load_imm32(out, "w10", *val);
                            writeln!(out, "    {} w11, w11, w10", instr_str).unwrap();
                        }
                        AsmOperand::Reg(rs) => {
                            writeln!(out, "    {} w11, w11, {}", instr_str, reg_name(rs)).unwrap();
                        }
                        AsmOperand::Stack(src_off) => {
                            writeln!(out, "    ldr w10, [x29, #{}]", src_off).unwrap();
                            writeln!(out, "    {} w11, w11, w10", instr_str).unwrap();
                        }
                        _ => panic!("Invalid binary src"),
                    }
                    writeln!(out, "    str w11, [x29, #{}]", off).unwrap();
                }
                _ => panic!("Invalid binary dest"),
            }
        }

        AsmInstruction::Cmp(lhs, rhs) => {
            let (lhs_reg, rhs_reg) = load_cmp_operands(lhs, rhs, out);
            writeln!(out, "    cmp {}, {}", lhs_reg, rhs_reg).unwrap();
        }

        AsmInstruction::SetCC(cc, op) => match op {
            AsmOperand::Reg(r) => {
                writeln!(out, "    cset {}, {}", reg_name(r), cond_code_name(cc)).unwrap()
            }
            AsmOperand::Stack(off) => {
                writeln!(out, "    cset w10, {}", cond_code_name(cc)).unwrap();
                writeln!(out, "    str w10, [x29, #{}]", off).unwrap();
            }
            _ => panic!("Invalid setcc operand"),
        },

        AsmInstruction::Idiv(op) => {
            match op {
                AsmOperand::Reg(divisor) => {
                    writeln!(out, "    mov w11, w0").unwrap(); // copy dividend
                    writeln!(out, "    sdiv w0, w11, {}", reg_name(divisor)).unwrap(); // quotient
                    writeln!(out, "    mul w1, w0, {}", reg_name(divisor)).unwrap(); // q*divisor
                    writeln!(out, "    sub w1, w11, w1").unwrap(); // remainder
                }
                AsmOperand::Stack(off) => {
                    writeln!(out, "    ldr w10, [x29, #{}]", off).unwrap(); // load divisor
                    writeln!(out, "    mov w11, w0").unwrap(); // copy dividend
                    writeln!(out, "    sdiv w0, w11, w10").unwrap(); // quotient
                    writeln!(out, "    mul w1, w0, w10").unwrap(); // q*divisor
                    writeln!(out, "    sub w1, w11, w1").unwrap(); // remainder
                }
                _ => panic!("Invalid Idiv operand"),
            }
        }

        AsmInstruction::Cdq => {
            writeln!(out, "    sxtw x1, w0").unwrap(); // extend w0 -> x1:w0
        }

        AsmInstruction::Jmp(label) => writeln!(out, "    b {}", label).unwrap(),
        AsmInstruction::JmpCC(cc, label) => {
            writeln!(out, "    b.{} {}", cond_code_name(cc), label).unwrap()
        }
        AsmInstruction::Label(label) => writeln!(out, "{}:", label).unwrap(),
        AsmInstruction::Ret => {}
    }
}

fn load_cmp_operands<'a>(
    lhs: &AsmOperand,
    rhs: &AsmOperand,
    out: &mut String,
) -> (&'static str, &'static str) {
    match (lhs, rhs) {
        (AsmOperand::Reg(l), AsmOperand::Reg(r)) => (reg_name(l), reg_name(r)),
        (AsmOperand::Reg(l), AsmOperand::Imm(v)) => {
            emit_load_imm32(out, "w11", *v);
            (reg_name(l), "w11")
        }
        (AsmOperand::Imm(v), AsmOperand::Reg(r)) => {
            emit_load_imm32(out, "w10", *v);
            ("w10", reg_name(r))
        }
        (AsmOperand::Stack(l_off), AsmOperand::Reg(r)) => {
            writeln!(out, "    ldr w10, [x29, #{}]", l_off).unwrap();
            ("w10", reg_name(r))
        }
        (AsmOperand::Reg(l), AsmOperand::Stack(r_off)) => {
            writeln!(out, "    ldr w11, [x29, #{}]", r_off).unwrap();
            (reg_name(l), "w11")
        }
        (AsmOperand::Stack(l_off), AsmOperand::Imm(v)) => {
            writeln!(out, "    ldr w10, [x29, #{}]", l_off).unwrap();
            emit_load_imm32(out, "w11", *v);
            ("w10", "w11")
        }
        (AsmOperand::Imm(v), AsmOperand::Stack(r_off)) => {
            writeln!(out, "    ldr w11, [x29, #{}]", r_off).unwrap();
            emit_load_imm32(out, "w10", *v);
            ("w10", "w11")
        }
        (AsmOperand::Stack(l_off), AsmOperand::Stack(r_off)) => {
            writeln!(out, "    ldr w10, [x29, #{}]", l_off).unwrap();
            writeln!(out, "    ldr w11, [x29, #{}]", r_off).unwrap();
            ("w10", "w11")
        }
        _ => panic!("Invalid cmp operands"),
    }
}

fn emit_load_imm32(out: &mut String, reg: &str, val: i32) {
    let val = val as u32;
    let lo = val & 0xffff;
    let hi = (val >> 16) & 0xffff;
    writeln!(out, "    movz {}, #{}", reg, lo).unwrap();
    if hi != 0 {
        writeln!(out, "    movk {}, #{}, lsl #16", reg, hi).unwrap();
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
        AsmCondCode::G => "gt",
        AsmCondCode::GE => "ge",
        AsmCondCode::L => "lt",
        AsmCondCode::LE => "le",
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
