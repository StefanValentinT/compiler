use clap::Id;

use crate::{
    parser::{Expr, FuncDef, Program, Stmt},
    tac::{TacBinaryOp, TacFuncDef, TacInstruction, TacProgram, TacUnaryOp, TacVal},
};
use std::{collections::HashMap, fmt::Write};

#[derive(Debug)]
pub enum AsmProgram {
    Program(AsmFuncDef),
}
#[derive(Debug)]
pub enum AsmFuncDef {
    Function {
        name: String,
        instructions: Vec<AsmInstruction>,
    },
}
#[derive(Debug)]
pub enum AsmInstruction {
    Mov { src: AsmOperand, dest: AsmOperand },
    Unary(AsmUnaryOp, AsmOperand),
    Binary(AsmBinaryOp, AsmOperand, AsmOperand),
    Cmp(AsmOperand, AsmOperand),
    Idiv(AsmOperand),
    Cdq,
    Jmp(String),
    JmpCC(AsmCondCode, String),
    SetCC(AsmCondCode, AsmOperand),
    Label(String),
    AllocateStack(i32),
    Ret,
}

#[derive(Debug)]
pub enum AsmCondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}
#[derive(Debug, Clone)]
pub enum AsmUnaryOp {
    Neg,
    Not,
}
#[derive(Debug)]
pub enum AsmBinaryOp {
    Add,
    Subtract,
    Multiply,
}
#[derive(Debug, Clone)]
pub enum AsmOperand {
    Imm(i32),
    Reg(AsmReg),
    Pseudo(String),
    Stack(i32),
}
#[derive(Debug, Clone)]
pub enum AsmReg {
    Ax,
    Dx,
    R10,
    R11,
}

pub fn gen_asm(program: TacProgram) -> AsmProgram {
    match program {
        TacProgram::Program { main_func } => {
            let asm_func = convert_func(main_func);

            let (asm_func, stack_size) = replace_pseudoregisters(asm_func);

            let asm_func = fixup_instructions(asm_func, stack_size);

            AsmProgram::Program(asm_func)
        }
    }
}

fn fixup_instructions(func: AsmFuncDef, stack_size: i32) -> AsmFuncDef {
    let AsmFuncDef::Function { name, instructions } = func;

    let mut fixed = Vec::new();

    if stack_size > 0 {
        fixed.push(AsmInstruction::AllocateStack(stack_size));
    }

    for instr in instructions {
        match instr {
            // ---------- cmp mem, mem ----------
            AsmInstruction::Cmp(lhs @ AsmOperand::Stack(_), rhs @ AsmOperand::Stack(_)) => {
                fixed.push(AsmInstruction::Mov {
                    src: lhs,
                    dest: AsmOperand::Reg(AsmReg::R10),
                });
                fixed.push(AsmInstruction::Cmp(AsmOperand::Reg(AsmReg::R10), rhs));
            }

            // ---------- cmp *, imm ----------
            AsmInstruction::Cmp(lhs, AsmOperand::Imm(val)) => {
                fixed.push(AsmInstruction::Mov {
                    src: AsmOperand::Imm(val),
                    dest: AsmOperand::Reg(AsmReg::R11),
                });
                fixed.push(AsmInstruction::Cmp(lhs, AsmOperand::Reg(AsmReg::R11)));
            }

            // ---------- idiv imm ----------
            AsmInstruction::Idiv(AsmOperand::Imm(val)) => {
                fixed.push(AsmInstruction::Mov {
                    src: AsmOperand::Imm(val),
                    dest: AsmOperand::Reg(AsmReg::R10),
                });
                fixed.push(AsmInstruction::Idiv(AsmOperand::Reg(AsmReg::R10)));
            }

            // ---------- mov mem, mem ----------
            AsmInstruction::Mov {
                src: src @ AsmOperand::Stack(_),
                dest: dest @ AsmOperand::Stack(_),
            } => {
                fixed.push(AsmInstruction::Mov {
                    src,
                    dest: AsmOperand::Reg(AsmReg::R10),
                });
                fixed.push(AsmInstruction::Mov {
                    src: AsmOperand::Reg(AsmReg::R10),
                    dest,
                });
            }

            // ---------- everything else ----------
            other => fixed.push(other),
        }
    }

    AsmFuncDef::Function {
        name,
        instructions: fixed,
    }
}

fn replace_pseudoregisters(func: AsmFuncDef) -> (AsmFuncDef, i32) {
    let AsmFuncDef::Function { name, instructions } = func;

    let mut offsets: HashMap<String, i32> = HashMap::new();
    let mut next_offset: i32 = -16;

    fn replace_op(
        op: AsmOperand,
        offsets: &mut HashMap<String, i32>,
        next_offset: &mut i32,
    ) -> AsmOperand {
        match op {
            AsmOperand::Pseudo(name) => {
                let offset = offsets.entry(name).or_insert_with(|| {
                    let off = *next_offset;
                    *next_offset -= 4;
                    off
                });
                AsmOperand::Stack(*offset)
            }
            other => other,
        }
    }

    let new_instructions = instructions
        .into_iter()
        .map(|instr| match instr {
            AsmInstruction::Mov { src, dest } => AsmInstruction::Mov {
                src: replace_op(src, &mut offsets, &mut next_offset),
                dest: replace_op(dest, &mut offsets, &mut next_offset),
            },

            AsmInstruction::Unary(op, operand) => {
                AsmInstruction::Unary(op, replace_op(operand, &mut offsets, &mut next_offset))
            }

            AsmInstruction::Binary(op, src, dest) => AsmInstruction::Binary(
                op,
                replace_op(src, &mut offsets, &mut next_offset),
                replace_op(dest, &mut offsets, &mut next_offset),
            ),

            AsmInstruction::Cmp(lhs, rhs) => AsmInstruction::Cmp(
                replace_op(lhs, &mut offsets, &mut next_offset),
                replace_op(rhs, &mut offsets, &mut next_offset),
            ),

            AsmInstruction::SetCC(cc, operand) => {
                AsmInstruction::SetCC(cc, replace_op(operand, &mut offsets, &mut next_offset))
            }

            AsmInstruction::Idiv(op) => {
                AsmInstruction::Idiv(replace_op(op, &mut offsets, &mut next_offset))
            }

            other => other,
        })
        .collect();

    let stack_size = ((-next_offset + 15) / 16) * 16;

    (
        AsmFuncDef::Function {
            name,
            instructions: new_instructions,
        },
        stack_size,
    )
}

fn convert_func(func: TacFuncDef) -> AsmFuncDef {
    match func {
        TacFuncDef::Function { name, body } => {
            let mut instructions = Vec::new();

            convert_instr(body, &mut instructions);

            AsmFuncDef::Function { name, instructions }
        }
    }
}

fn convert_instr(tac_instructions: Vec<TacInstruction>, instructions: &mut Vec<AsmInstruction>) {
    use AsmInstruction::*;
    for instr in tac_instructions {
        let mut new_instr = match instr {
            TacInstruction::Return(val) => vec![
                Mov {
                    src: tac_op_to_asm(val),
                    dest: AsmOperand::Reg(AsmReg::Ax),
                },
                Ret,
            ],
            TacInstruction::Unary { op, src, dest } => {
                let asm_src = tac_op_to_asm(src);
                let asm_dest = tac_op_to_asm(dest);

                match op {
                    TacUnaryOp::Not => vec![
                        AsmInstruction::Cmp(AsmOperand::Imm(0), asm_src),
                        AsmInstruction::Mov {
                            src: AsmOperand::Imm(0),
                            dest: asm_dest.clone(),
                        },
                        AsmInstruction::SetCC(AsmCondCode::E, asm_dest),
                    ],
                    _ => vec![
                        AsmInstruction::Mov {
                            src: asm_src.clone(),
                            dest: asm_dest.clone(),
                        },
                        AsmInstruction::Unary(tac_unop_to_asm(op), asm_dest),
                    ],
                }
            }

            TacInstruction::Binary {
                op,
                src1,
                src2,
                dest,
            } => {
                let asm_src1 = tac_op_to_asm(src1);
                let asm_src2 = tac_op_to_asm(src2);
                let asm_dest = tac_op_to_asm(dest);

                match op {
                    TacBinaryOp::Add | TacBinaryOp::Subtract | TacBinaryOp::Multiply => {
                        let asm_op = tac_binop_to_asm(op);
                        vec![
                            AsmInstruction::Mov {
                                src: asm_src1,
                                dest: asm_dest.clone(),
                            },
                            AsmInstruction::Binary(asm_op, asm_src2, asm_dest),
                        ]
                    }
                    TacBinaryOp::Divide | TacBinaryOp::Remainder => {
                        let mut instrs = vec![
                            AsmInstruction::Mov {
                                src: asm_src1,
                                dest: AsmOperand::Reg(AsmReg::Ax),
                            },
                            AsmInstruction::Cdq,
                            AsmInstruction::Idiv(asm_src2),
                        ];

                        let result_move = match op {
                            TacBinaryOp::Divide => AsmInstruction::Mov {
                                src: AsmOperand::Reg(AsmReg::Ax),
                                dest: asm_dest,
                            },
                            TacBinaryOp::Remainder => AsmInstruction::Mov {
                                src: AsmOperand::Reg(AsmReg::Dx),
                                dest: asm_dest,
                            },
                            _ => unreachable!(),
                        };

                        instrs.push(result_move);
                        instrs
                    }

                    TacBinaryOp::Equal
                    | TacBinaryOp::NotEqual
                    | TacBinaryOp::LessThan
                    | TacBinaryOp::LessOrEqual
                    | TacBinaryOp::GreaterThan
                    | TacBinaryOp::GreaterOrEqual => {
                        vec![
                            AsmInstruction::Cmp(asm_src2, asm_src1),
                            AsmInstruction::Mov {
                                src: AsmOperand::Imm(0),
                                dest: asm_dest.clone(),
                            },
                            AsmInstruction::SetCC(tac_relop_to_cc(op), asm_dest),
                        ]
                    }
                    _ => unimplemented!(),
                }
            }

            TacInstruction::Copy { src, dest } => vec![Mov {
                src: tac_op_to_asm(src),
                dest: tac_op_to_asm(dest),
            }],
            TacInstruction::Jump { target } => vec![Jmp(target)],
            TacInstruction::JumpIfZero { condition, target } => {
                let val = tac_op_to_asm(condition);
                vec![Cmp(AsmOperand::Imm(0), val), JmpCC(AsmCondCode::E, target)]
            }
            TacInstruction::JumpIfNotZero { condition, target } => {
                let val = tac_op_to_asm(condition);
                vec![Cmp(AsmOperand::Imm(0), val), JmpCC(AsmCondCode::NE, target)]
            }
            TacInstruction::Label(ident) => vec![Label(ident)],
        };
        instructions.append(&mut new_instr);
    }
}

fn tac_op_to_asm(input: TacVal) -> AsmOperand {
    use AsmOperand::{Imm, Pseudo};
    match input {
        TacVal::Constant(int) => Imm(int),
        TacVal::Var(ident) => Pseudo(ident),
    }
}

fn tac_relop_to_cc(op: TacBinaryOp) -> AsmCondCode {
    use AsmCondCode::*;
    use TacBinaryOp::*;
    match op {
        Equal => E,
        NotEqual => NE,
        LessThan => L,
        LessOrEqual => LE,
        GreaterThan => G,
        GreaterOrEqual => GE,
        _ => panic!("Not a relational operator"),
    }
}

fn tac_unop_to_asm(input: TacUnaryOp) -> AsmUnaryOp {
    use AsmUnaryOp::*;

    match input {
        TacUnaryOp::Complement => Not,
        TacUnaryOp::Negate => Neg,
        TacUnaryOp::Not => panic!("Handled via Cmp + SetCC, not AsmUnaryOp"),
    }
}

fn tac_binop_to_asm(op: TacBinaryOp) -> AsmBinaryOp {
    use AsmBinaryOp::*;
    match op {
        TacBinaryOp::Add => Add,
        TacBinaryOp::Subtract => Subtract,
        TacBinaryOp::Multiply => Multiply,
        _ => unimplemented!(),
    }
}
