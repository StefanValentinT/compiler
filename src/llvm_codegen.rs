use std::collections::{HashMap, HashSet};

use crate::tac::{TacBinaryOp, TacFuncDef, TacInstruction, TacProgram, TacUnaryOp, TacVal};

pub fn emit_llvm(program: &TacProgram) -> String {
    let TacProgram::Program(funcs) = program;

    let mut defined_functions = HashSet::new();
    let mut external_functions = HashSet::new();

    for f in funcs {
        if let TacFuncDef::Function { name, .. } = f {
            defined_functions.insert(name.clone());
        }
    }

    let mut reg_counter = 0;
    let mut functions_ir = String::new();

    for f in funcs {
        functions_ir.push_str(&emit_function(
            f,
            &defined_functions,
            &mut external_functions,
            &mut reg_counter,
        ));
        functions_ir.push('\n');
    }

    let mut out = String::new();

    for ext in &external_functions {
        out.push_str(&format!("declare i32 @{}(...)\n", ext));
    }

    if !external_functions.is_empty() {
        out.push('\n');
    }

    out.push_str(&functions_ir);
    out
}

pub fn emit_function(
    func: &TacFuncDef,
    defined_functions: &HashSet<String>,
    external_functions: &mut HashSet<String>,
    reg_counter: &mut usize,
) -> String {
    *reg_counter = 0;

    let TacFuncDef::Function { name, params, body } = func;

    let locals = collect_locals(body, params);

    let mut out = String::new();

    out.push_str(&format!(
        "define i32 @{}({}) {{\n",
        name,
        params
            .iter()
            .map(|p| format!("i32 %arg_{}", p))
            .collect::<Vec<_>>()
            .join(", ")
    ));

    out.push_str("entry:\n");

    for v in &locals {
        if !params.contains(v) {
            out.push_str(&format!("  %{} = alloca i32\n", v));
        }
    }

    for p in params {
        out.push_str(&format!(
            "  %{} = alloca i32\n  store i32 %arg_{}, i32* %{}\n",
            p, p, p
        ));
    }

    for instr in body {
        out.push_str(&emit_instr(
            instr,
            defined_functions,
            external_functions,
            reg_counter,
        ));
    }

    out.push_str("}\n");
    out
}

fn emit_instr(
    instr: &TacInstruction,
    defined_functions: &HashSet<String>,
    external_functions: &mut HashSet<String>,
    reg_counter: &mut usize,
) -> String {
    match instr {
        TacInstruction::Label(name) => format!("{}:\n", name),

        TacInstruction::Return(v) => {
            let (load_instr, val_name) = load_val_instr(v, reg_counter);
            format!("{}  ret i32 {}\n", load_instr, val_name)
        }

        TacInstruction::Copy { src, dest } => {
            let (load_instr, val_name) = load_val_instr(src, reg_counter);
            format!(
                "{}  store i32 {}, i32* %{}\n",
                load_instr,
                val_name,
                var_name(dest)
            )
        }

        TacInstruction::Unary { op, src, dest } => {
            let (load_instr, val_name) = load_val_instr(src, reg_counter);
            let r = fresh_reg(reg_counter);

            match op {
                TacUnaryOp::Negate => {
                    format!(
                        "{}  {} = sub i32 0, {}\n  store i32 {}, i32* %{}\n",
                        load_instr,
                        r,
                        val_name,
                        r,
                        var_name(dest)
                    )
                }

                TacUnaryOp::Complement => {
                    format!(
                        "{}  {} = xor i32 {}, -1\n  store i32 {}, i32* %{}\n",
                        load_instr,
                        r,
                        val_name,
                        r,
                        var_name(dest)
                    )
                }

                TacUnaryOp::Not => {
                    let c = fresh_reg(reg_counter);
                    format!(
                        "{}  {} = icmp eq i32 {}, 0\n  {} = zext i1 {} to i32\n  store i32 {}, i32* %{}\n",
                        load_instr,
                        c,
                        val_name,
                        r,
                        c,
                        r,
                        var_name(dest)
                    )
                }
            }
        }

        TacInstruction::Binary {
            op,
            src1,
            src2,
            dest,
        } => {
            let (a_instr, a_val) = load_val_instr(src1, reg_counter);
            let (b_instr, b_val) = load_val_instr(src2, reg_counter);
            let r = fresh_reg(reg_counter);

            match op {
                TacBinaryOp::Add => bin(&a_instr, &b_instr, "add", &a_val, &b_val, &r, dest),
                TacBinaryOp::Subtract => bin(&a_instr, &b_instr, "sub", &a_val, &b_val, &r, dest),
                TacBinaryOp::Multiply => bin(&a_instr, &b_instr, "mul", &a_val, &b_val, &r, dest),
                TacBinaryOp::Divide => bin(&a_instr, &b_instr, "sdiv", &a_val, &b_val, &r, dest),
                TacBinaryOp::Remainder => bin(&a_instr, &b_instr, "srem", &a_val, &b_val, &r, dest),

                _ => {
                    let cmp = fresh_reg(reg_counter);
                    let pred = match op {
                        TacBinaryOp::Equal => "eq",
                        TacBinaryOp::NotEqual => "ne",
                        TacBinaryOp::LessThan => "slt",
                        TacBinaryOp::LessOrEqual => "sle",
                        TacBinaryOp::GreaterThan => "sgt",
                        TacBinaryOp::GreaterOrEqual => "sge",
                        _ => unreachable!(),
                    };
                    format!(
                        "{}{}  {} = icmp {} i32 {}, {}\n  {} = zext i1 {} to i32\n  store i32 {}, i32* %{}\n",
                        a_instr,
                        b_instr,
                        cmp,
                        pred,
                        a_val,
                        b_val,
                        r,
                        cmp,
                        r,
                        var_name(dest)
                    )
                }
            }
        }

        TacInstruction::Jump { target } => format!("  br label %{}\n", target),

        TacInstruction::JumpIfZero { condition, target } => {
            let (load_instr, val_name) = load_val_instr(condition, reg_counter);

            let reg_num = *reg_counter;
            let r = fresh_reg(reg_counter);
            let cont_label = format!("cont{}", reg_num);

            format!(
                "{}  {} = icmp eq i32 {}, 0\n  br i1 {}, label %{}, label %{}\n{}:\n",
                load_instr, r, val_name, r, target, cont_label, cont_label
            )
        }

        TacInstruction::JumpIfNotZero { condition, target } => {
            let (load_instr, val_name) = load_val_instr(condition, reg_counter);

            let reg_num = *reg_counter;
            let r = fresh_reg(reg_counter);
            let cont_label = format!("cont{}", reg_num);

            format!(
                "{}  {} = icmp ne i32 {}, 0\n  br i1 {}, label %{}, label %{}\n{}:\n",
                load_instr, r, val_name, r, target, cont_label, cont_label
            )
        }

        TacInstruction::FunCall {
            fun_name,
            args,
            dest,
        } => {
            if !defined_functions.contains(fun_name) {
                external_functions.insert(fun_name.clone());
            }

            let mut ir = String::new();
            let mut arg_vals = vec![];
            for a in args {
                let (load_instr, val_name) = load_val_instr(a, reg_counter);
                ir.push_str(&load_instr);
                arg_vals.push(format!("i32 {}", val_name));
            }

            let r = fresh_reg(reg_counter);
            ir.push_str(&format!(
                "  {} = call i32 @{}({})\n  store i32 {}, i32* %{}\n",
                r,
                fun_name,
                arg_vals.join(", "),
                r,
                var_name(dest)
            ));
            ir
        }
    }
}

fn load_val_instr(v: &TacVal, reg_counter: &mut usize) -> (String, String) {
    match v {
        TacVal::Constant(c) => ("".to_string(), c.to_string()),
        TacVal::Var(name) => {
            let r = fresh_reg(reg_counter);
            let instr = format!("  {} = load i32, i32* %{}\n", r, name);
            (instr, r)
        }
    }
}

fn bin(a_instr: &str, b_instr: &str, op: &str, a: &str, b: &str, r: &str, dest: &TacVal) -> String {
    format!(
        "{}{}  {} = {} i32 {}, {}\n  store i32 {}, i32* %{}\n",
        a_instr,
        b_instr,
        r,
        op,
        a,
        b,
        r,
        var_name(dest)
    )
}

fn fresh_reg(reg_counter: &mut usize) -> String {
    let r = format!("%r{}", *reg_counter);
    *reg_counter += 1;
    r
}

fn var_name(v: &TacVal) -> &str {
    match v {
        TacVal::Var(s) => s,
        _ => panic!("Expected variable"),
    }
}

fn collect_locals(body: &[TacInstruction], params: &[String]) -> HashSet<String> {
    let mut vars = HashSet::new();

    for p in params {
        vars.insert(p.clone());
    }

    for i in body {
        match i {
            TacInstruction::Copy { dest, .. }
            | TacInstruction::Unary { dest, .. }
            | TacInstruction::Binary { dest, .. }
            | TacInstruction::FunCall { dest, .. } => {
                if let TacVal::Var(v) = dest {
                    vars.insert(v.clone());
                }
            }
            _ => {}
        }
    }

    vars
}
