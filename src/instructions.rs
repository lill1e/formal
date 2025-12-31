use std::collections::{HashMap, HashSet};

use crate::{
    explicate_control::{OrderedNode, ReturnableNode, TerminalNode},
    parser::{BinaryOperation, UnaryOperation},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Register {
    Rax,
    Rbp,
    Rsp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteRegister {
    Al,
}

#[derive(Debug, Clone, PartialEq)]
pub enum X86Value {
    Register(Register),
    ByteRegister(ByteRegister),
    Var(String),
    Immediate(i32),
    Memory(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpType {
    Equals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Addq(X86Value, X86Value),
    Subq(X86Value, X86Value),
    Movq(X86Value, X86Value),
    Negq(X86Value),
    Retq,
    Jmp(String),
    Pushq(X86Value),
    Popq(X86Value),
    Cmpq(X86Value, X86Value),
    Movzbq(X86Value, X86Value),
    Xorq(X86Value, X86Value),
    Set(CmpType, ByteRegister),
}

impl TerminalNode {
    pub fn select_instructions(&self) -> X86Value {
        match self {
            TerminalNode::Number(n) => X86Value::Immediate(*n as i32),
            TerminalNode::Boolean(b) => X86Value::Immediate(if *b { 1 } else { 0 }),
            TerminalNode::Reference(sym) => X86Value::Var(sym.to_string()),
            TerminalNode::Void => X86Value::Immediate(0),
        }
    }
}

impl ReturnableNode {
    pub fn select_instructions(&self, target: X86Value) -> Vec<Instruction> {
        match self {
            ReturnableNode::Binary(BinaryOperation::Addition, m, n) => vec![
                Instruction::Movq(m.select_instructions(), target.clone()),
                Instruction::Addq(n.select_instructions(), target),
            ],
            ReturnableNode::Binary(BinaryOperation::Subtraction, m, n) => vec![
                Instruction::Movq(m.select_instructions(), target.clone()),
                Instruction::Subq(n.select_instructions(), target),
            ],
            ReturnableNode::Binary(BinaryOperation::Equals, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::Equals, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target),
            ],
            ReturnableNode::Binary(BinaryOperation::NotEquals, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::Equals, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target.clone()),
                Instruction::Xorq(X86Value::Immediate(1), target),
            ],
            ReturnableNode::Binary(BinaryOperation::Greater, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::Greater, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target),
            ],
            ReturnableNode::Binary(BinaryOperation::GreaterEqual, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::GreaterEqual, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target),
            ],
            ReturnableNode::Binary(BinaryOperation::Less, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::Less, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target),
            ],
            ReturnableNode::Binary(BinaryOperation::LessEqual, x, y) => vec![
                Instruction::Movq(x.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Cmpq(y.select_instructions(), X86Value::Register(Register::Rax)),
                Instruction::Set(CmpType::LessEqual, ByteRegister::Al),
                Instruction::Movzbq(X86Value::ByteRegister(ByteRegister::Al), target),
            ],
            ReturnableNode::Unary(UnaryOperation::Negation, n) => vec![
                Instruction::Movq(n.select_instructions(), target.clone()),
                Instruction::Negq(target),
            ],
            ReturnableNode::Unary(UnaryOperation::Not, n) => vec![
                Instruction::Movq(n.select_instructions(), target.clone()),
                Instruction::Xorq(X86Value::Immediate(1), target),
            ],
            ReturnableNode::Terminal(t) => vec![Instruction::Movq(t.select_instructions(), target)],
        }
    }
}

impl OrderedNode {
    pub fn select_instructions(self) -> Vec<Instruction> {
        match self {
            OrderedNode::Return(val) => val.select_instructions(X86Value::Register(Register::Rax)),
            OrderedNode::Binding(sym, binding, tail) => {
                let mut instrs = binding.select_instructions(X86Value::Var(sym));
                instrs.append(&mut tail.select_instructions());
                instrs
            }
        }
    }
}

impl X86Value {
    fn to_memory(self, offset: i32) -> X86Value {
        match self {
            X86Value::Register(_) => self,
            X86Value::ByteRegister(_) => self,
            X86Value::Immediate(_) => self,
            X86Value::Memory(_) => self,
            X86Value::Var(_) => X86Value::Memory(offset),
        }
    }

    fn to_var(&self) -> Option<String> {
        match self {
            X86Value::Register(_) => None,
            X86Value::ByteRegister(_) => None,
            X86Value::Immediate(_) => None,
            X86Value::Memory(_) => None,
            X86Value::Var(v) => Some(v.to_string()),
        }
    }

    fn is_var(&self) -> bool {
        match self {
            X86Value::Register(_) => false,
            X86Value::ByteRegister(_) => false,
            X86Value::Immediate(_) => false,
            X86Value::Memory(_) => false,
            X86Value::Var(_) => true,
        }
    }

    fn is_memory(&self) -> bool {
        match self {
            X86Value::Memory(_) => true,
            _ => false,
        }
    }
}

impl Instruction {
    fn has_var(&self) -> bool {
        match self {
            Instruction::Retq => false,
            Instruction::Jmp(_) => false,
            Instruction::Set(_, _) => false,
            Instruction::Movq(lhs, rhs)
            | Instruction::Addq(lhs, rhs)
            | Instruction::Subq(lhs, rhs)
            | Instruction::Cmpq(lhs, rhs)
            | Instruction::Xorq(lhs, rhs) => lhs.is_var() || rhs.is_var(),
            Instruction::Pushq(v)
            | Instruction::Popq(v)
            | Instruction::Movzbq(v, _)
            | Instruction::Negq(v) => v.is_var(),
        }
    }

    fn assign_homes(self, vars: &HashMap<String, i32>) -> Instruction {
        match self {
            i if !i.has_var() => i,
            Instruction::Movq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Movq(new_lhs, new_rhs);
            }
            Instruction::Addq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Addq(new_lhs, new_rhs);
            }
            Instruction::Subq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Subq(new_lhs, new_rhs);
            }
            Instruction::Cmpq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Cmpq(new_lhs, new_rhs);
            }
            Instruction::Movzbq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Movzbq(new_lhs, new_rhs);
            }
            Instruction::Xorq(lhs, rhs) => {
                let lhs_var = lhs.to_var();
                let rhs_var = rhs.to_var();
                let new_lhs = if lhs_var.is_some() && vars.contains_key(&lhs_var.clone().unwrap()) {
                    lhs.to_memory(vars[&lhs_var.unwrap()])
                } else {
                    lhs
                };
                let new_rhs = if rhs_var.is_some() && vars.contains_key(&rhs_var.clone().unwrap()) {
                    rhs.to_memory(vars[&rhs_var.unwrap()])
                } else {
                    rhs
                };
                return Instruction::Xorq(new_lhs, new_rhs);
            }
            Instruction::Negq(v) => {
                let var = v.to_var();
                let new_var = if var.is_some() && vars.contains_key(&var.clone().unwrap()) {
                    v.to_memory(vars[&var.unwrap()])
                } else {
                    v
                };
                return Instruction::Negq(new_var);
            }
            Instruction::Retq
            | Instruction::Jmp(_)
            | Instruction::Pushq(_)
            | Instruction::Popq(_)
            | Instruction::Set(_, _) => self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

pub trait Instructions {
    fn allocate_homes(&self) -> HashMap<String, i32>;
    fn assign_homes(self) -> (Self, usize)
    where
        Self: Sized;
    fn patch_instructions(self) -> Self;
    fn generate_asm(self, vars: usize) -> Vec<Block>;
}

impl Instructions for Vec<Instruction> {
    fn allocate_homes(&self) -> HashMap<String, i32> {
        let all_vars = self
            .iter()
            .flat_map(|instr| match instr {
                Instruction::Retq => Vec::new(),
                Instruction::Jmp(_) => Vec::new(),
                Instruction::Set(_, _) => Vec::new(),
                Instruction::Addq(lhs, rhs) | Instruction::Subq(lhs, rhs) => {
                    let mut vars = Vec::new();
                    match lhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    match rhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    return vars;
                }
                Instruction::Movq(lhs, rhs) => {
                    let mut vars = Vec::new();
                    match lhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    match rhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    return vars;
                }
                Instruction::Pushq(v) => match v {
                    X86Value::Var(v_name) => vec![v_name],
                    _ => Vec::new(),
                },
                Instruction::Negq(v) => match v {
                    X86Value::Var(v_name) => vec![v_name],
                    _ => Vec::new(),
                },
                Instruction::Popq(v) => match v {
                    X86Value::Var(v_name) => vec![v_name],
                    _ => Vec::new(),
                },
                Instruction::Movzbq(v, _) => match v {
                    X86Value::Var(v_name) => vec![v_name],
                    _ => Vec::new(),
                },
                Instruction::Cmpq(lhs, rhs) => {
                    let mut vars = Vec::new();
                    match lhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    match rhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    return vars;
                }
                Instruction::Xorq(lhs, rhs) => {
                    let mut vars = Vec::new();
                    match lhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    match rhs {
                        X86Value::Var(v) => {
                            vars.push(v);
                        }
                        _ => {}
                    }
                    return vars;
                }
            })
            .collect::<HashSet<&String>>();
        let mut var_mapping: HashMap<String, i32> = HashMap::new();
        let mut curr: i32 = 0;
        for var in all_vars {
            curr += 8;
            var_mapping.insert(var.clone(), -1 * curr);
        }
        return var_mapping;
    }

    fn assign_homes(self) -> (Self, usize) {
        let all_vars = self.allocate_homes();
        return (
            self.iter()
                .map(|instr| instr.clone().assign_homes(&all_vars))
                .collect(),
            all_vars.len(),
        );
    }

    fn patch_instructions(self) -> Self {
        self.into_iter()
            .flat_map(|instr| match instr {
                Instruction::Movq(lhs, rhs) if lhs == rhs => vec![],
                Instruction::Movq(lhs, rhs) if lhs.is_memory() && rhs.is_memory() => {
                    vec![
                        Instruction::Movq(lhs.clone(), X86Value::Register(Register::Rax)),
                        Instruction::Movq(X86Value::Register(Register::Rax), rhs.clone()),
                    ]
                }
                Instruction::Addq(lhs, rhs) if lhs.is_memory() && rhs.is_memory() => {
                    vec![
                        Instruction::Movq(rhs.clone(), X86Value::Register(Register::Rax)),
                        Instruction::Addq(lhs.clone(), X86Value::Register(Register::Rax)),
                        Instruction::Movq(X86Value::Register(Register::Rax), rhs.clone()),
                    ]
                }
                _ => vec![instr],
            })
            .collect()
    }

    fn generate_asm(self, vars: usize) -> Vec<Block> {
        let mut blocks = Vec::new();
        let stack_space = if (vars * 8) % 16 == 0 {
            (vars * 8) as i32
        } else {
            (vars * 8 + 8) as i32
        };
        blocks.push(Block {
            label: String::from("main"),
            instructions: vec![
                Instruction::Pushq(X86Value::Register(Register::Rbp)),
                Instruction::Movq(
                    X86Value::Register(Register::Rsp),
                    X86Value::Register(Register::Rbp),
                ),
                Instruction::Addq(
                    X86Value::Immediate(stack_space * -1),
                    X86Value::Register(Register::Rsp),
                ),
                Instruction::Jmp(String::from("start")),
            ],
        });
        let mut instrs = self.clone();
        instrs.push(Instruction::Jmp(String::from("conclusion")));
        blocks.push(Block {
            label: String::from("start"),
            instructions: instrs,
        });
        blocks.push(Block {
            label: String::from("conclusion"),
            instructions: vec![
                Instruction::Addq(
                    X86Value::Immediate(stack_space),
                    X86Value::Register(Register::Rsp),
                ),
                Instruction::Popq(X86Value::Register(Register::Rbp)),
                Instruction::Retq,
            ],
        });
        return blocks;
    }
}

impl ByteRegister {
    fn stringify(&self) -> String {
        match self {
            Self::Al => String::from("%al"),
        }
    }
}

impl Register {
    fn stringify(&self) -> String {
        match self {
            Self::Rax => String::from("%rax"),
            Self::Rbp => String::from("%rbp"),
            Self::Rsp => String::from("%rsp"),
        }
    }
}

impl X86Value {
    fn stringify(&self) -> String {
        match self {
            X86Value::Register(reg) => reg.to_string(),
            X86Value::ByteRegister(reg) => reg.to_string(),
            X86Value::Var(sym) => sym.to_string(),
            X86Value::Immediate(n) => format!("${}", n),
            X86Value::Memory(offset) => format!("{}(%rbp)", offset),
        }
    }
}

impl CmpType {
    fn stringify(&self) -> String {
        match self {
            CmpType::Equals => String::from("e"),
            CmpType::Greater => String::from("g"),
            CmpType::GreaterEqual => String::from("ge"),
            CmpType::Less => String::from("l"),
            CmpType::LessEqual => String::from("le"),
        }
    }
}

impl Instruction {
    fn stringify(&self) -> String {
        match self {
            Instruction::Addq(lhs, rhs) => format!("addq {}, {}", lhs.to_string(), rhs.to_string()),
            Instruction::Subq(lhs, rhs) => format!("subq {}, {}", lhs.to_string(), rhs.to_string()),
            Instruction::Movq(lhs, rhs) => format!("movq {}, {}", lhs.to_string(), rhs.to_string()),
            Instruction::Jmp(label) => format!("jmp {}", label),
            Instruction::Retq => String::from("retq"),
            Instruction::Pushq(v) => format!("pushq {}", v.to_string()),
            Instruction::Popq(v) => format!("popq {}", v.to_string()),
            Instruction::Cmpq(lhs, rhs) => format!("cmpq {}, {}", lhs.to_string(), rhs.to_string()),
            Instruction::Movzbq(lhs, dest) => {
                format!("movzbq {}, {}", lhs.to_string(), dest.to_string())
            }
            Instruction::Xorq(lhs, rhs) => format!("xorq {}, {}", lhs.to_string(), rhs.to_string()),
            Instruction::Negq(v) => format!("negq {}", v.to_string()),
            Instruction::Set(cmp, dest) => format!("set{} {}", cmp.to_string(), dest.to_string()),
        }
    }
}

impl ToString for ByteRegister {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for Register {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for X86Value {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for CmpType {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl Block {
    fn stringify(&self, tab: bool) -> String {
        let tab_str = if tab { "    " } else { "" };
        format!(
            "{}{}.align 8\n{}:\n{}",
            if self.label == "main" {
                tab_str.to_owned() + ".globl main\n"
            } else {
                String::new()
            },
            tab_str,
            self.label,
            self.instructions
                .iter()
                .map(|instruction| tab_str.to_owned() + &instruction.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl ToString for Block {
    fn to_string(&self) -> String {
        return self.stringify(true);
    }
}
