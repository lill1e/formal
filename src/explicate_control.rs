use anyhow::{Result, bail};

use crate::{
    instructions::CmpType,
    parser::{BinaryOperation, UnaryOperation},
    rco::{AtomicNode, ComplexNode},
};

#[derive(Debug, Clone)]
pub enum TerminalNode {
    Number(i32),
    Boolean(bool),
    Reference(String),
    Void,
}

#[derive(Debug, Clone)]
pub enum ReturnableNode {
    Binary(BinaryOperation, TerminalNode, TerminalNode),
    Unary(UnaryOperation, TerminalNode),
    Terminal(TerminalNode),
}

#[derive(Debug, Clone)]
pub enum OrderedNode {
    Return(ReturnableNode),
    Binding(String, ReturnableNode, Box<OrderedNode>),
    Goto(String),
    If(CmpType, TerminalNode, TerminalNode, String, String),
}

#[derive(Debug, Clone)]
pub struct ProgramNode {
    pub blocks: Vec<(String, OrderedNode)>,
    counter: u32,
}

impl BinaryOperation {
    fn as_cmptype(&self) -> Option<CmpType> {
        match self {
            BinaryOperation::Equals => Some(CmpType::Equals),
            BinaryOperation::Less => Some(CmpType::Less),
            BinaryOperation::LessEqual => Some(CmpType::LessEqual),
            BinaryOperation::Greater => Some(CmpType::Greater),
            BinaryOperation::GreaterEqual => Some(CmpType::GreaterEqual),
            BinaryOperation::Addition
            | BinaryOperation::Subtraction
            | BinaryOperation::NotEquals => None,
        }
    }
}

impl ProgramNode {
    fn create_block(&mut self, tail: OrderedNode) -> String {
        let block_label = format!("block{}", self.counter);
        self.counter += 1;
        self.blocks.push((block_label.clone(), tail.clone()));
        block_label
    }
}

impl AtomicNode {
    pub fn explicate_tail(self) -> OrderedNode {
        match self {
            AtomicNode::Void => OrderedNode::Return(ReturnableNode::Terminal(TerminalNode::Void)),
            AtomicNode::Number(n) => {
                OrderedNode::Return(ReturnableNode::Terminal(TerminalNode::Number(n)))
            }
            AtomicNode::Boolean(b) => {
                OrderedNode::Return(ReturnableNode::Terminal(TerminalNode::Boolean(b)))
            }
            AtomicNode::Reference(sym) => OrderedNode::Return(ReturnableNode::Terminal(
                TerminalNode::Reference(sym.to_string()),
            )),
        }
    }

    pub fn explicate_assign(self, binding: String, tail: OrderedNode) -> OrderedNode {
        match self {
            AtomicNode::Void => OrderedNode::Binding(
                binding,
                ReturnableNode::Terminal(TerminalNode::Void),
                Box::new(tail),
            ),
            AtomicNode::Number(n) => OrderedNode::Binding(
                binding,
                ReturnableNode::Terminal(TerminalNode::Number(n)),
                Box::new(tail),
            ),
            AtomicNode::Boolean(b) => OrderedNode::Binding(
                binding,
                ReturnableNode::Terminal(TerminalNode::Boolean(b)),
                Box::new(tail),
            ),
            AtomicNode::Reference(sym) => OrderedNode::Binding(
                binding,
                ReturnableNode::Terminal(TerminalNode::Reference(sym)),
                Box::new(tail),
            ),
        }
    }

    fn as_terminal(self) -> TerminalNode {
        match self {
            AtomicNode::Void => TerminalNode::Void,
            AtomicNode::Number(n) => TerminalNode::Number(n),
            AtomicNode::Boolean(b) => TerminalNode::Boolean(b),
            AtomicNode::Reference(sym) => TerminalNode::Reference(sym.to_string()),
        }
    }
}

impl ComplexNode {
    pub fn is_pure(&self) -> bool {
        match self {
            ComplexNode::Binary(_, _, _) => true,
            ComplexNode::Atomic(_) => true,
            ComplexNode::Let(_, rhs, body) => rhs.is_pure() && body.is_pure(),
            ComplexNode::Begin(exprs, last) => {
                exprs.iter().map(|e| e.is_pure()).all(|v| v) && last.is_pure()
            }
            ComplexNode::Assignment(_, _) => false,
            ComplexNode::Unary(_, _) => true,
            ComplexNode::If(cond, conseq, alt) => {
                cond.is_pure() && conseq.is_pure() && alt.is_pure()
            }
        }
    }

    pub fn explicate_effect(
        self,
        tail: OrderedNode,
        program: &mut ProgramNode,
    ) -> Result<OrderedNode> {
        Ok(match self {
            _ if self.is_pure() => tail,
            ComplexNode::Let(sym, rhs, body) => {
                rhs.explicate_assign(sym, body.explicate_effect(tail, program)?, program)?
            }
            ComplexNode::Assignment(sym, rhs) => rhs.explicate_assign(sym, tail, program)?,
            ComplexNode::Begin(exprs, last) => {
                let mut curr = last.explicate_effect(tail, program);
                for expr in exprs {
                    curr = expr.explicate_effect(curr?, program);
                }
                return curr;
            }
            ComplexNode::If(cond, conseq, alt) => {
                let cont = program.create_block(tail);
                cond.explicate_pred(
                    conseq.explicate_effect(OrderedNode::Goto(cont.clone()), program)?,
                    alt.explicate_effect(OrderedNode::Goto(cont), program)?,
                    program,
                )?
            }
            _ => panic!("explicate_effect received an impure value (needs impl)"),
        })
    }

    pub fn explicate_tail(self, program: &mut ProgramNode) -> Result<OrderedNode> {
        Ok(match self {
            ComplexNode::Binary(operation, a1, a2) => OrderedNode::Return(ReturnableNode::Binary(
                operation,
                a1.as_terminal(),
                a2.as_terminal(),
            )),
            ComplexNode::Atomic(a) => a.explicate_tail(),
            ComplexNode::Let(sym, binding, body) => {
                binding.explicate_assign(sym, body.explicate_tail(program)?, program)?
            }
            ComplexNode::Begin(exprs, last) => {
                let mut tail = last.explicate_tail(program);
                for expr in exprs {
                    tail = expr.explicate_effect(tail?, program);
                }
                return tail;
            }
            ComplexNode::Assignment(_, _) => self.explicate_effect(
                OrderedNode::Return(ReturnableNode::Terminal(TerminalNode::Void)),
                program,
            )?,
            ComplexNode::Unary(operation, a) => {
                OrderedNode::Return(ReturnableNode::Unary(operation, a.as_terminal()))
            }
            ComplexNode::If(cond, conseq, alt) => cond.explicate_pred(
                conseq.explicate_tail(program)?,
                alt.explicate_tail(program)?,
                program,
            )?,
        })
    }

    pub fn explicate_assign(
        self,
        binding: String,
        tail: OrderedNode,
        program: &mut ProgramNode,
    ) -> Result<OrderedNode> {
        Ok(match self {
            ComplexNode::Atomic(a) => a.explicate_assign(binding, tail),
            ComplexNode::Binary(operation, a1, a2) => OrderedNode::Binding(
                binding,
                ReturnableNode::Binary(operation, a1.as_terminal(), a2.as_terminal()),
                Box::new(tail),
            ),
            ComplexNode::Let(sym, rhs, body) => {
                rhs.explicate_assign(sym, body.explicate_assign(binding, tail, program)?, program)?
            }
            ComplexNode::Begin(exprs, last) => {
                let mut seq_tail = last.explicate_assign(binding, tail, program);
                for expr in exprs {
                    seq_tail = expr.explicate_effect(seq_tail?, program)
                }
                return seq_tail;
            }
            ComplexNode::Assignment(_, _) => self.explicate_effect(
                OrderedNode::Binding(
                    binding,
                    ReturnableNode::Terminal(TerminalNode::Void),
                    Box::new(tail),
                ),
                program,
            )?,
            ComplexNode::Unary(operation, a) => OrderedNode::Binding(
                binding,
                ReturnableNode::Unary(operation, a.as_terminal()),
                Box::new(tail),
            ),
            ComplexNode::If(cond, conseq, alt) => {
                let block_label = program.create_block(tail);
                cond.explicate_pred(
                    conseq.explicate_assign(
                        binding.clone(),
                        OrderedNode::Goto(block_label.clone()),
                        program,
                    )?,
                    alt.explicate_assign(binding, OrderedNode::Goto(block_label), program)?,
                    program,
                )?
            }
        })
    }

    pub fn explicate_pred(
        self,
        conseq: OrderedNode,
        alt: OrderedNode,
        program: &mut ProgramNode,
    ) -> Result<OrderedNode> {
        Ok(match self.clone() {
            ComplexNode::Atomic(AtomicNode::Reference(sym)) => OrderedNode::If(
                CmpType::Equals,
                TerminalNode::Reference(sym),
                TerminalNode::Boolean(true),
                program.create_block(conseq),
                program.create_block(alt),
            ),
            ComplexNode::Let(sym, rhs, body) => {
                rhs.explicate_assign(sym, body.explicate_pred(conseq, alt, program)?, program)?
            }
            ComplexNode::Unary(UnaryOperation::Not, child) => OrderedNode::If(
                CmpType::Equals,
                child.as_terminal(),
                TerminalNode::Boolean(false),
                program.create_block(conseq),
                program.create_block(alt),
            ),
            ComplexNode::Binary(
                BinaryOperation::Equals
                | BinaryOperation::Less
                | BinaryOperation::LessEqual
                | BinaryOperation::Greater
                | BinaryOperation::GreaterEqual,
                lhs,
                rhs,
            ) => OrderedNode::If(
                match self {
                    ComplexNode::Binary(op, _, _) => op.as_cmptype().unwrap_or(CmpType::Equals),
                    _ => bail!("Invalid Sequence Events (explicate_pred)"),
                },
                lhs.as_terminal(),
                rhs.as_terminal(),
                program.create_block(conseq),
                program.create_block(alt),
            ),
            ComplexNode::Atomic(AtomicNode::Boolean(b)) => {
                if b {
                    conseq
                } else {
                    alt
                }
            }
            ComplexNode::If(cond, conseq_alt, alt_alt) => {
                let conseq_block = program.create_block(conseq);
                let alt_block = program.create_block(alt);
                cond.explicate_pred(
                    conseq_alt.explicate_pred(
                        OrderedNode::Goto(conseq_block.clone()),
                        OrderedNode::Goto(alt_block.clone()),
                        program,
                    )?,
                    alt_alt.explicate_pred(
                        OrderedNode::Goto(conseq_block),
                        OrderedNode::Goto(alt_block),
                        program,
                    )?,
                    program,
                )?
            }
            _ => bail!("Unexpected case for explicate_pred"),
        })
    }

    pub fn explicate_control(self) -> Result<ProgramNode> {
        let mut program = ProgramNode {
            blocks: Vec::new(),
            counter: 0,
        };
        let e = self.explicate_tail(&mut program)?;
        program.blocks.push((String::from("start"), e));
        Ok(program)
    }
}

impl TerminalNode {
    fn stringify(&self) -> String {
        match self {
            TerminalNode::Number(n) => n.to_string(),
            TerminalNode::Boolean(b) => b.to_string(),
            TerminalNode::Reference(sym) => sym.to_string(),
            TerminalNode::Void => String::from("void"),
        }
    }
}

impl ReturnableNode {
    fn stringify(&self) -> String {
        match self {
            ReturnableNode::Binary(BinaryOperation::Addition, n, m) => {
                format!("{} + {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::Subtraction, n, m) => {
                format!("{} - {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::Equals, n, m) => {
                format!("{} == {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::NotEquals, n, m) => {
                format!("{} != {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Unary(UnaryOperation::Negation, n) => format!("-{}", n.to_string()),
            ReturnableNode::Unary(UnaryOperation::Not, b) => format!("!{}", b.to_string()),
            ReturnableNode::Terminal(n) => n.to_string(),
            ReturnableNode::Binary(BinaryOperation::Greater, n, m) => {
                format!("{} > {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::GreaterEqual, n, m) => {
                format!("{} >= {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::Less, n, m) => {
                format!("{} < {}", n.to_string(), m.to_string())
            }
            ReturnableNode::Binary(BinaryOperation::LessEqual, n, m) => {
                format!("{} <= {}", n.to_string(), m.to_string())
            }
        }
    }
}

fn stringify_op(cmp: &CmpType) -> String {
    String::from(match cmp {
        CmpType::Equals => "==",
        CmpType::Greater => ">",
        CmpType::GreaterEqual => ">=",
        CmpType::Less => "<",
        CmpType::LessEqual => "<=",
    })
}

impl OrderedNode {
    fn stringify(&self) -> String {
        match self {
            OrderedNode::Return(v) => format!("return {}", v.to_string()),
            OrderedNode::Binding(sym, rhs, tail) => {
                format!("{} = {}\n{}", sym, rhs.to_string(), tail.to_string())
            }
            OrderedNode::Goto(block) => format!("goto {}", block),
            OrderedNode::If(op, lhs, rhs, conseq, alt) => format!(
                "if {} {} {}: goto {}\nelse goto {}",
                lhs.to_string(),
                stringify_op(op),
                rhs.to_string(),
                conseq,
                alt
            ),
        }
    }
}

impl ProgramNode {
    fn stringify(&self) -> String {
        self.blocks
            .iter()
            .map(|block| format!("{}:\n{}", block.0, block.1.stringify()))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl ToString for TerminalNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for ReturnableNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for OrderedNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for ProgramNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}
