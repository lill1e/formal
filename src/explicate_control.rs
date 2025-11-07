use crate::rco::{AtomicNode, ComplexNode};

#[derive(Debug, Clone)]
pub enum TerminalNode {
    Number(u32),
    Reference(String),
}

#[derive(Debug, Clone)]
pub enum ReturnableNode {
    Addition(TerminalNode, TerminalNode),
    Terminal(TerminalNode),
}

#[derive(Debug, Clone)]
pub enum OrderedNode {
    Return(ReturnableNode),
    Binding(String, ReturnableNode, Box<OrderedNode>),
}

impl AtomicNode {
    pub fn explicate_tail(self) -> OrderedNode {
        match self {
            AtomicNode::Number(n) => {
                OrderedNode::Return(ReturnableNode::Terminal(TerminalNode::Number(n)))
            }
            AtomicNode::Reference(sym) => OrderedNode::Return(ReturnableNode::Terminal(
                TerminalNode::Reference(sym.to_string()),
            )),
        }
    }

    pub fn explicate_assign(self, binding: String, tail: OrderedNode) -> OrderedNode {
        match self {
            AtomicNode::Number(n) => OrderedNode::Binding(
                binding,
                ReturnableNode::Terminal(TerminalNode::Number(n)),
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
            AtomicNode::Number(n) => TerminalNode::Number(n),
            AtomicNode::Reference(sym) => TerminalNode::Reference(sym.to_string()),
        }
    }
}

impl ComplexNode {
    pub fn is_pure(&self) -> bool {
        match self {
            ComplexNode::Addition(_, _) => true,
            ComplexNode::Atomic(_) => true,
            ComplexNode::Let(_, rhs, body) => rhs.is_pure() && body.is_pure(),
            ComplexNode::Begin(exprs, last) => {
                exprs.iter().map(|e| e.is_pure()).all(|v| v) && last.is_pure()
            }
        }
    }

    pub fn explicate_effect(self, tail: OrderedNode) -> OrderedNode {
        match self {
            n if n.is_pure() => tail,
            _ => panic!("explicate_effect received an impure value (needs impl)"),
        }
    }

    pub fn explicate_tail(self) -> OrderedNode {
        match self {
            ComplexNode::Addition(a1, a2) => {
                OrderedNode::Return(ReturnableNode::Addition(a1.as_terminal(), a2.as_terminal()))
            }
            ComplexNode::Atomic(a) => a.explicate_tail(),
            ComplexNode::Let(sym, binding, body) => {
                binding.explicate_assign(sym, body.explicate_tail())
            }
            ComplexNode::Begin(exprs, last) => {
                let mut tail = last.explicate_tail();
                for expr in exprs {
                    tail = expr.explicate_effect(tail);
                }
                return tail;
            }
        }
    }

    pub fn explicate_assign(self, binding: String, tail: OrderedNode) -> OrderedNode {
        match self {
            ComplexNode::Atomic(a) => a.explicate_assign(binding, tail),
            ComplexNode::Addition(a1, a2) => OrderedNode::Binding(
                binding,
                ReturnableNode::Addition(a1.as_terminal(), a2.as_terminal()),
                Box::new(tail),
            ),
            ComplexNode::Let(sym, rhs, body) => {
                rhs.explicate_assign(sym, body.explicate_assign(binding, tail))
            }
            ComplexNode::Begin(exprs, last) => {
                let mut seq_tail = last.explicate_assign(binding, tail);
                for expr in exprs {
                    seq_tail = expr.explicate_effect(seq_tail)
                }
                return seq_tail;
            }
        }
    }

    pub fn explicate_control(self) -> OrderedNode {
        return self.explicate_tail();
    }
}

impl TerminalNode {
    fn stringify(&self) -> String {
        match self {
            TerminalNode::Number(n) => n.to_string(),
            TerminalNode::Reference(sym) => sym.to_string(),
        }
    }
}

impl ReturnableNode {
    fn stringify(&self) -> String {
        match self {
            ReturnableNode::Addition(n, m) => format!("{} + {}", n.to_string(), m.to_string()),
            ReturnableNode::Terminal(n) => n.to_string(),
        }
    }
}

impl OrderedNode {
    fn stringify(&self) -> String {
        match self {
            OrderedNode::Return(v) => format!("return {}", v.to_string()),
            OrderedNode::Binding(sym, rhs, tail) => {
                format!("{} = {}\n{}", sym, rhs.to_string(), tail.to_string())
            }
        }
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
