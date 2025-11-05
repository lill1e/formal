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
    pub fn explicate_tail(self) -> OrderedNode {
        match self {
            ComplexNode::Addition(a1, a2) => {
                OrderedNode::Return(ReturnableNode::Addition(a1.as_terminal(), a2.as_terminal()))
            }
            ComplexNode::Atomic(a) => a.explicate_tail(),
            ComplexNode::Let(sym, binding, body) => {
                binding.explicate_assign(sym, body.explicate_tail())
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
