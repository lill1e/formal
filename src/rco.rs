use std::collections::HashMap;

use crate::parser::{BinaryOperation, Node, UnaryOperation};

#[derive(Debug, Clone)]
pub enum AtomicNode {
    Number(i32),
    Boolean(bool),
    Void,
    Reference(String),
}

#[derive(Debug, Clone)]
pub enum ComplexNode {
    Binary(BinaryOperation, AtomicNode, AtomicNode),
    Unary(UnaryOperation, AtomicNode),
    Let(String, Box<ComplexNode>, Box<ComplexNode>),
    Atomic(AtomicNode),
    Begin(Vec<ComplexNode>, Box<ComplexNode>),
    Assignment(String, Box<ComplexNode>),
}

fn make_lets(lets: Vec<(String, ComplexNode)>, after: ComplexNode) -> ComplexNode {
    let mut curr = after;
    if lets.is_empty() {
        return curr;
    }
    for i in lets.len() - 1..=0 {
        curr = ComplexNode::Let(
            lets[i].0.clone(),
            Box::new(lets[i].1.clone()),
            Box::new(curr),
        );
    }
    return curr;
}

impl Node {
    fn rco_atom(self, counter: &mut u32) -> (AtomicNode, HashMap<String, ComplexNode>) {
        match self {
            Node::Number(n) => (AtomicNode::Number(n), HashMap::new()),
            Node::Boolean(b) => (AtomicNode::Boolean(b), HashMap::new()),
            Node::Void => (AtomicNode::Void, HashMap::new()),
            Node::Reference(sym) => (AtomicNode::Reference(sym), HashMap::new()),
            Node::Binary(_, _, _)
            | Node::Unary(_, _)
            | Node::Begin(_, _)
            | Node::Let(_, _, _)
            | Node::Assignment(_, _) => {
                let binding = format!("tmp.{}", counter);
                *counter += 1;
                return (
                    AtomicNode::Reference(binding.clone()),
                    HashMap::from([(binding.clone(), self.rco_exp(counter))]),
                );
            }
        }
    }

    fn rco_exp(self, counter: &mut u32) -> ComplexNode {
        match self {
            Node::Void => ComplexNode::Atomic(AtomicNode::Void),
            Node::Number(n) => ComplexNode::Atomic(AtomicNode::Number(n)),
            Node::Boolean(b) => ComplexNode::Atomic(AtomicNode::Boolean(b)),
            Node::Binary(op, b1, b2) => {
                let tmp1 = (*b1).rco_atom(counter);
                let tmp2 = (*b2).rco_atom(counter);
                make_lets(
                    vec![tmp1.1.clone(), tmp2.1.clone()]
                        .into_iter()
                        .flat_map(|m| m.into_iter().collect::<Vec<_>>())
                        .collect::<Vec<_>>(),
                    ComplexNode::Binary(op, tmp1.0, tmp2.0),
                )
            }
            Node::Unary(op, b) => {
                let tmp = (*b).rco_atom(counter);
                make_lets(
                    vec![tmp.1.clone()]
                        .into_iter()
                        .flat_map(|m| m.into_iter().collect::<Vec<_>>())
                        .collect(),
                    ComplexNode::Unary(op, tmp.0),
                )
            }
            Node::Begin(exprs, last) => ComplexNode::Begin(
                exprs.into_iter().map(|e| e.rco_exp(counter)).collect(),
                Box::new(last.rco_exp(counter)),
            ),
            Node::Let(sym, rhs, body) => ComplexNode::Let(
                sym,
                Box::new(rhs.rco_exp(counter)),
                Box::new(body.rco_exp(counter)),
            ),
            Node::Reference(sym) => ComplexNode::Atomic(AtomicNode::Reference(sym)),
            Node::Assignment(sym, rhs) => {
                ComplexNode::Assignment(sym, Box::new(rhs.rco_exp(counter)))
            }
        }
    }

    pub fn remove_complex_operands(self) -> ComplexNode {
        let mut counter = 0;
        return self.rco_exp(&mut counter);
    }
}

impl AtomicNode {
    fn stringify(&self) -> String {
        match self {
            AtomicNode::Void => String::from("void"),
            AtomicNode::Number(n) => n.to_string(),
            AtomicNode::Boolean(b) => b.to_string(),
            AtomicNode::Reference(sym) => sym.to_string(),
        }
    }
}

impl ComplexNode {
    fn stringify(&self) -> String {
        match self {
            ComplexNode::Atomic(node) => node.stringify(),
            ComplexNode::Binary(BinaryOperation::Addition, n1, n2) => {
                format!("(+ {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Binary(BinaryOperation::Subtraction, n1, n2) => {
                format!("(- {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Let(sym, binding, body) => format!(
                "(let [({} {})] {})",
                sym,
                binding.stringify(),
                body.stringify()
            ),
            ComplexNode::Begin(exprs, last) => format!(
                "{}\n{}",
                exprs
                    .iter()
                    .map(|e| e.stringify())
                    .collect::<Vec<String>>()
                    .join("\n"),
                last.stringify()
            ),
            ComplexNode::Assignment(sym, rhs) => format!("(set! {} {})", sym, rhs.stringify()),
            ComplexNode::Binary(BinaryOperation::Equals, n1, n2) => {
                format!("(eq? {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Binary(BinaryOperation::NotEquals, n1, n2) => {
                format!("(not (eq? {} {}))", n1.stringify(), n2.stringify())
            }
            ComplexNode::Unary(UnaryOperation::Negation, n) => format!("(- {})", n.stringify()),
            ComplexNode::Unary(UnaryOperation::Not, n) => format!("(not {})", n.stringify()),
            ComplexNode::Binary(BinaryOperation::Greater, n1, n2) => {
                format!("(> {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Binary(BinaryOperation::GreaterEqual, n1, n2) => {
                format!("(>= {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Binary(BinaryOperation::Less, n1, n2) => {
                format!("(< {} {})", n1.stringify(), n2.stringify())
            }
            ComplexNode::Binary(BinaryOperation::LessEqual, n1, n2) => {
                format!("(<= {} {})", n1.stringify(), n2.stringify())
            }
        }
    }
}

impl ToString for AtomicNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}

impl ToString for ComplexNode {
    fn to_string(&self) -> String {
        return self.stringify();
    }
}
