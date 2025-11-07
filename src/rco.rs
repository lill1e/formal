use std::collections::HashMap;

use crate::parser::Node;

#[derive(Debug, Clone)]
pub enum AtomicNode {
    Number(i32),
    Reference(String),
}

#[derive(Debug, Clone)]
pub enum ComplexNode {
    Addition(AtomicNode, AtomicNode),
    Let(String, Box<ComplexNode>, Box<ComplexNode>),
    Atomic(AtomicNode),
    Begin(Vec<ComplexNode>, Box<ComplexNode>),
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
            Node::Addition(_, _) => {
                let binding = format!("tmp.{}", counter);
                *counter += 1;
                return (
                    AtomicNode::Reference(binding.clone()),
                    HashMap::from([(binding.clone(), self.rco_exp(counter))]),
                );
            }
            Node::Begin(_, _) => {
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
            Node::Number(n) => ComplexNode::Atomic(AtomicNode::Number(n)),
            Node::Addition(b1, b2) => {
                let tmp1 = (*b1).rco_atom(counter);
                let tmp2 = (*b2).rco_atom(counter);
                make_lets(
                    vec![tmp1.1.clone(), tmp2.1.clone()]
                        .into_iter()
                        .flat_map(|m| m.into_iter().collect::<Vec<_>>())
                        .collect::<Vec<_>>(),
                    ComplexNode::Addition(tmp1.0, tmp2.0),
                )
            }
            Node::Begin(exprs, last) => ComplexNode::Begin(
                exprs.into_iter().map(|e| e.rco_exp(counter)).collect(),
                Box::new(last.rco_exp(counter)),
            ),
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
            AtomicNode::Number(n) => n.to_string(),
            AtomicNode::Reference(sym) => sym.to_string(),
        }
    }
}

impl ComplexNode {
    fn stringify(&self) -> String {
        match self {
            ComplexNode::Atomic(node) => node.stringify(),
            ComplexNode::Addition(n1, n2) => format!("(+ {} {})", n1.stringify(), n2.stringify()),
            ComplexNode::Let(sym, binding, body) => format!(
                "(let [({} {})] {})",
                sym,
                (*binding).stringify(),
                (*body).stringify()
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
