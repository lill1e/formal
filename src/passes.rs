use crate::parser::Node;

fn remove_redundant_begins(ast: Node) -> Node {
    match ast {
        Node::Void | Node::Boolean(_) | Node::Number(_) | Node::Reference(_) => ast,
        Node::Begin(exprs, fin) if exprs.is_empty() => remove_redundant_begins(*fin),
        Node::Begin(exprs, fin) => Node::Begin(
            exprs
                .iter()
                .map(|e| remove_redundant_begins(e.clone()))
                .collect(),
            Box::new(remove_redundant_begins(*fin)),
        ),
        Node::Let(sym, rhs, body) => Node::Let(
            sym,
            Box::new(remove_redundant_begins(*rhs)),
            Box::new(remove_redundant_begins(*body)),
        ),
        Node::Assignment(sym, rhs) => {
            Node::Assignment(sym, Box::new(remove_redundant_begins(*rhs)))
        }
        Node::Unary(op, child) => Node::Unary(op, Box::new(remove_redundant_begins(*child))),
        Node::Binary(op, x, y) => Node::Binary(
            op,
            Box::new(remove_redundant_begins(*x)),
            Box::new(remove_redundant_begins(*y)),
        ),
        Node::If(cond, conseq, alt) => Node::If(
            Box::new(remove_redundant_begins(*cond)),
            Box::new(remove_redundant_begins(*conseq)),
            Box::new(remove_redundant_begins(*alt)),
        ),
    }
}

pub fn passify(ast: Node, debug: bool) -> Node {
    let pass1 = remove_redundant_begins(ast);
    if debug {
        println!("Remove Redundant Begins:\n{:?}\n", pass1);
    }
    pass1
}
