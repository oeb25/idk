use crate::common::{Ident, Span};
use itertools::Itertools;

#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{}", r#"items.iter().format("\n")"#)]
pub struct Document {
    pub items: Vec<DocumentItem>,
}

#[derive(Debug, Clone, derive_more::Display)]
pub enum DocumentItem {
    #[display(fmt = "@{_0}: {}", r#"_1.iter().format(",")"#)]
    Node(Node, Vec<Box<Term>>),
    #[display(fmt = "#{_0} {_1}")]
    Relation(Agents, Relation),
    #[display(fmt = "? {_0}")]
    Query(Box<Term>),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{_0}")]
pub struct Node(pub Ident);

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node({})", self.0)
    }
}

#[derive(Debug, Clone, Copy, derive_more::Display)]
#[display(fmt = "{_0} {_1} {_2}")]
pub struct Relation(pub Node, pub RelationType, pub Node);
#[derive(Debug, Clone, Copy, derive_more::Display)]
pub enum RelationType {
    #[display(fmt = "->")]
    To,
    #[display(fmt = "<-")]
    From,
    #[display(fmt = "<->")]
    Between,
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{{{}}}", "_0.iter().format(\",\")")]
pub struct Agents(pub Vec<Agent>);
#[derive(Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{_0}")]
pub struct Agent(pub Ident);
impl std::fmt::Debug for Agent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Eq, derive_more::Display)]
#[display(fmt = "{kind}")]
pub struct Term {
    pub kind: TermKind,
    pub span: Span,
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display)]
pub enum TermKind {
    #[display(fmt = "{_0}")]
    Boolean(bool),
    #[display(fmt = "{_0}")]
    Var(Ident),
    #[display(fmt = "¬{_0}")]
    Neg(Box<Term>),
    #[display(fmt = "(K{_0}{_1})")]
    K(Agent, Box<Term>),
    #[display(fmt = "(C{_0}{_1})")]
    C(Agents, Box<Term>),
    #[display(fmt = "(E{_0}{_1})")]
    E(Agents, Box<Term>),
    #[display(fmt = "(E^({_0}){_1}{_2})")]
    EBounded(u32, Agents, Box<Term>),
    #[display(fmt = "(D{_0}{_1})")]
    D(Agents, Box<Term>),
    #[display(fmt = "({_0} & {_1})")]
    Con(Box<Term>, Box<Term>),
    #[display(fmt = "({_0} | {_1})")]
    Dis(Box<Term>, Box<Term>),
    #[display(fmt = "({_0} -> {_1})")]
    Imp(Box<Term>, Box<Term>),
}

impl TermKind {
    pub(crate) fn parsed(self, p: usize, q: usize) -> Box<Term> {
        Box::new(Term {
            kind: self,
            span: Span::start_end(p, q),
        })
    }
}
