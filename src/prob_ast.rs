use crate::common::Ident;

use itertools::Itertools;

pub enum Fact {
    M(States, f64),
    Bel(States, f64),
    Plaus(States, f64),
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{{{}}}", "_0.iter().format(\",\")")]
pub struct States(pub Vec<State>);
#[derive(Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{_0}")]
pub struct State(pub Ident);
impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
