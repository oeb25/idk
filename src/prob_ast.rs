use crate::common::Ident;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum Fact {
    #[display(fmt = "Agents: {}", r#"_0.iter().format(",")"#)]
    Agents(Vec<State>),
    #[display(fmt = "m({_0}) = {_1}")]
    M(States, f64),
    #[display(fmt = "Bel({_0}) = {_1}")]
    Bel(States, f64),
    #[display(fmt = "Plaus({_0}) = {_1}")]
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
