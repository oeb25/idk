use crate::common::Ident;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, derive_more::Display)]
pub enum Fact {
    #[display(fmt = "States: {}", r#"_0.iter().format(",")"#)]
    States(States),
    #[display(fmt = "m{_0}({_1}) = {_2}")]
    M(Ident, States, f64),
    #[display(fmt = "Bel{_0}({_1}) = {_2}")]
    Bel(Ident, States, f64),
    #[display(fmt = "Plaus{_0}({_1}) = {_2}")]
    Plaus(Ident, States, f64),
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{{{}}}", "_0.iter().format(\",\")")]
pub struct States(pub Vec<State>);
impl States {
    pub fn iter(&self) -> impl Iterator<Item = State> + '_ {
        self.0.iter().copied()
    }
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "{_0}")]
pub struct State(pub Ident);
impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
