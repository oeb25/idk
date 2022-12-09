use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;

use crate::{
    common::Ident,
    epi_ast::{Agent, Document, DocumentItem, Node, RelationType, Term},
};

pub fn run(doc: Document) {
    let mut model = Model::default();
    let mut queries = vec![];

    for i in &doc.items {
        match i {
            DocumentItem::Node(n, ts) => {
                model
                    .nodes
                    .entry(*n)
                    .or_default()
                    .facts
                    .extend_from_slice(ts);
            }
            DocumentItem::Relation(agents, rel) => {
                if matches!(rel.1, RelationType::To | RelationType::Between) {
                    let start = model.nodes.entry(rel.0).or_default();
                    for a in &agents.0 {
                        start.relations.entry(*a).or_default().insert(rel.2);
                    }
                }
                if matches!(rel.1, RelationType::From | RelationType::Between) {
                    let end = model.nodes.entry(rel.2).or_default();
                    for a in &agents.0 {
                        end.relations.entry(*a).or_default().insert(rel.0);
                    }
                }
            }
            DocumentItem::Query(t) => queries.push(t.clone()),
        }
    }

    use comfy_table::{
        modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, CellAlignment,
        ContentArrangement, Table,
    };

    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic);

    let nodes = model.nodes.keys().sorted().collect_vec();

    table.set_header(std::iter::once(Cell::new("")).chain(nodes.iter().map(|s| {
        Cell::new(s)
            .add_attribute(Attribute::Bold)
            .set_alignment(CellAlignment::Center)
    })));

    for q in &queries {
        table.add_row(
            std::iter::once(Cell::new(format!("{q}"))).chain(nodes.iter().map(|n| {
                let v = model.models(**n, q);
                let mut attrs = vec![];
                if v {
                    attrs.push(Attribute::Bold);
                } else {
                    attrs.push(Attribute::Dim);
                }
                Cell::new(v).add_attributes(attrs)
            })),
        );
    }
    println!("{table}");
}

#[derive(Debug, Clone, Default)]
struct NodeState {
    facts: Vec<Box<Term>>,
    relations: IndexMap<Agent, IndexSet<Node>>,
}

#[derive(Debug, Clone, Default)]
struct Model {
    nodes: IndexMap<Node, NodeState>,
}

impl Model {
    fn query(&self, t: &Term) -> IndexSet<Node> {
        self.nodes
            .keys()
            .copied()
            .filter(|n| self.models(*n, t))
            .collect()
    }
    fn models(&self, n: Node, t: &Term) -> bool {
        use crate::epi_ast::TermKind as TK;

        match &t.kind {
            TK::Boolean(b) => *b,
            TK::Var(_) => {
                if let Some(ns) = self.nodes.get(&n) {
                    if ns.facts.iter().any(|p| p.as_ref() == t) {
                        return true;
                    }
                    if ns
                        .facts
                        .iter()
                        .any(|p| p == &TK::Neg(Box::new(t.clone())).parsed(0, 0))
                    {
                        return false;
                    }

                    let cfg = z3::Config::new();
                    let ctx = z3::Context::new(&cfg);

                    let solver = z3::Solver::new(&ctx);

                    let pre = ns
                        .facts
                        .iter()
                        .map(|f| f.to_z3(&ctx))
                        .fold(None, |a, b| {
                            if let Some(a) = a {
                                Some(a & b)
                            } else {
                                Some(b)
                            }
                        })
                        .unwrap_or_else(|| z3::ast::Bool::from_bool(&ctx, true));

                    let cond = pre.implies(&t.to_z3(&ctx));

                    let all_vars: IndexSet<_> = ns
                        .facts
                        .iter()
                        .flat_map(|fact| fact.all_vars())
                        .chain(t.all_vars())
                        .collect();

                    let bounds: Vec<Box<dyn z3::ast::Ast>> = all_vars
                        .iter()
                        .map(|v| -> Box<dyn z3::ast::Ast> {
                            Box::new(z3::ast::Bool::new_const(&ctx, v.text()))
                        })
                        .collect();

                    let actual = z3::ast::forall_const(
                        &ctx,
                        &bounds.iter().map(|a| a.as_ref()).collect_vec(),
                        &[],
                        &cond,
                    );

                    solver.assert(&actual);

                    solver.check() == z3::SatResult::Sat
                } else {
                    false
                }
            }
            TK::Neg(i) => !self.models(n, i),
            TK::K(a, inner) => {
                if let Some(ns) = self.nodes.get(&n) {
                    if let Some(relations) = ns.relations.get(a) {
                        return relations.iter().all(|r| self.models(*r, inner));
                    }
                }
                true
            }
            TK::E(agents, inner) => agents
                .0
                .iter()
                .all(|a| self.models(n, &TK::K(*a, inner.clone()).parsed(0, 0))),
            TK::EBounded(k, agents, inner) => {
                if *k == 0 {
                    self.models(n, inner)
                } else {
                    self.models(
                        n,
                        &TK::E(
                            agents.clone(),
                            TK::EBounded(k - 1, agents.clone(), inner.clone()).parsed(0, 0),
                        )
                        .parsed(0, 0),
                    )
                }
            }
            TK::C(agents, inner) => (1..1000).all(|k| {
                self.models(
                    n,
                    &TK::EBounded(k, agents.clone(), inner.clone()).parsed(0, 0),
                )
            }),
            TK::D(agents, inner) => {
                if let Some(ns) = self.nodes.get(&n) {
                    let targets = ns
                        .relations
                        .iter()
                        .filter_map(|(a, targets)| {
                            if agents.0.contains(a) {
                                Some(targets.clone())
                            } else {
                                None
                            }
                        })
                        .reduce(|a, b| a.intersection(&b).copied().collect())
                        .unwrap_or_default();

                    targets.iter().all(|target| self.models(*target, inner))
                } else {
                    todo!()
                }
            }
            TK::PublicAnnouncement(a, b) => {
                if self.models(n, a) {
                    self.update_with(a).models(n, b)
                } else {
                    true
                }
            }
            TK::Con(l, r) => self.models(n, l) && self.models(n, r),
            TK::Dis(l, r) => self.models(n, l) || self.models(n, r),
            TK::Imp(l, r) => {
                if self.models(n, l) {
                    self.models(n, r)
                } else {
                    true
                }
            }
        }
    }

    pub fn update_with(&self, a: &Term) -> Model {
        Model {
            nodes: self
                .nodes
                .iter()
                .filter_map(|(n, ns)| {
                    if self.models(*n, a) {
                        Some((*n, ns.clone()))
                    } else {
                        None
                    }
                })
                .collect(),
        }
        // Model {
        //     nodes: self
        //         .nodes
        //         .iter()
        //         .map(|(n, ns)| {
        //             let mut new_facts = ns.facts.clone();
        //             new_facts.push(Box::new(a.clone()));
        //             (
        //                 *n,
        //                 NodeState {
        //                     facts: new_facts,
        //                     relations: ns.relations.clone(),
        //                 },
        //             )
        //         })
        //         .collect(),
        // }
    }
}

impl Term {
    fn to_z3<'ctx>(&self, ctx: &'ctx z3::Context) -> z3::ast::Bool<'ctx> {
        use crate::epi_ast::TermKind as TK;
        use z3::ast::Bool;

        match &self.kind {
            TK::Boolean(b) => Bool::from_bool(ctx, *b),
            TK::Var(p) => Bool::new_const(ctx, p.text()),
            TK::Neg(b) => !b.to_z3(ctx),
            TK::Con(l, r) => l.to_z3(ctx) | r.to_z3(ctx),
            TK::Dis(l, r) => l.to_z3(ctx) & r.to_z3(ctx),
            TK::Imp(l, r) => l.to_z3(ctx).implies(&r.to_z3(ctx)),
            TK::K(_, _)
            | TK::C(_, _)
            | TK::E(_, _)
            | TK::EBounded(_, _, _)
            | TK::D(_, _)
            | TK::PublicAnnouncement(_, _) => {
                todo!("Could not convert {self} to z3")
            }
        }
    }

    fn all_vars(&self) -> IndexSet<Ident> {
        use crate::epi_ast::TermKind as TK;

        match &self.kind {
            TK::Boolean(_) => Default::default(),
            TK::Var(v) => [*v].into_iter().collect(),
            TK::Neg(i) => i.all_vars(),
            TK::K(_, i) | TK::C(_, i) | TK::E(_, i) | TK::EBounded(_, _, i) | TK::D(_, i) => {
                i.all_vars()
            }
            TK::PublicAnnouncement(l, r) | TK::Con(l, r) | TK::Dis(l, r) | TK::Imp(l, r) => {
                l.all_vars().union(&r.all_vars()).copied().collect()
            }
        }
    }
}
