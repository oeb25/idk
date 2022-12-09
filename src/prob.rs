use comfy_table::Table;
use indexmap::IndexMap;
use itertools::Itertools;
use yansi::Paint;
use z3::{
    ast::{Ast, Real, BV},
    Config, Context, FuncDecl, Model, Solver, Sort,
};

use crate::{
    common::{real, real_to_f64},
    prob_ast::Fact,
};

pub fn start(facts: &[Fact]) -> miette::Result<()> {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    run(&ctx, facts)?;
    Ok(())
}

struct FnGroup<'a> {
    m: FuncDecl<'a>,
    bel: FuncDecl<'a>,
    plaus: FuncDecl<'a>,
}

struct Probability<'a> {
    ctx: &'a Context,
    suffixes: Vec<&'static str>,
    names: Vec<String>,
    all: Vec<(u64, BV<'a>)>,
    fns: IndexMap<&'static str, FnGroup<'a>>,
}

impl<'a> Probability<'a> {
    fn new(
        ctx: &'a Context,
        suffixes: impl IntoIterator<Item = &'static str>,
        names: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        let names = names.into_iter().map(|n| n.into()).collect_vec();
        let n = names.len();

        let all = (0..1u64 << n)
            .sorted_by_key(|s| s.count_ones())
            .map(|f| (f, BV::from_u64(ctx, f, n as _)))
            .collect_vec();

        let mut fns = IndexMap::new();
        let suffixes = suffixes.into_iter().collect_vec();

        for &suffix in &suffixes {
            fns.insert(suffix, create_fn_group(ctx, n, suffix));
        }

        Self {
            ctx,
            names,
            suffixes,
            all,
            fns,
        }
    }
    fn n(&self) -> u32 {
        self.names.len() as _
    }
    fn all(&self) -> impl Iterator<Item = (u64, &BV)> {
        self.all.iter().map(|(idx, bv)| (*idx, bv))
    }
    fn all_bv(&self) -> impl Iterator<Item = &BV> {
        self.all.iter().map(|(_, bv)| bv)
    }

    fn assert_preamble_for(&self, solver: &Solver<'a>, suffix: &str) {
        solver.assert(
            &self
                .all_bv()
                .map(|x| self.m(suffix, x))
                .fold(real(self.ctx, 0.0), |a, b| a + b)
                ._eq(&real(self.ctx, 1.0)),
        );

        solver.assert(
            &self
                .all_bv()
                .map(|x| {
                    self.m(suffix, x).ge(&real(self.ctx, 0.0))
                        & self.m(suffix, x).le(&real(self.ctx, 1.0))
                })
                .reduce(|a, b| a & b)
                .unwrap(),
        );

        let x = BV::new_const(self.ctx, "x", self.n());
        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self
                .plaus(suffix, &x)
                ._eq(&self.all_bv().fold(real(self.ctx, 0.0), |a, b| {
                    a + (b & &x)
                        ._eq(&BV::from_i64(self.ctx, 0, self.n()))
                        .ite(&real(self.ctx, 0.0), &self.m(suffix, b))
                })),
        ));

        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self
                .bel(suffix, &x)
                ._eq(&self.all_bv().fold(real(self.ctx, 0.0), |a, b| {
                    a + (b & &x)
                        ._eq(&b)
                        .ite(&self.m(suffix, b), &real(self.ctx, 0.0))
                })),
        ));

        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self
                .plaus(suffix, &x)
                ._eq(&(real(self.ctx, 1.0) - self.bel(suffix, &!&x))),
        ));

        solver.assert(
            &self
                .bel(
                    suffix,
                    &BV::from_i64(self.ctx, (1 << self.n()) - 1, self.n() as _),
                )
                ._eq(&real(self.ctx, 1.0)),
        );

        solver.assert(
            &self
                .m(suffix, &BV::from_i64(self.ctx, 0, self.n()))
                ._eq(&real(self.ctx, 0.0)),
        );
    }
    fn assert_preamble(&self, solver: &Solver<'a>) {
        for suf in &self.suffixes {
            self.assert_preamble_for(solver, suf);
        }
    }

    fn compute_combination(
        &mut self,
        solver: &Solver<'a>,
        model: &Model,
        a: &str,
        b: &str,
    ) -> &'static str {
        let ab = crate::str_intern::intern(&format!("{a},{b}"));

        let g = create_fn_group(self.ctx, self.n() as _, ab);

        let all_pairs = self
            .all()
            .filter(|x| x.0 != 0)
            .combinations_with_replacement(2)
            .flat_map(|xs| {
                if xs[0].0 == xs[1].0 {
                    vec![(xs[0], xs[1])]
                } else {
                    vec![(xs[0], xs[1]), (xs[1], xs[0])]
                }
            })
            .collect_vec();

        let k = all_pairs
            .iter()
            .filter_map(|(l, r)| {
                if (l.0 & r.0) == 0 {
                    Some(
                        self.compute_m(model, a, l.1).unwrap()
                            * self.compute_m(model, b, r.1).unwrap(),
                    )
                } else {
                    None
                }
            })
            .reduce(|l, r| l + r)
            .unwrap_or(0.0);

        for (u, u_bv) in self.all().filter(|x| x.0 != 0) {
            let body = all_pairs
                .iter()
                .filter_map(|(l, r)| {
                    let res = l.0 & r.0;
                    if res != 0 {
                        if res == u {
                            Some(
                                self.compute_m(model, a, l.1).unwrap()
                                    * self.compute_m(model, b, r.1).unwrap(),
                            )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .reduce(|l, r| l + r)
                .unwrap_or(0.0);

            let result = 1.0 / (1.0 - k) * body;

            solver.assert(&g.m(u_bv)._eq(&real(self.ctx, result)));
        }

        self.fns.insert(ab, g);
        self.assert_preamble_for(solver, ab);

        ab
    }

    fn m(&self, suffix: &str, x: &BV<'a>) -> Real {
        self.fns[suffix].m(x)
    }
    fn bel(&self, suffix: &str, x: &BV<'a>) -> Real {
        self.fns[suffix].bel(x)
    }
    fn plaus(&self, suffix: &str, x: &BV<'a>) -> Real {
        self.fns[suffix].plaus(x)
    }

    fn compute_m(&self, model: &Model, suffix: &str, x: &BV) -> Option<f64> {
        let res = model.eval(&self.m(suffix, x), true)?;
        real_to_f64(&res)
    }
    fn compute_bel(&self, model: &Model, suffix: &str, x: &BV) -> Option<f64> {
        let res = model.eval(&self.bel(suffix, x), true)?;
        real_to_f64(&res)
    }
    fn compute_plaus(&self, model: &Model, suffix: &str, x: &BV) -> Option<f64> {
        let res = model.eval(&self.plaus(suffix, x), true)?;
        real_to_f64(&res)
    }

    fn compute_name(&self, idx: u64) -> String {
        self.names
            .iter()
            .enumerate()
            .filter_map(|(i, n)| if idx & (1 << i) == 0 { None } else { Some(n) })
            .format(",")
            .to_string()
    }

    fn table(&self, suffix: &str, model: &Model) -> Table {
        use comfy_table::{
            modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, CellAlignment,
            ContentArrangement,
        };

        let mut table = Table::new();
        table
            .load_preset(UTF8_FULL)
            .apply_modifier(UTF8_ROUND_CORNERS)
            .set_content_arrangement(ContentArrangement::Dynamic);
        table.set_header(
            [
                "X".to_string(),
                format!("m{suffix}(X)"),
                format!("Bel{suffix}(X)"),
                format!("Plaus{suffix}(X)"),
            ]
            .map(|c| {
                Cell::new(c)
                    .add_attribute(Attribute::Bold)
                    .set_alignment(CellAlignment::Center)
            }),
        );

        for idx in (1..1i64 << self.n()).sorted_by_key(|s| s.count_ones()) {
            let bv = BV::from_i64(self.ctx, idx, self.n());

            table.add_row(
                [
                    self.compute_name(idx as _),
                    self.compute_m(model, suffix, &bv).unwrap().to_string(),
                    self.compute_bel(model, suffix, &bv).unwrap().to_string(),
                    self.compute_plaus(model, suffix, &bv).unwrap().to_string(),
                ]
                .map(|s| {
                    Cell::new(&s).add_attribute(if s == "0" {
                        Attribute::Dim
                    } else {
                        Attribute::NormalIntensity
                    })
                }),
            );
        }

        table
    }

    fn bv_for(&self, n: &str) -> miette::Result<BV> {
        let idx = self
            .names
            .iter()
            .position(|p| p == n)
            .ok_or(miette::miette!(
                "Agent by the name '{n}' does not exists. Possible states are '{}'",
                self.names.iter().format(", ")
            ))?;
        Ok(BV::from_i64(self.ctx, 1 << idx, self.n()))
    }
    fn bv_for_group<'b>(&self, group: impl IntoIterator<Item = &'b str>) -> miette::Result<BV> {
        let zero = BV::from_i64(self.ctx, 0, self.n());
        group
            .into_iter()
            .fold(Ok(zero), |acc, n| Ok(acc? | self.bv_for(n)?))
    }

    fn assert_m<'b>(
        &self,
        solver: &Solver<'a>,
        suffix: &str,
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.m(suffix, &bv)._eq(&real(self.ctx, f)));
        Ok(())
    }

    fn assert_bel<'b>(
        &self,
        solver: &Solver<'a>,
        suffix: &str,
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.bel(suffix, &bv)._eq(&real(self.ctx, f)));
        Ok(())
    }

    fn assert_plaus<'b>(
        &self,
        solver: &Solver<'a>,
        suffix: &str,
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.plaus(suffix, &bv)._eq(&real(self.ctx, f)));
        Ok(())
    }
}

impl<'a> FnGroup<'a> {
    fn m(&self, x: &BV<'a>) -> Real {
        self.m.apply(&[x]).as_real().unwrap()
    }
    fn bel(&self, x: &BV<'a>) -> Real {
        self.bel.apply(&[x]).as_real().unwrap()
    }
    fn plaus(&self, x: &BV<'a>) -> Real {
        self.plaus.apply(&[x]).as_real().unwrap()
    }
}

fn create_fn_group<'a>(ctx: &'a Context, n: usize, suffix: &str) -> FnGroup<'a> {
    let m = FuncDecl::new(
        ctx,
        format!("m{suffix}"),
        &[&Sort::bitvector(ctx, n as _)],
        &Sort::real(ctx),
    );
    let bel = FuncDecl::new(
        ctx,
        format!("bel{suffix}"),
        &[&Sort::bitvector(ctx, n as _)],
        &Sort::real(ctx),
    );
    let plaus = FuncDecl::new(
        ctx,
        format!("plaus{suffix}"),
        &[&Sort::bitvector(ctx, n as _)],
        &Sort::real(ctx),
    );
    FnGroup { m, bel, plaus }
}

fn run(ctx: &Context, facts: &[Fact]) -> miette::Result<()> {
    let mut suffixes = vec![];
    let mut sort_order = None;

    let mut states = facts
        .iter()
        .flat_map(|f| match f {
            Fact::States(states) => {
                sort_order = Some(states.clone());
                &states.0
            }
            Fact::M(suf, states, _) | Fact::Bel(suf, states, _) | Fact::Plaus(suf, states, _) => {
                if !suffixes.contains(&suf.text()) {
                    suffixes.push(suf.text());
                }
                &states.0
            }
        })
        .copied()
        .map(|s| s.0.text())
        .collect_vec();

    if let Some(sort_order) = sort_order {
        states.sort_by_key(|s| sort_order.iter().position(|p| p.0.text() == *s));
        states.dedup();
    } else {
        states.sort();
        states.dedup();
    }

    let mut p = Probability::new(ctx, suffixes.iter().copied(), states);

    let solver = Solver::new(ctx);

    for f in facts {
        match f {
            Fact::States(_) => {}
            Fact::M(suffix, states, f) => {
                p.assert_m(&solver, suffix, states.0.iter().map(|s| s.0.text()), *f)?
            }
            Fact::Bel(suffix, states, f) => {
                p.assert_bel(&solver, suffix, states.0.iter().map(|s| s.0.text()), *f)?
            }
            Fact::Plaus(suffix, states, f) => {
                p.assert_plaus(&solver, suffix, states.0.iter().map(|s| s.0.text()), *f)?
            }
        }
    }

    p.assert_preamble(&solver);
    dbg!(solver.check());

    let model = solver.get_model().expect("no model generated");

    for suf in &p.suffixes {
        let table = p.table(suf, &model);
        println!("{table}");
    }

    for (a, b) in p
        .suffixes
        .iter()
        .copied()
        .tuple_combinations()
        .collect_vec()
    {
        println!(
            "{}",
            Paint::yellow(format!("Computed combination of m{a} ⊕ m{b}:"))
        );
        let ab = p.compute_combination(&solver, &model, a, b);
        dbg!(solver.check());
        let model = solver.get_model().expect("no model generated");
        let table = p.table(ab, &model);
        println!("{table}");
    }

    Ok(())
}
