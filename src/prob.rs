use comfy_table::Table;
use itertools::Itertools;
use z3::{
    ast::{Ast, Real, BV},
    Config, Context, FuncDecl, Model, Solver, Sort,
};

use crate::prob_ast::Fact;

pub fn start(facts: &[Fact]) -> miette::Result<()> {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    run(&ctx, facts)?;
    Ok(())
}

struct Probability<'a> {
    ctx: &'a Context,
    names: Vec<String>,
    all: Vec<BV<'a>>,
    m_fun: FuncDecl<'a>,
    bel_fun: FuncDecl<'a>,
    plaus_fun: FuncDecl<'a>,
}

impl<'a> Probability<'a> {
    fn new(ctx: &'a Context, names: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let names = names.into_iter().map(|n| n.into()).collect_vec();
        let n = names.len();

        let all = (0..1i64 << n)
            .sorted_by_key(|s| s.count_ones())
            .map(|f| BV::from_i64(ctx, f, n as _))
            .collect_vec();

        let m_fun = FuncDecl::new(ctx, "m", &[&Sort::bitvector(ctx, n as _)], &Sort::real(ctx));
        let bel_fun = FuncDecl::new(
            ctx,
            "Bel",
            &[&Sort::bitvector(ctx, n as _)],
            &Sort::real(ctx),
        );
        let plaus_fun = FuncDecl::new(
            ctx,
            "Plaus",
            &[&Sort::bitvector(ctx, n as _)],
            &Sort::real(ctx),
        );

        let p = Self {
            ctx,
            names,
            all,
            m_fun,
            bel_fun,
            plaus_fun,
        };

        p
    }
    fn new_const<const N: usize>(
        ctx: &'a Context,
        names: [impl Into<String>; N],
    ) -> (Self, [BV<'a>; N]) {
        let all = (0..1i64 << N)
            .sorted_by_key(|s| s.count_ones())
            .map(|f| BV::from_i64(ctx, f, N as _))
            .collect_vec();

        let m_fun = FuncDecl::new(ctx, "m", &[&Sort::bitvector(ctx, N as _)], &Sort::real(ctx));
        let bel_fun = FuncDecl::new(
            ctx,
            "Bel",
            &[&Sort::bitvector(ctx, N as _)],
            &Sort::real(ctx),
        );
        let plaus_fun = FuncDecl::new(
            ctx,
            "Plaus",
            &[&Sort::bitvector(ctx, N as _)],
            &Sort::real(ctx),
        );

        let p = Self {
            ctx,
            names: names.map(|n| n.into()).to_vec(),
            all,
            m_fun,
            bel_fun,
            plaus_fun,
        };

        (
            p,
            std::array::from_fn(|x| BV::from_i64(ctx, 1 << x, N as _)),
        )
    }

    fn n(&self) -> u32 {
        self.names.len() as _
    }

    fn assert_preamble(&self, solver: &Solver<'a>) {
        solver.assert(
            &self
                .all
                .iter()
                .map(|x| self.m(x))
                .fold(real(self.ctx, 0.0), |a, b| a + b)
                ._eq(&real(self.ctx, 1.0)),
        );
        let x = BV::new_const(self.ctx, "x", 3);
        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self
                .plaus(&x)
                ._eq(&self.all.iter().fold(real(self.ctx, 0.0), |a, b| {
                    a + (b & &x)
                        ._eq(&BV::from_i64(self.ctx, 0, 3))
                        .ite(&real(self.ctx, 0.0), &self.m(b))
                })),
        ));
        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self.plaus(&x)._eq(&(real(self.ctx, 1.0) - self.bel(&!&x))),
        ));
        solver.assert(
            &self
                .bel(&BV::from_i64(self.ctx, (1 << self.n()) - 1, self.n() as _))
                ._eq(&real(self.ctx, 1.0)),
        );
    }

    fn m(&self, x: &BV<'a>) -> Real {
        self.m_fun.apply(&[x]).as_real().unwrap()
    }
    fn bel(&self, x: &BV<'a>) -> Real {
        self.bel_fun.apply(&[x]).as_real().unwrap()
    }
    fn plaus(&self, x: &BV<'a>) -> Real {
        self.plaus_fun.apply(&[x]).as_real().unwrap()
    }

    fn table(&self, model: Model) -> Table {
        let mut table = Table::new();
        table.set_header(["X", "m(X)", "Bel(X)", "Plaus(X)"]);

        for idx in (1..1i64 << self.n()).sorted_by_key(|s| s.count_ones()) {
            let eval = |x| real_to_f64(&model.eval(&x, true).unwrap()).unwrap();

            table.add_row([
                self.names
                    .iter()
                    .enumerate()
                    .filter_map(|(i, n)| if idx & (1 << i) == 0 { None } else { Some(n) })
                    .format(",")
                    .to_string(),
                eval(self.m(&BV::from_i64(self.ctx, idx, self.n()))).to_string(),
                eval(self.bel(&BV::from_i64(self.ctx, idx, self.n()))).to_string(),
                eval(self.plaus(&BV::from_i64(self.ctx, idx, self.n()))).to_string(),
            ]);
        }

        table
    }

    fn bv_for(&self, n: &str) -> miette::Result<BV> {
        let idx = self
            .names
            .iter()
            .position(|p| p == n)
            .ok_or(miette::miette!(
                "Agent by the name '{n}' does not exists. Possible agents are '{}'",
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
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.m(&bv)._eq(&real(self.ctx, f)));
        Ok(())
    }

    fn assert_bel<'b>(
        &self,
        solver: &Solver<'a>,
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.bel(&bv)._eq(&real(self.ctx, f)));
        Ok(())
    }

    fn assert_plaus<'b>(
        &self,
        solver: &Solver<'a>,
        group: impl IntoIterator<Item = &'b str>,
        f: f64,
    ) -> miette::Result<()> {
        let bv = self.bv_for_group(group)?;
        solver.assert(&self.plaus(&bv)._eq(&real(self.ctx, f)));
        Ok(())
    }
}

fn real(ctx: &Context, f: f64) -> Real {
    let (a, b) = approximate_rational(f);
    Real::from_real(ctx, a, b)
}
fn run(ctx: &Context, facts: &[Fact]) -> miette::Result<()> {
    let mut sort_order = None;

    let mut states = facts
        .iter()
        .flat_map(|f| match f {
            Fact::Agents(agents) => {
                sort_order = Some(agents.clone());
                agents
            }
            Fact::M(agents, _) | Fact::Bel(agents, _) | Fact::Plaus(agents, _) => &agents.0,
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

    let p = Probability::new(ctx, states);

    let solver = Solver::new(ctx);
    p.assert_preamble(&solver);

    for f in facts {
        match f {
            Fact::Agents(_) => {}
            Fact::M(states, f) => p.assert_m(&solver, states.0.iter().map(|s| s.0.text()), *f)?,
            Fact::Bel(states, f) => {
                p.assert_bel(&solver, states.0.iter().map(|s| s.0.text()), *f)?
            }
            Fact::Plaus(states, f) => {
                p.assert_plaus(&solver, states.0.iter().map(|s| s.0.text()), *f)?
            }
        }
    }

    dbg!(solver.check());

    let model = solver.get_model().unwrap();

    let table = p.table(model);

    eprintln!("{table}");

    Ok(())
}

fn real_to_f64(r: &Real) -> Option<f64> {
    let (a, b) = r.as_real()?;
    Some(a as f64 / b as f64)
}

fn approximate_rational(f: f64) -> (i32, i32) {
    let sign = f.signum();
    let f = f.abs();

    let mut a = 0;
    let mut b = 1;

    while (((a as f64) / (b as f64)) - f).abs() > f64::EPSILON {
        let delta = (a as f64) / (b as f64);

        match delta.partial_cmp(&f).unwrap() {
            std::cmp::Ordering::Less => a += 1,
            std::cmp::Ordering::Equal => break,
            std::cmp::Ordering::Greater => b += 1,
        }
    }

    (a * sign as i32, b)
}
