use comfy_table::Table;
use itertools::Itertools;
use z3::{
    ast::{Ast, Real, BV},
    Config, Context, FuncDecl, Model, Solver, Sort,
};

pub fn start() {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    run(&ctx);
}

struct Probability<'a, const N: usize> {
    ctx: &'a Context,
    names: [String; N],
    all: Vec<BV<'a>>,
    m_fun: FuncDecl<'a>,
    bel_fun: FuncDecl<'a>,
    plaus_fun: FuncDecl<'a>,
}

impl<'a, const N: usize> Probability<'a, N> {
    fn new(ctx: &'a Context, names: [impl Into<String>; N]) -> (Self, [BV<'a>; N]) {
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
            names: names.map(|n| n.into()),
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

    fn assert_preamble(&self, solver: &Solver<'a>) {
        let real = |f| {
            let (a, b) = approximate_rational(f);
            Real::from_real(self.ctx, a, b)
        };

        solver.assert(
            &self
                .all
                .iter()
                .map(|x| self.m(x))
                .fold(real(0.0), |a, b| a + b)
                ._eq(&real(1.0)),
        );
        let x = BV::new_const(self.ctx, "x", 3);
        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self.plaus(&x)._eq(&self.all.iter().fold(real(0.0), |a, b| {
                a + (b & &x)
                    ._eq(&BV::from_i64(self.ctx, 0, 3))
                    .ite(&real(0.0), &self.m(b))
            })),
        ));
        solver.assert(&z3::ast::forall_const(
            self.ctx,
            &[&x],
            &[],
            &self.plaus(&x)._eq(&(real(1.0) - self.bel(&!&x))),
        ));
        solver.assert(
            &self
                .bel(&BV::from_i64(self.ctx, (1 << N) - 1, N as _))
                ._eq(&real(1.0)),
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

        for idx in (1..1i64 << N).sorted_by_key(|s| s.count_ones()) {
            let eval = |x| real_to_f32(&model.eval(&x, true).unwrap()).unwrap();

            table.add_row([
                self.names
                    .iter()
                    .enumerate()
                    .filter_map(|(i, n)| if idx & (1 << i) == 0 { None } else { Some(n) })
                    .format(",")
                    .to_string(),
                eval(self.m(&BV::from_i64(self.ctx, idx, N as _))).to_string(),
                eval(self.bel(&BV::from_i64(self.ctx, idx, N as _))).to_string(),
                eval(self.plaus(&BV::from_i64(self.ctx, idx, N as _))).to_string(),
            ]);
        }

        table
    }
}

fn run(ctx: &Context) {
    const SIZE: usize = 3;

    let (p, [r, y, g]) = Probability::<SIZE>::new(ctx, ["R", "Y", "G"]);

    let solver = Solver::new(ctx);
    p.assert_preamble(&solver);

    let real = |f| {
        let (a, b) = approximate_rational(f);
        Real::from_real(ctx, a, b)
    };

    // m({}) = 0
    // m({R}) = 0.1
    // m({Y}) = 0.1
    // m({G}) = 0.1
    // m({R,G}) = 0.2
    // m({Y,G}) = 0.2
    // Bel({R,Y}) = 0.5
    // Plaus({R,Y}) = 0.9

    let zero = BV::from_i64(ctx, 0, 3);
    solver.assert(&p.m(&zero)._eq(&real(0.0)));
    solver.assert(&p.m(&r)._eq(&real(0.1)));
    solver.assert(&p.m(&y)._eq(&real(0.1)));
    solver.assert(&p.m(&g)._eq(&real(0.1)));
    // solver.assert(&p.m(&(&r | &y))._eq(&real(0.1)));
    solver.assert(&p.m(&(&r | &g))._eq(&real(0.2)));
    solver.assert(&p.m(&(&y | &g))._eq(&real(0.2)));

    solver.assert(&p.bel(&(&r | &y))._eq(&real(0.5)));
    solver.assert(&p.plaus(&(&y | &g))._eq(&real(0.9)));

    dbg!(solver.check());

    let model = solver.get_model().unwrap();

    let table = p.table(model);

    eprintln!("{table}");
}

fn real_to_f32(r: &Real) -> Option<f32> {
    let (a, b) = r.as_real()?;
    Some(a as f32 / b as f32)
}

fn approximate_rational(f: f32) -> (i32, i32) {
    let sign = f.signum();
    let f = f.abs();

    let mut a = 0;
    let mut b = 1;

    while (((a as f32) / (b as f32)) - f).abs() > f32::EPSILON {
        let delta = (a as f32) / (b as f32);

        match delta.partial_cmp(&f).unwrap() {
            std::cmp::Ordering::Less => a += 1,
            std::cmp::Ordering::Equal => break,
            std::cmp::Ordering::Greater => b += 1,
        }
    }

    (a * sign as i32, b)
}
