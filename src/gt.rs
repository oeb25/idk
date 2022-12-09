use comfy_table::Table;
use itertools::Itertools;
use miette::{Context, IntoDiagnostic};
use yansi::Paint;
use z3::ast::Ast;

use crate::common::{real, real_to_f64};

#[derive(Debug, PartialEq, Clone)]
struct Setting {
    header: Vec<String>,
    rows: Vec<(String, Vec<(f64, f64)>)>,
}

impl Setting {
    fn parse(src: &str) -> miette::Result<Setting> {
        let mut lines = src.lines();

        let header = lines
            .next()
            .ok_or_else(|| miette::miette!("Table as no header"))?;
        let header = header
            .split('|')
            .skip(1)
            .map(|h| h.trim().to_string())
            .collect_vec();

        let mut rows = vec![];

        for (row, l) in lines.enumerate() {
            let mut cols = l.split('|');
            let name = cols
                .next()
                .ok_or_else(|| miette::miette!("Row {} has no row name", row + 1))?;

            let cols = cols
                .enumerate()
                .map(|(x, cell)| -> miette::Result<(f64, f64)> {
                    let (l, r) = cell.split_once(',').ok_or_else(|| {
                        miette::miette!(
                            "'{cell}' ({row},{x}) had invalid format. Expected 'f64,f64' like '2,3'"
                        )
                    })?;

                    let l = l.trim();
                    let r = r.trim();

                    Ok((
                        l.parse().into_diagnostic().with_context(|| {
                            format!("Parsing float '{l}' in '{cell}' ({row},{x})")
                        })?,
                        r.parse().into_diagnostic().with_context(|| {
                            format!("Parsing float '{r}' in '{cell}' ({row},{x})")
                        })?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;

            rows.push((name.trim().to_string(), cols));
        }

        Ok(Self { header, rows })
    }

    fn rows(&self) -> impl Iterator<Item = (usize, &str, Vec<(f64, f64)>)> {
        self.rows
            .iter()
            .enumerate()
            .map(|(y, (n, row))| (y, n.as_str(), row.clone()))
    }
    fn cols(&self) -> impl Iterator<Item = (usize, &str, Vec<(f64, f64)>)> {
        self.header.iter().enumerate().map(|(y, n)| {
            (
                y,
                n.as_str(),
                self.rows.iter().map(|(_, data)| data[y]).collect_vec(),
            )
        })
    }

    fn remove_row(&mut self, y: usize) {
        self.rows.remove(y);
    }
    fn remove_col(&mut self, x: usize) {
        self.header.remove(x);
        for (_, row) in &mut self.rows {
            row.remove(x);
        }
    }

    fn surviving_elimination_of_weakly_dominated_strategies(&self) -> Option<Setting> {
        let mut remove_row = None;
        let mut remove_col = None;
        if self.rows.len() > 1 {
            for (y, n, row) in self.rows() {
                if self.rows().all(|(y2, _, r)| {
                    if y != y2 {
                        row.iter().zip(&r).any(|(a, b)| a.0 < b.0)
                            && row.iter().zip(r).all(|(a, b)| a.0 <= b.0)
                    } else {
                        true
                    }
                }) {
                    println!("Strategy '{n}' (row {}) is weakly dominated!", y + 1);
                    remove_row = Some(y);
                    break;
                }
            }
        }

        if self.cols().count() > 1 {
            for (x, n, col) in self.cols() {
                if self.cols().all(|(x2, _, r)| {
                    if x != x2 {
                        col.iter().zip(&r).any(|(a, b)| a.1 < b.1)
                            && col.iter().zip(r).all(|(a, b)| a.1 <= b.1)
                    } else {
                        true
                    }
                }) {
                    println!("Strategy '{n}' (column {}) is weakly dominated!", x + 1);
                    remove_col = Some(x);
                    break;
                }
            }
        }

        match (remove_row, remove_col) {
            (None, None) => None,
            (None, Some(remove_col)) => {
                let mut new = self.clone();
                new.remove_col(remove_col);
                Some(new)
            }
            (Some(remove_row), None) => {
                let mut new = self.clone();
                new.remove_row(remove_row);
                Some(new)
            }
            (Some(remove_row), Some(remove_col)) => {
                let mut new = self.clone();
                new.remove_row(remove_row);
                new.remove_col(remove_col);
                Some(new)
            }
        }
    }

    fn surviving_elimination_of_strict_dominated_strategies(&self) -> Option<Setting> {
        if self.rows.len() > 1 {
            let mut remove_row = None;

            for (y, n, row) in self.rows() {
                if let Some((to_remove, n2, _)) = self.rows().find(|(y2, _, r)| {
                    if y != *y2 {
                        row.iter().zip(r).all(|(a, b)| a.0 > b.0)
                    } else {
                        false
                    }
                }) {
                    println!(
                        "Strategy '{n2}' (row {}) is strictly dominated by '{n}'!",
                        to_remove + 1
                    );
                    remove_row = Some(to_remove);
                    break;
                }
            }

            if let Some(y) = remove_row {
                let mut new = self.clone();
                new.remove_row(y);
                return Some(new);
            }
        }

        if self.cols().count() > 1 {
            let mut remove_col = None;

            for (x, n, col) in self.cols() {
                if let Some((to_remove, n2, _)) = self.cols().find(|(x2, _, r)| {
                    if x != *x2 {
                        col.iter().zip(r).all(|(a, b)| a.1 > b.1)
                    } else {
                        false
                    }
                }) {
                    println!(
                        "Strategy '{n2}' (col {}) is strictly dominated by '{n}'!",
                        to_remove + 1
                    );
                    remove_col = Some(to_remove);
                    break;
                }
            }

            if let Some(x) = remove_col {
                let mut new = self.clone();
                new.remove_col(x);
                return Some(new);
            }
        }

        None
    }

    fn cells<'a>(&'a self) -> impl Iterator<Item = CellView<'a>> + 'a {
        self.rows
            .iter()
            .enumerate()
            .flat_map(move |(y, (rn, data))| {
                data.iter().enumerate().map(move |(x, cell)| {
                    CellView::new(y, rn.as_str(), x, self.header[x].as_str(), *cell)
                })
            })
    }

    fn pareto_improvements(&self, row: usize, col: usize) -> impl Iterator<Item = CellView> {
        let c1 = self.cell_at(row, col);

        self.cells().filter(move |c2| {
            if &c1 != c2 {
                (c2.cell.0 >= c1.cell.0 && c2.cell.1 >= c1.cell.1)
                    && (c2.cell.0 > c1.cell.0 || c2.cell.1 > c1.cell.1)
            } else {
                false
            }
        })
    }

    fn is_pareto_optimum(&self, row: usize, col: usize) -> bool {
        self.pareto_improvements(row, col).count() == 0
    }

    fn pareto_optimum(&self) -> impl Iterator<Item = CellView> {
        // If there is no outcome that every agent finds at least as
        // good and at least one agent finds better
        self.cells()
            .filter(|c| self.is_pareto_optimum(c.row, c.col))
    }

    fn cell_at(&self, row: usize, col: usize) -> CellView {
        CellView {
            row,
            row_strategy: &self.rows[row].0,
            col,
            col_strategy: &self.header[col],
            cell: self.rows[row].1[col],
        }
    }

    fn is_nash(&self, row: usize, col: usize) -> (bool, bool) {
        let c = self.cell_at(row, col);

        let l = self.rows().all(|r| {
            let d = r.2[col];
            d.0 <= c.cell.0
        });
        let r = self.cols().all(|r| {
            let d = r.2[row];
            d.1 <= c.cell.1
        });

        (l, r)
    }

    /// Computes the mixed strategy ratios for a 2x2 game
    fn mixed_strategy(&self) -> Option<(Vec<(i64, i64)>, Vec<(i64, i64)>)> {
        if self.cols().count() < 2 || self.rows().count() < 2 {
            return None;
        }

        let cfg = z3::Config::new();
        let ctx = z3::Context::new(&cfg);

        let solver = z3::Solver::new(&ctx);

        let ps = (0..self.cols().count())
            .map(|idx| z3::ast::Real::new_const(&ctx, format!("p{idx}")))
            .collect_vec();
        let qs = (0..self.rows().count())
            .map(|idx| z3::ast::Real::new_const(&ctx, format!("q{idx}")))
            .collect_vec();

        for c in ps.iter().chain(&qs) {
            solver.assert(&(real(&ctx, 0.0).le(&c) & c.le(&real(&ctx, 1.0))));
        }

        solver.assert(
            &ps.iter()
                .cloned()
                .reduce(|a, b| a + b)
                .unwrap()
                ._eq(&real(&ctx, 1.0)),
        );
        solver.assert(
            &qs.iter()
                .cloned()
                .reduce(|a, b| a + b)
                .unwrap()
                ._eq(&real(&ctx, 1.0)),
        );

        let values_1 = self
            .cols()
            .map(|(_, _, data)| {
                data.iter()
                    .zip(&ps)
                    .map(|(l, r)| real(&ctx, l.1) * r)
                    .reduce(|a, b| a + b)
                    .unwrap_or_else(|| real(&ctx, 0.0))
            })
            .collect_vec();
        let values_2 = self
            .rows()
            .map(|(_, _, data)| {
                data.iter()
                    .zip(&qs)
                    .map(|(l, r)| real(&ctx, l.0) * r)
                    .reduce(|a, b| a + b)
                    .unwrap_or_else(|| real(&ctx, 0.0))
            })
            .collect_vec();

        let eq_1 = values_1
            .iter()
            .tuple_windows()
            .map(|(a, b)| a._eq(b))
            .reduce(|a, b| a & b)
            .unwrap();
        let eq_2 = values_2
            .iter()
            .tuple_windows()
            .map(|(a, b)| a._eq(b))
            .reduce(|a, b| a & b)
            .unwrap();

        solver.assert(&eq_1);
        solver.assert(&eq_2);

        match solver.check() {
            z3::SatResult::Unsat => None,
            z3::SatResult::Unknown => None,
            z3::SatResult::Sat => {
                let model = solver.get_model().unwrap();

                let prep = |x: z3::ast::Real| x.as_real().unwrap();

                let ps = ps
                    .iter()
                    .map(|p| prep(model.eval(p, true).unwrap()))
                    .collect_vec();
                let qs = qs
                    .iter()
                    .map(|q| prep(model.eval(q, true).unwrap()))
                    .collect_vec();

                Some((ps, qs))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct CellView<'a> {
    row: usize,
    row_strategy: &'a str,
    col: usize,
    col_strategy: &'a str,
    cell: (f64, f64),
}

impl<'a> CellView<'a> {
    fn new(
        row: usize,
        row_strategy: &'a str,
        col: usize,
        col_strategy: &'a str,
        cell: (f64, f64),
    ) -> Self {
        Self {
            row,
            row_strategy,
            col,
            col_strategy,
            cell,
        }
    }
}

impl std::fmt::Display for Setting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use comfy_table::{
            modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, CellAlignment,
            ContentArrangement,
        };

        let probabilities = self.mixed_strategy();

        let mut table = Table::new();
        table
            .load_preset(UTF8_FULL)
            .apply_modifier(UTF8_ROUND_CORNERS)
            .set_content_arrangement(ContentArrangement::Dynamic);
        let mut header = self.header.clone();

        if let Some((_, qs)) = &probabilities {
            for (h, q) in header.iter_mut().zip(qs) {
                *h += &format!(" [{}/{}]", q.0, q.1);
            }
        }

        header.insert(0, String::new());
        table.set_header(header.iter().map(|h| {
            Cell::new(h)
                .add_attribute(Attribute::Bold)
                .set_alignment(CellAlignment::Center)
        }));

        for (row, (n, data)) in self.rows.iter().enumerate() {
            let n = if let Some((ps, _)) = &probabilities {
                let p = ps[row];
                format!("{n} [{}/{}]", p.0, p.1)
            } else {
                n.to_string()
            };

            table.add_row(
                std::iter::once(Cell::new(n).add_attribute(Attribute::Bold)).chain(
                    data.iter().enumerate().map(|(col, (l, r))| {
                        let (a, b) = self.is_nash(row, col);

                        let cell = Cell::new(format!(
                            "{l}{},{r}{}",
                            if a { "*" } else { "" },
                            if b { "*" } else { "" }
                        ));

                        match (a && b, self.is_pareto_optimum(row, col)) {
                            (true, true) => cell
                                .fg(comfy_table::Color::Yellow)
                                .add_attributes(vec![Attribute::Underlined, Attribute::Bold]),
                            (true, false) => cell
                                .fg(comfy_table::Color::Blue)
                                .add_attributes(vec![Attribute::Underlined]),
                            (false, true) => cell.fg(comfy_table::Color::Red),
                            (false, false) => cell,
                        }
                    }),
                ),
            );
        }

        write!(f, "{}", table)
    }
}

pub fn run(src: &str) -> miette::Result<()> {
    let setting = Setting::parse(src)?;
    println!("{}", setting);

    println!(
        "{} {} {}",
        Paint::blue("[Pure nash]").underline(),
        Paint::red("[Pareto optimum]"),
        Paint::yellow("[Both]").bold().underline()
    );

    println!();

    if setting.mixed_strategy().is_none() {
        println!(
            "{}\n",
            Paint::red("No mixed-strategy Nash Equilibrium exists")
                .bold()
                .underline()
                .italic(),
        );
    }

    println!(
        "{} {}",
        Paint::cyan("Pure nash equilibrium:"),
        setting
            .cells()
            .filter(|c| setting.is_nash(c.row, c.col) == (true, true))
            .map(|c| Paint::blue(format!("({},{})", c.row_strategy, c.col_strategy)).underline())
            .format(", ")
    );
    println!(
        "{} {}",
        Paint::cyan("Pareto optimum:"),
        setting
            .pareto_optimum()
            .map(|c| Paint::red(format!("({},{})", c.row_strategy, c.col_strategy)))
            .format(", ")
    );

    println!();
    println!("{}", Paint::yellow("Computing pareto improvements:"));

    for c in setting.cells() {
        let imp = setting
            .pareto_improvements(c.row, c.col)
            .filter(|c| setting.is_nash(c.row, c.col) == (true, true))
            .map(|c| format!("({},{})", c.row_strategy, c.col_strategy))
            .join(", ");

        if imp.is_empty() {
            println!(
                "  ({},{}): {}",
                c.row_strategy,
                c.col_strategy,
                Paint::new("has no improvements").dimmed()
            );
        } else {
            println!("  ({},{}): {imp}", c.row_strategy, c.col_strategy)
        }
    }

    {
        let mut setting = setting.clone();

        println!();
        println!(
            "{}",
            Paint::yellow("Computing surviving elimination of weakly dominated strategies:")
        );

        while let Some(next) = setting.surviving_elimination_of_weakly_dominated_strategies() {
            setting = next;
            println!("{}", setting);
        }
    }
    {
        let mut setting = setting.clone();

        println!();
        println!(
            "{}",
            Paint::yellow("Computing surviving elimination of strictly dominated strategies:")
        );

        while let Some(next) = setting.surviving_elimination_of_strict_dominated_strategies() {
            setting = next;
            println!("{}", setting);
        }
    }

    Ok(())
}
