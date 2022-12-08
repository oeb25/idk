use comfy_table::Table;
use itertools::Itertools;
use miette::{Context, IntoDiagnostic};
use yansi::Paint;

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
        if self.rows.len() > 1 {
            let mut remove_row = None;

            for (y, n, row) in self.rows() {
                if self.rows().all(|(y2, _, r)| {
                    if y != y2 {
                        row.iter().zip(r).all(|(a, b)| a.0 < b.0)
                    } else {
                        true
                    }
                }) {
                    println!("Strategy '{n}' (row {}) is weakly dominated!", y + 1);
                    remove_row = Some(y);
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
                if self.cols().all(|(x2, _, r)| {
                    if x != x2 {
                        col.iter().zip(r).all(|(a, b)| a.1 < b.1)
                    } else {
                        true
                    }
                }) {
                    println!("Strategy '{n}' (column {}) is weakly dominated!", x + 1);
                    remove_col = Some(x);
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

    fn pareto_optimum(&self) -> impl Iterator<Item = CellView> {
        // If there is no outcome that every agent finds at least as
        // good and at least one agent finds better
        self.cells()
            .filter(|c1| self.pareto_improvements(c1.row, c1.col).count() == 0)
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
        let mut table = Table::new();
        let mut header = self.header.clone();
        header.insert(0, String::new());
        table.set_header(header);

        for (row, (n, data)) in self.rows.iter().enumerate() {
            table.add_row(
                std::iter::once(n.to_string()).chain(data.iter().enumerate().map(
                    |(col, (l, r))| {
                        let (a, b) = self.is_nash(row, col);
                        format!(
                            "{l}{},{r}{}",
                            if a { "*" } else { "" },
                            if b { "*" } else { "" }
                        )
                    },
                )),
            );
        }

        write!(f, "{}", table)
    }
}

pub fn run(src: &str) -> miette::Result<()> {
    let mut setting = Setting::parse(src)?;
    println!("{}", setting);

    println!(
        "{} {}",
        Paint::cyan("Pure nash equilibrium:"),
        setting
            .cells()
            .filter(|c| setting.is_nash(c.row, c.col) == (true, true))
            .map(|c| format!("({},{})", c.row_strategy, c.col_strategy))
            .format(", ")
    );
    println!(
        "{} {}",
        Paint::cyan("Pareto optimum:"),
        setting
            .pareto_optimum()
            .map(|c| format!("({},{})", c.row_strategy, c.col_strategy))
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
                "  ({},{}): has no improvements",
                c.row_strategy, c.col_strategy
            );
        } else {
            println!("  ({},{}): {imp}", c.row_strategy, c.col_strategy)
        }
    }

    println!();
    println!(
        "{}",
        Paint::yellow("Computing surviving elimination of weakly dominated strategies:")
    );

    while let Some(next) = setting.surviving_elimination_of_weakly_dominated_strategies() {
        setting = next;
        println!("{}", setting);
    }

    Ok(())
}
