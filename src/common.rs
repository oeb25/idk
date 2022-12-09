use crate::str_intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Construct the zero span. This will be voided with any joins
    pub fn zero() -> Span {
        Self { start: 0, end: 0 }
    }
    /// Construct a new span starting at `start` and ending at `end`.
    pub fn start_end(start: usize, end: usize) -> Span {
        Self { start, end }
    }
    /// Construct a new span starting at `start` and ending at `start + len`.
    pub fn start_len(start: usize, len: usize) -> Span {
        Self {
            start,
            end: start + len,
        }
    }
    /// The start location in the source code.
    pub fn start(&self) -> usize {
        self.start
    }
    /// The end location in the source code.
    pub fn end(&self) -> usize {
        self.end
    }
    /// The length in bytes in the span.
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    /// Returns true if the span contains zero bytes.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Returns the span containing the two spans. If either of the spans are
    /// empty, the other will be returned.
    pub fn join(self, other: Span) -> Span {
        if self.is_empty() {
            return other;
        }
        if other.is_empty() {
            return self;
        }
        Span::start_end(self.start().min(other.start()), self.end().max(other.end()))
    }
}
impl From<miette::SourceSpan> for Span {
    fn from(s: miette::SourceSpan) -> Self {
        Span::start_len(s.offset(), s.len())
    }
}
impl From<Span> for miette::SourceSpan {
    fn from(s: Span) -> Self {
        (s.start(), s.len()).into()
    }
}
impl FromIterator<Span> for Span {
    fn from_iter<T: IntoIterator<Item = Span>>(iter: T) -> Self {
        iter.into_iter()
            .filter(|s| !s.is_empty())
            .reduce(|a, b| a.join(b))
            .unwrap_or(Span { start: 0, end: 0 })
    }
}

#[derive(Clone, Copy, Eq)]
/// An identifier in the source file, associated with its source location.
///
/// All of its comparison-related trait impl's throw out the source location,
/// and only considers the identifier string.
pub struct Ident {
    text: &'static str,
    span: Span,
}
impl Ident {
    /// Create a new interned `Ident`.
    pub fn new(text: &str, span: Span) -> Ident {
        Ident {
            text: str_intern::intern(text),
            span,
        }
    }
    /// The text string of the identifier
    pub fn text(self) -> &'static str {
        self.text
    }
    /// The source location of the identifier
    pub fn span(self) -> Span {
        self.span
    }
    pub fn skip(self, n: usize) -> Ident {
        Ident::new(
            &self.text()[n..],
            Span::start_len(self.span.start() + n, self.span.len() - n),
        )
    }
}
impl std::hash::Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text.hash(state);
    }
}
impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.text.fmt(f)
    }
}
impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.text.fmt(f)
    }
}
impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}
impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.text.partial_cmp(&other.text)
    }
}
impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl std::ops::Deref for Ident {
    type Target = str;

    fn deref(&self) -> &str {
        self.text
    }
}
impl From<Ident> for miette::SourceSpan {
    fn from(i: Ident) -> Self {
        i.span.into()
    }
}

pub fn real_to_f64(r: &z3::ast::Real) -> Option<f64> {
    let (a, b) = r.as_real()?;
    Some(a as f64 / b as f64)
}
pub fn real<'ctx>(ctx: &'ctx z3::Context, f: f64) -> z3::ast::Real<'ctx> {
    let (a, b) = approximate_rational(f);
    z3::ast::Real::from_real(ctx, a, b)
}
pub fn approximate_rational(f: f64) -> (i32, i32) {
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
