use crate::{common::Span, epi_ast, prob_ast};

lalrpop_util::lalrpop_mod!(epi_parser, "/epi_parser.rs");
lalrpop_util::lalrpop_mod!(prob_parser, "/prob_parser.rs");

pub fn parse_epi(src: &str) -> Result<epi_ast::Document, ParseError> {
    static PARSER: once_cell::sync::Lazy<epi_parser::DocumentParser> =
        once_cell::sync::Lazy::new(epi_parser::DocumentParser::new);

    match PARSER.parse(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(src, e)),
    }
}
pub fn parse_prob(src: &str) -> Result<Vec<prob_ast::Fact>, ParseError> {
    static PARSER: once_cell::sync::Lazy<prob_parser::FactsParser> =
        once_cell::sync::Lazy::new(prob_parser::FactsParser::new);

    match PARSER.parse(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(src, e)),
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
pub enum ParseError {
    #[error("Invalid Token")]
    InvalidToken {
        #[source_code]
        src: String,
        #[label("This token is not valid in this context")]
        err_span: Span,
    },
    #[error("Unrecognized Token")]
    #[diagnostic(help("Expected tokens here are: {expected}"))]
    UnrecognizedToken {
        #[source_code]
        src: String,
        #[label = "The token \"{token}\" is unrecognized in this context."]
        err_span: Span,
        token: String,
        expected: String,
    },
    #[error("Unrecognized EOF")]
    #[diagnostic(help("Expected tokens in this context are:\n{expected}"))]
    UnrecognizedEOF {
        #[source_code]
        src: String,
        #[label = "The document ends too early. Are you missing a token?"]
        err_span: Span,
        expected: String,
    },
    #[error("Assignment of non-function expression to multiple variable")]
    #[diagnostic(help("Only methods allow assignment to multiple variables"))]
    ExprAssignmentToMultipleVars {
        #[source_code]
        src: String,
        #[label = "The assignment to multiple variables occurs here, but only one is allowed"]
        err_span: Span,
    },
}

impl ParseError {
    fn new(
        src: &str,
        e: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, Self>,
    ) -> Self {
        let prep_src = || format!("{}\n", src);

        match e {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::InvalidToken {
                src: prep_src(),
                err_span: Span::start_len(location, 0),
            },
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                ParseError::UnrecognizedEOF {
                    src: prep_src(),
                    err_span: Span::start_len(location, 0),
                    expected: expected.join(", "),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseError::UnrecognizedToken {
                    src: prep_src(),
                    err_span: Span::start_end(token.0, token.0),
                    token: token.1.to_string(),
                    expected: expected.join(", "),
                }
            }
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { error } => match error {
                ParseError::ExprAssignmentToMultipleVars { err_span, .. } => {
                    ParseError::ExprAssignmentToMultipleVars {
                        src: prep_src(),
                        err_span,
                    }
                }
                _ => error,
            },
        }
    }
}
