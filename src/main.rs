// enum LambdaTerm {
//     Variable {
//         name: String,
//     },
//     Lambda {
//         bound_name: String,
//         body: Box<LambdaTerm>,
//     },
//     Application {
//         function: Box<LambdaTerm>,
//         argument: Box<LambdaTerm>,
//     },
// }
//
// enum UnnamedLambdaTerm {
//     Variable {
//         index: u32,
//     },
//     Lambda {
//         body: Box<UnnamedLambdaTerm>,
//     },
//     Application {
//         function: Box<UnnamedLambdaTerm>,
//         argument: Box<UnnamedLambdaTerm>,
//     },
// }

mod ast;

#[cfg(test)]
mod test;

use crate::ast::Expr;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lalrpop_util::lexer::Token;
use lalrpop_util::{lalrpop_mod, ParseError};
use std::io::{stdin, Read};
use std::sync::Arc;

lalrpop_mod!(pub yalamb);

fn show_diagnostic(source: &str, error: ParseError<usize, Token<'_>, &'static str>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add("/dev/stdin", source);

    let diagnostic = Diagnostic::error();

    let diagnostic = match error {
        ParseError::InvalidToken { location } => diagnostic
            .with_message("Invalid token")
            .with_labels(vec![Label::primary(file_id, location..location)]),
        ParseError::UnrecognizedEOF { location, expected } => diagnostic
            .with_message("Unrecognized EOF")
            .with_labels(vec![Label::primary(file_id, location..location)])
            .with_notes(vec![format!(
                "Expected one of the following: {}",
                expected.join(", ")
            )]),
        ParseError::UnrecognizedToken {
            token: (start, t, end),
            expected,
        } => diagnostic
            .with_message(format!("Unrecognized token: {}", t))
            .with_labels(vec![Label::primary(file_id, start..end)])
            .with_notes(vec![format!(
                "Expected one of the following: {}",
                expected.join(", ")
            )]),
        ParseError::ExtraToken {
            token: (start, t, end),
        } => diagnostic
            .with_message(format!("Extra token: {}", t))
            .with_labels(vec![Label::primary(file_id, start..end)]),
        ParseError::User { .. } => {
            unreachable!()
        }
    };

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let ast: Result<Arc<Expr>, ParseError<_, _, _>> =
        crate::yalamb::ExprParser::new().parse(&input);

    match ast {
        Ok(ast) => {
            println!("Ast: {:#?}", ast);

            println!("Type: {:#?}", ast.type_of().unwrap());

            let unnamed_ast = ast.to_unnamed();

            println!("Unnamed Ast: {:#?}", unnamed_ast);

            let result = unnamed_ast.evaluate().unwrap();

            println!("Evaluated: {:#?}", result);
        }
        Err(e) => show_diagnostic(&input, e),
    };
}
