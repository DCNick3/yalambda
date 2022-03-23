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

#[macro_use]
extern crate lalrpop_util;

use crate::ast::{Expr, UnnamedExpr};
use std::io::{stdin, Read};
use std::sync::Arc;

lalrpop_mod!(pub calculator1);

fn assert_eval(source: &str, expected_result: &str) {
    let ast: Arc<Expr> = calculator1::ExprParser::new().parse(source).unwrap();

    println!("Ast: {:#?}", ast);

    let unnamed_ast = ast.to_unnamed();

    println!("Unnamed Ast: {:#?}", unnamed_ast);

    let result = unnamed_ast.evaluate().unwrap();

    println!("Evaluated: {:#?}", result);

    let expected_result: Arc<Expr> = calculator1::ExprParser::new()
        .parse(expected_result)
        .unwrap();
    let expected_result = expected_result.to_unnamed();

    println!("Expected result: {:#?}", expected_result);

    assert!(UnnamedExpr::equivalent(&result, &expected_result));
}

macro_rules! test_eval {
    ($test_name:ident, $source:literal, $result:literal) => {
        #[test]
        fn $test_name() {
            assert_eval($source, $result);
        }
    };
}

test_eval!(simp_app1, r"(\x.x 0)", r"0");
test_eval!(simp_app2, r"(\x.x succ 0)", r"succ 0");
test_eval!(simp_if1, r"if true then false else true", r"false");
test_eval!(simp_if2, r"if true then 0 else succ 0", r"0");

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let ast: Arc<Expr> = calculator1::ExprParser::new().parse(&input).unwrap();

    println!("Ast: {:#?}", ast);

    let unnamed_ast = ast.to_unnamed();

    println!("Unnamed Ast: {:#?}", unnamed_ast);

    let result = unnamed_ast.evaluate().unwrap();

    println!("Evaluated: {:#?}", result);
}
