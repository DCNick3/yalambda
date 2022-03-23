use crate::ast::{Error, Expr, UnnamedExpr};
use std::sync::Arc;

fn parse(source: &str) -> Arc<UnnamedExpr> {
    let ast: Arc<Expr> = crate::yalamb::ExprParser::new().parse(source).unwrap();

    println!("Ast: {:#?}", ast);

    let unnamed_ast = ast.to_unnamed();

    println!("Unnamed Ast: {:#?}", unnamed_ast);

    unnamed_ast
}

fn eval(source: &str) -> Arc<UnnamedExpr> {
    let unnamed_ast = parse(source);

    let result = unnamed_ast.evaluate().unwrap();

    println!("Evaluated: {:#?}", result);

    result
}

fn assert_eval(source: &str, expected_result: &str) {
    let result = eval(source);

    let expected_result: Arc<Expr> = crate::yalamb::ExprParser::new()
        .parse(expected_result)
        .unwrap();
    let expected_result = expected_result.to_unnamed();

    println!("Expected result: {:#?}", expected_result);

    assert_eq!(&result, &expected_result);
}

fn assert_err(source: &str, expected_error: Error) {
    let unnamed_ast = parse(source);

    let result = unnamed_ast.evaluate().unwrap_err();

    println!("Evaluated: {:#?}", result);

    assert_eq!(&result, &expected_error);
}

macro_rules! test_eval {
    ($test_name:ident, $source:literal, $result:literal) => {
        #[test]
        fn $test_name() {
            assert_eval($source, $result);
        }
    };
}

macro_rules! test_err {
    ($test_name:ident, $source:literal, $result:expr) => {
        #[test]
        fn $test_name() {
            assert_err($source, $result);
        }
    };
}

test_eval!(simp_app1, r"(\x.x 0)", r"0");
test_eval!(simp_app2, r"(\x.x succ 0)", r"succ 0");
test_eval!(simp_app3, r"(\x. (\y. succ x) 0)", r"\y. succ 0");
test_eval!(simp_app4, r"(\x. (\y. succ x) y)", r"\z. succ y");

test_eval!(simp_if1, r"if true then false else true", r"false");
test_eval!(simp_if2, r"if true then 0 else succ 0", r"0");
test_err!(
    simp_if_err,
    r"if 0 then true else false",
    Error::IfTypeError(parse("0"))
);

test_eval!(simp_iz1, r"iszero 0", r"true");
test_eval!(simp_iz2, r"iszero pred 0", r"true");
test_eval!(simp_iz3, r"iszero succ 0", r"false");

test_eval!(simp_pred1, r"pred succ 0", "0");
test_eval!(simp_pred2, r"pred 0", "0");
test_eval!(simp_pred3, r"pred pred succ 0", "0");
test_eval!(simp_pred4, r"pred succ succ 0", "succ 0");

// test_eval!(clash_app1, r"", r"\y. ");
