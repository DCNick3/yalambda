use crate::ast::{Expr, RuntimeError, UnnamedExpr};
use std::sync::Arc;

fn parse(source: &str) -> Arc<UnnamedExpr> {
    let ast: Arc<Expr> = crate::yalamb::ExprParser::new().parse(source).unwrap();

    println!("Ast: {:#?}", ast);

    println!("Type: {:#?}", ast.type_of().unwrap());

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

#[allow(unused)]
fn assert_err(source: &str, expected_error: RuntimeError) {
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

// macro_rules! test_err {
//     ($test_name:ident, $source:literal, $result:expr) => {
//         #[test]
//         fn $test_name() {
//             assert_err($source, $result);
//         }
//     };
// }

test_eval!(simp_app1, r"(\x:nat.x 0)", r"0");
test_eval!(simp_app2, r"(\x:nat.x succ 0)", r"succ 0");
test_eval!(
    simp_app3,
    r"(\x:nat. (\y:nat. succ x) 0)",
    r"\y:nat. succ 0"
);
// can't typecheck because of the unbound y
// test_eval!(
//     simp_app4,
//     r"(\x:nat. (\y:nat. succ x) y)",
//     r"\z:nat. succ y"
// );

test_eval!(simp_if1, r"if true then false else true", r"false");
test_eval!(simp_if2, r"if true then 0 else succ 0", r"0");

// can't typecheck because, well, those are errornous
// test_err!(
//     simp_if_err1,
//     r"if 0 then true else false",
//     RuntimeError::IfTypeError(parse(r"0"))
// );
// test_err!(
//     simp_if_err2,
//     r"if \x.x then true else false",
//     RuntimeError::IfTypeError(parse(r"\x.x"))
// );

test_eval!(simp_iz1, r"iszero 0", r"true");
test_eval!(simp_iz2, r"iszero pred 0", r"true");
test_eval!(simp_iz3, r"iszero succ 0", r"false");

test_eval!(simp_pred1, r"pred succ 0", "0");
test_eval!(simp_pred2, r"pred 0", "0");
test_eval!(simp_pred3, r"pred pred succ 0", "0");
test_eval!(simp_pred4, r"pred succ succ 0", "succ 0");

test_eval!(
    irreducible1,
    r"\x:bool.if x then 0 else 0",
    r"\x:bool.if x then 0 else 0"
);

test_eval!(
    simp_cmplx1,
    r"if iszero (\x:bool. if x then succ 0 else 0 false) then true else false",
    r"true"
);

test_eval!(
    weird,
    r"((\x:(nat->nat)->(nat->nat).x \x:nat->nat.x) \x:nat.x)",
    r"\x:nat.x"
);

// test_eval!(clash_app1, r"", r"\y. ");
