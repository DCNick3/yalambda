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

test_eval!(simp_app1, r"(\x:Nat.x 0)", r"0");
test_eval!(simp_app2, r"(\x:Nat.x succ 0)", r"succ 0");
test_eval!(
    simp_app3,
    r"(\x:Nat. (\y:Nat. succ x) 0)",
    r"\y:Nat. succ 0"
);
// can't typecheck because of the unbound y
// test_eval!(
//     simp_app4,
//     r"(\x:Nat. (\y:Nat. succ x) y)",
//     r"\z:Nat. succ y"
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
    r"\x:Bool.if x then 0 else 0",
    r"\x:Bool.if x then 0 else 0"
);

test_eval!(
    simp_cmplx1,
    r"if iszero (\x:Bool. if x then succ 0 else 0 false) then true else false",
    r"true"
);

test_eval!(
    weird,
    r"((\x:(Nat->Nat)->(Nat->Nat).x \x:Nat->Nat.x) \x:Nat.x)",
    r"\x:Nat.x"
);

// test_eval!(clash_app1, r"", r"\y. ");

test_eval!(simp_let, r"let x = 1 in x", r"1");
test_eval!(mult_let, r"let x = 1, y = 2 in {x:x, y:y}", r"{x:1,y:2}");
test_eval!(
    rec_let,
    r"let {x, y:y} = {x:1, y:2} in {x:y, y:x}",
    r"{y:1,x:2}"
);

test_eval!(
    let_nested,
    r"
let a = 1 in
let b = 2 in
    {a, b}
",
    r"{a:1,b:2}"
);

test_eval!(
    let_escape,
    r"let x = \t:Nat.1 in let y = \t:Nat.(x t) in (y 0)",
    r"1"
);

test_eval!(
    some_fun,
    r"
let
    pair   = \x:Nat. \y:Nat. {_1:x, _2:y},
    first  = \p:{_1:Nat,_2:Nat}. let {_1, _2} = p in _1,
    second = \p:{_1:Nat,_2:Nat}. let {_1:x, _2:y} = p in y,
in let
    flip   = \p:{_1:Nat,_2:Nat}. ((pair (second p)) (first p)),
in
    (flip ((pair 1) 2))
",
    r"{_1:2, _2:1}"
);
