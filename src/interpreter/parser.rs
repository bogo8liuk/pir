use std::{num::ParseIntError, str::FromStr};

use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Parser, Span,
};
use pest_derive::Parser;

use crate::interpreter::ast::{self, IntExpr, Process};

#[derive(Parser)]
#[grammar = "interpreter/grammar.pest"]
struct LangParser;

pub type ParserErr = Error<Rule>;

enum CharParseError {
    // There are no characters to parse
    Empty,
    // Characters are more than one.
    // NB: it's up to the client to build a string with a size of at least 2.
    MultipleChars { chars: String },
}

impl CharParseError {}

// A Pair is a pair where one component is the input matched input and the other
// component is the matched rule.

lazy_static::lazy_static! {
    static ref NUM_EXPR_PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left) | Op::infix(div, Left) | Op::infix(r#mod, Left))
            .op(Op::prefix(neg))
    };

    static ref PROC_PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(proc_or, Right))
    };
}

pub fn parse<'a>(src: &'a str) -> Result<ast::Process, ParserErr> {
    let parse_result = LangParser::parse(Rule::program, src);
    match parse_result {
        Ok(mut pairs) => {
            let sub_pairs = pairs.next().expect("Expecting a process").into_inner();
            make_process(sub_pairs)
        }
        Err(e) => Err(e),
    }
}

fn make_process(pairs: Pairs<Rule>) -> Result<ast::Process, Error<Rule>> {
    PROC_PRATT_PARSER
        .map_primary(|pair| {
            match pair.as_rule() {
                Rule::expression => {
                    let tokens = pair.into_inner();
                    let expr = make_expression(tokens);
                    expr.map(|x| ast::Process::Expr(x))
                }
                Rule::r#loop => {
                    let tokens = pair
                        .into_inner()
                        .next()
                        .expect("Expecting a process")
                        .into_inner();
                    let sub_proc = make_process(tokens);
                    sub_proc.map(|p| ast::Process::Loop(Box::new(p)))
                }
                Rule::channel_declaration => {
                    let tokens = pair.into_inner();
                    let (chan_id, mut updated_tokens) = make_channel_id(tokens);
                    // Skipping process `then` operator
                    //updated_tokens
                    //    .next()
                    //    .expect("Expecting process sequence operator");

                    match make_process(
                        updated_tokens
                            .next()
                            .expect("Expecting a process")
                            .into_inner(),
                    ) {
                        Ok(proc) => Ok(ast::Process::ChanDeclaration(chan_id, Box::new(proc))),
                        Err(e) => Err(e),
                    }
                }
                rule => unreachable!("Unexpected rule {:?}, expecting one of process", rule),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::proc_or => Process::Par,
                rule => unreachable!("Expecting a process operator, found {:?}", rule),
            };

            match (lhs, rhs) {
                (Ok(proc1), Ok(proc2)) => Ok(op(Box::new(proc1), Box::new(proc2))),
                _ => todo!("Is this reachable?"),
            }
        })
        .parse(pairs)
}

/*
fn make_process(mut pairs: Pairs<Rule>) -> Result<ast::Process, ParserErr> {
    let pair = pairs.next().expect("Expecting a type of process");
    let rule = pair.as_rule();

    match rule {
        Rule::expression => {
            let tokens = pair.into_inner();
            let expr = make_expression(tokens);
            expr.map(|x| ast::Process::Expr(x))
        }
        Rule::r#loop => {
            let tokens = pair
                .into_inner()
                .next()
                .expect("Expecting a process")
                .into_inner();
            let sub_proc = make_process(tokens);
            sub_proc.map(|p| ast::Process::Loop(Box::new(p)))
        }
        Rule::channel_declaration => {
            let tokens = pair.into_inner();
            let (chan_id, mut updated_tokens) = make_channel_id(tokens);
            // Skipping process `and` operator
            updated_tokens
                .next()
                .expect("Expecting process sequence operator");

            match make_process(
                updated_tokens
                    .next()
                    .expect("Expecting a process")
                    .into_inner(),
            ) {
                Ok(proc) => Ok(ast::Process::ChanDeclaration(chan_id, Box::new(proc))),
                Err(e) => Err(e),
            }
        }
        _ => {
            unreachable!(
                "Unreachable code: expecting a sub-rule of process, found {:?}",
                rule
            )
        }
    }
}
*/

// This function gives back ownership of pairs
fn make_channel_id(mut pairs: Pairs<Rule>) -> (String, Pairs<Rule>) {
    let pair = pairs.next().expect("Expecting a channel declaration");
    let rule = pair.as_rule();

    match rule {
        Rule::var_identifier => (pair.as_str().into(), pairs),
        _ => unreachable!("Expecting a channel id, found rule {:?}", rule),
    }
}

fn make_expression(mut pairs: Pairs<Rule>) -> Result<ast::Expression, ParserErr> {
    let pair = pairs.next().expect("Expecting an expression");
    let rule = pair.as_rule();

    match rule {
        Rule::value => {
            let tokens = pair.into_inner();
            let val = make_value(tokens);
            val.map(|x| ast::Expression::Val(x))
        }
        Rule::int_expr => {
            let sub_pairs = pair.into_inner();
            make_int_expr(sub_pairs).map(|int_expr| int_expr.into())
        }
        r => unreachable!(
            "Unexpected syntax error, value rule should be matched, rule {:?} found instead",
            r,
        ),
    }
}

fn make_int_expr(pairs: Pairs<Rule>) -> Result<ast::IntExpr, ParserErr> {
    NUM_EXPR_PRATT_PARSER
        .map_primary(|pair| match pair.as_rule() {
            Rule::int_expr => make_int_expr(pair.into_inner()),
            Rule::int_literal => {
                let pair_clone = pair.clone();
                make_raw_i32_literal(pair.into_inner())
                    .map(IntExpr::Lit)
                    .map_err(|err| stringable_error_to_parser_error(err, pair_clone))
            }
            rule => unreachable!(
                "Unexpected rule {:?}, expecting one of int expression",
                rule
            ),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => IntExpr::Add,
                Rule::sub => IntExpr::Sub,
                Rule::mul => IntExpr::Mul,
                Rule::div => IntExpr::Div,
                Rule::r#mod => IntExpr::Mod,
                r => unreachable!("Unexpected rule {:?}, expecting an infix operator", r),
            };
            match (lhs, rhs) {
                (Ok(e1), Ok(e2)) => Ok(op(Box::new(e1), Box::new(e2)).into()),
                _ => todo!("Check if this point can be reached when there are errors"),
            }
        })
        .map_prefix(|op, rhs| match rhs {
            Ok(int_expr) => match op.as_rule() {
                Rule::neg => Ok(IntExpr::Neg(Box::new(int_expr)).into()),
                r => unreachable!("Unreachable code: expecting neg rule, found {:?}", r),
            },
            _ => todo!("return reachable error, since we can write anything we want with ops"),
        })
        .parse(pairs)
}

fn make_value(mut pairs: Pairs<Rule>) -> Result<ast::Value, ParserErr> {
    let pair = pairs.next().expect("Expecting a value");
    let rule = pair.as_rule();

    match rule {
        Rule::literal => {
            let tokens = pair.into_inner();
            make_literal(tokens)
        }
        _ => unreachable!("Unexpected syntax error, literal rule should be matched"),
    }
}

fn make_literal(mut pairs: Pairs<Rule>) -> Result<ast::Value, ParserErr> {
    let pairs_clone = pairs.clone();
    let pair = pairs.next().expect("Expecting a literal");
    let rule = pair.as_rule();

    match rule {
        Rule::string_literal => {
            let tokens = pair.into_inner();
            Ok(make_string_literal(tokens))
        }
        Rule::char_literal => {
            let pair_clone = pair.clone();
            let tokens = pair.into_inner();
            make_char_literal(tokens).map_err(|e| match e {
                CharParseError::Empty => custom_error_from_span(
                    String::from("No character to parse"),
                    pair_clone.as_span(),
                ),
                CharParseError::MultipleChars { chars: _ } => custom_error_from_span(
                    String::from("Characters to parse are more than one"),
                    pair_clone.as_span(),
                ),
            })
        }
        Rule::int_literal => {
            let lit_res = make_i32_literal(pairs_clone);
            lit_res.map_or_else(
                |p| Err(stringable_error_to_parser_error(p, pair)),
                |i| Ok(i),
            )
        }
        Rule::float_literal => {
            let lit_res = make_f32_literal(pairs_clone);
            lit_res.map_or_else(
                |p| Err(stringable_error_to_parser_error(p, pair)),
                |fl| Ok(fl),
            )
        }
        _ => unreachable!("Unexpected syntax error, string literal rule should be matched"), //Result::Err(Err(pair)),
    }
}

fn custom_error_from_span(msg: String, span: Span) -> ParserErr {
    pest::error::Error::new_from_span(
        pest::error::ErrorVariant::CustomError { message: msg },
        span,
    )
}

fn make_f32_literal(pairs: Pairs<Rule>) -> Result<ast::Value, std::num::ParseFloatError> {
    let owned = remove_whitespaces(pairs);
    let src = owned.as_str();
    f32::from_str(src).map(ast::Value::Float32)
}

fn make_i32_literal(pairs: Pairs<Rule>) -> Result<ast::Value, ParseIntError> {
    make_raw_i32_literal(pairs).map(ast::Value::Int32)
}

fn make_raw_i32_literal(pairs: Pairs<Rule>) -> Result<i32, ParseIntError> {
    let owned = remove_whitespaces(pairs);
    let src = owned.as_str();
    i32::from_str_radix(src, 10)
}

fn remove_whitespaces(pairs: Pairs<Rule>) -> String {
    let splitted: Vec<_> = pairs.as_str().split_whitespace().collect();
    let joined = splitted.join("");
    joined.as_str().to_owned()
}

fn make_string_literal(pairs: Pairs<Rule>) -> ast::Value {
    // Lifetime elision in Err type
    //Result<ast::Value, Err> {
    let str = String::from(pairs.as_str());
    ast::Value::Str(str)
}

fn make_char_literal(pairs: Pairs<Rule>) -> Result<ast::Value, CharParseError> {
    let str = String::from(pairs.as_str());
    let mut chars = str.chars();
    /* Actually, the check for other characters is useless, because it should
    be already performed by the parser. Thus, that check is not done. */
    match chars.next() {
        // Performing look-ahead after escaping character \
        Some('\\') => match chars.next() {
            Some('\\') => Ok(ast::Value::Char('\\'.into())),
            Some('\'') => Ok(ast::Value::Char('\''.into())),
            Some('n') => Ok(ast::Value::Char('\n'.into())),
            Some('t') => Ok(ast::Value::Char('\t'.into())),
            Some('r') => Ok(ast::Value::Char('\r'.into())),
            Some('b') => Ok(ast::Value::Char('\x08'.into())),
            /* This is the character \ followed by none of the above (\, n, t, etc.).
            It should be already avoided by the grammar. */
            Some(c) => unreachable!(
                "Unreachable code: illegal character <{}> after escaping character \\ in character literal",
                c
            ),
            /* This is the following case: '\'. The grammar should avoid it. */
            None => unreachable!("Unreachable code: no characters after escaping character \\ in character literal"),
        },
        Some(char) => Ok(ast::Value::Char(char.into())),
        /* This case should be unreachable, since the parser should perform the
        check before. */
        None => Err(CharParseError::Empty),
    }
}

fn escape(str: &str) -> &str {
    match str {
        "\\\\" => "\\",
        "\\\"" => "\"",
        "\\'" => "'",
        "\\n" => "\n",
        "\\t" => "\t",
        "\\r" => "\r",
        "\\b" => "\x08",
        _ => unreachable!("Unexpected escaped character: {}", str),
    }
}

fn stringable_error_to_parser_error<T>(err: T, pair: Pair<Rule>) -> ParserErr
where
    T: ToString,
{
    pest::error::Error::new_from_span(
        pest::error::ErrorVariant::CustomError {
            message: err.to_string(),
        },
        pair.as_span(),
    )
}

#[cfg(test)]
mod tests {
    use std::{f32::INFINITY, str::FromStr};

    use super::*;
    use crate::interpreter::ast;

    #[test]
    fn should_parse_char_lit_correctly() {
        assert_eq!(
            parse("'c'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'c' as u32,
            ))))
        );

        assert_eq!(
            parse("'0'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '0' as u32,
            ))))
        );

        assert_eq!(
            parse("'Z'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'Z' as u32,
            ))))
        );

        assert_eq!(
            parse("' '"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                ' ' as u32,
            ))))
        );

        assert_eq!(
            parse("'$'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '$' as u32,
            ))))
        );

        assert_eq!(
            parse("'Γ'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'Γ' as u32,
            ))))
        );

        assert_eq!(
            parse("'·'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '·' as u32,
            ))))
        );

        assert_eq!(
            parse("'é'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'é' as u32,
            ))))
        );

        assert_eq!(
            parse("'ट'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'ट' as u32,
            ))))
        );

        assert_eq!(
            parse("'あ'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'あ' as u32,
            ))))
        );

        assert_eq!(
            parse("'€'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '€' as u32,
            ))))
        );

        assert_eq!(
            parse("'漢'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '漢' as u32,
            ))))
        );

        assert_eq!(
            parse("'Д'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'Д' as u32,
            ))))
        );

        assert_eq!(
            parse("'\\n'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '\n' as u32,
            ))))
        );

        assert_eq!(
            parse("'\\t'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                '\t' as u32,
            ))))
        );

        assert_eq!(
            parse("'Ճ'"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Char(
                'Ճ' as u32,
            ))))
        );
    }

    #[test]
    fn should_not_parse_char_lit_correctly() {
        assert!(parse("''").is_err());

        // This is expected to be an error because it has an accent
        assert!(parse("'Д́'").is_err());

        assert!(parse("'W '").is_err());

        assert!(parse("' ~'").is_err());

        assert!(parse("' ¬ '").is_err());

        assert!(parse("'j    '").is_err());

        assert!(parse("'f").is_err());

        assert!(parse("9'").is_err());

        assert!(parse("'c@'").is_err());

        assert!(parse("'42!!!!!!!!!!'").is_err());

        assert!(parse("'lorem ipsum°'").is_err());

        assert!(parse("'字%'").is_err());

        assert!(parse("'").is_err());

        assert!(parse("汉'").is_err());

        assert!(parse("'Ճ").is_err());
    }

    #[test]
    fn should_parse_string_lit_correctly() {
        assert_eq!(
            parse("\"\""),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Str(
                String::from_str("").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"hello world!\""),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Str(
                String::from_str("hello world!").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("     \"hello world!\"   "),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Str(
                String::from_str("hello world!").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"À\""),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Str(
                String::from_str("À").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"\\n\""),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Str(
                String::from_str("\\n").expect("Testing expect"),
            ))))
        );
    }

    #[test]
    fn should_be_malformed_stringlit() {
        assert!(parse("ok\"").is_err());

        assert!(parse("\"ok").is_err());

        assert!(parse("\"hello world!\"\"").is_err());
    }

    #[test]
    fn should_parse_i32_lit_correctly() {
        assert_eq!(
            parse("234"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(
                234
            ))))
        );

        assert_eq!(
            parse("   234     "),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(
                234
            ))))
        );

        assert_eq!(
            parse("0"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(
                0
            ))))
        );

        assert_eq!(
            parse("-0"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Neg(
                Box::new(IntExpr::Lit(0))
            ))))
        );

        assert_ne!(
            parse("+0"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(
                0
            ))))
        );

        assert_eq!(
            parse("-432"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Neg(
                Box::new(IntExpr::Lit(432))
            ))))
        );

        assert_eq!(
            parse("-                 432"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Neg(
                Box::new(IntExpr::Lit(432))
            ))))
        )
    }

    #[test]
    fn should_not_parse_i32_lit_correctly() {
        assert!(parse("2147483648").is_err());

        assert!(parse("+234").is_err());

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Int32(
                42
            ))))
        );

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Int32(
                -7
            ))))
        );

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Expr(ast::Expression::Val(ast::Value::Int32(
                7
            ))))
        );

        assert!(parse("4 2").is_err());
    }

    #[test]
    fn should_parse_float_literal_correctly() {
        assert_eq!(
            parse("123.456"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(123.456)
            )))
        );

        assert_eq!(
            parse("   78.038           "),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(78.038)
            )))
        );

        assert_eq!(
            parse("0.450099"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(0.450099)
            )))
        );

        assert_eq!(
            parse("-0.4"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-0.4)
            )))
        );

        assert_eq!(
            parse("   -     7.05   "),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-7.05)
            )))
        );

        assert_eq!(
            parse("-2005.0002301"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-2005.0002301)
            )))
        );

        assert_eq!(
            parse("02.7"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(2.7),
            )))
        );

        assert_ne!(
            parse("0.0001"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-2005.0002301)
            )))
        );

        println!(
            ">>>>>>>>>>>>>>>>>>>> {:?}",
            parse("9324920392048932084210489058709438490850932490932850.2112").unwrap()
        );
        assert_eq!(
            parse("9324920392048932084210489058709438490850932490932850.2112"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(INFINITY)
            )))
        );
    }

    #[test]
    fn try_parse_with_whitespace() {
        let res1 = i32::from_str_radix("  -   42", 10);

        if res1.is_err() {
            println!(
                "whitespace parse result of \"  -   42\" is: {}",
                res1.as_ref().unwrap_err()
            )
        }

        let res2 = i32::from_str_radix("  -   4  2 ", 10);

        if res2.is_err() {
            println!(
                "whitespace parse result of \"  -   4  2 \" is: {}",
                res2.as_ref().unwrap_err()
            )
        }

        let res3 = i32::from_str_radix("-42", 10);

        if res3.is_err() {
            println!(
                "whitespace parse result of \"-42\" is: {}",
                res3.as_ref().unwrap_err()
            )
        }
    }

    #[test]
    fn should_not_parse_f32_literal() {
        assert!(parse("0.").is_err());
        assert!(parse("901.").is_err());
        assert!(parse(".948").is_err());

        // If new syntax will exist, just remove tests asserting errors,
        // old ones remain right!
        assert_ne!(
            parse("7..4"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(7.4)
            )))
        );
        assert!(parse("7..4").is_err());

        assert_ne!(
            parse("(-8).2"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-8.2)
            )))
        );
        assert!(parse("(-8).2").is_err());

        assert_ne!(
            parse("7   .948"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(7.948)
            )))
        );
        assert!(parse("7   .948").is_err());

        assert_ne!(
            parse("-8   .606"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-8.606)
            )))
        );
        assert!(parse("-8   .606").is_err());

        assert_ne!(
            parse("-1776   .  6906"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(-1776.6906)
            )))
        );
        assert!(parse("-1776   .  6906").is_err());

        assert_ne!(
            parse("6.8.7"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(68.7)
            )))
        );
        assert_ne!(
            parse("6.8.7"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(6.87)
            )))
        );
        assert!(parse("6.8.7").is_err());

        assert_ne!(
            parse("5.  1"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(5.1)
            )))
        );
        assert!(parse("5.  1").is_err());

        assert_ne!(
            parse("5 .01"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(5.01)
            )))
        );
        assert!(parse("5 .01").is_err());

        assert_ne!(
            parse("0 .  111"),
            Ok(ast::Process::Expr(ast::Expression::Val(
                ast::Value::Float32(0.111)
            )))
        );
        assert!(parse("0 .  111").is_err());
    }

    #[test]
    fn should_parse_int_expr() {
        assert_eq!(
            parse("1+2"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Add(
                Box::new(IntExpr::Lit(1)),
                Box::new(IntExpr::Lit(2))
            ))))
        );

        assert_eq!(
            parse("(3-4)"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Sub(
                Box::new(IntExpr::Lit(3)),
                Box::new(IntExpr::Lit(4))
            ))))
        );

        assert_eq!(
            parse("  -  10 +    5"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Add(
                Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(10)))),
                Box::new(IntExpr::Lit(5))
            ))))
        );

        assert_eq!(
            parse("9  *-32"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Mul(
                Box::new(IntExpr::Lit(9)),
                Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(32))))
            ))))
        );

        assert_eq!(
            parse("   4 - 0 - 7"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Sub(
                Box::new(IntExpr::Sub(
                    Box::new(IntExpr::Lit(4)),
                    Box::new(IntExpr::Lit(0))
                )),
                Box::new(IntExpr::Lit(7))
            ))))
        );

        assert_eq!(parse("   4 - 0 - 7"), parse("(4-0)-7"));

        assert_eq!(
            parse(" 8 %7   "),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Mod(
                Box::new(IntExpr::Lit(8)),
                Box::new(IntExpr::Lit(7))
            ))))
        );

        assert_eq!(
            parse("-88+ 8 %7   "),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Add(
                Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(88)))),
                Box::new(IntExpr::Mod(
                    Box::new(IntExpr::Lit(8)),
                    Box::new(IntExpr::Lit(7))
                ))
            ))))
        );

        assert_eq!(
            parse("-88*- 8 %7"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Mod(
                Box::new(IntExpr::Mul(
                    Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(88)))),
                    Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(8)))),
                )),
                Box::new(IntExpr::Lit(7))
            ))))
        );

        assert_eq!(
            parse("403 / 89 --1/0"),
            Ok(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Sub(
                Box::new(IntExpr::Div(
                    Box::new(IntExpr::Lit(403)),
                    Box::new(IntExpr::Lit(89)),
                )),
                Box::new(IntExpr::Div(
                    Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(1)))),
                    Box::new(IntExpr::Lit(0)),
                )),
            ))))
        );
    }

    #[test]
    fn should_parse_new_chan() {
        assert_eq!(
            parse("chan x. 17 "),
            Ok(ast::Process::ChanDeclaration(
                "x".into(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(
                    17
                ))))
            ))
        );

        assert!(parse("chanx. 0").is_err());

        assert_eq!(
            parse("  chan  foo  . loop (0)"),
            Ok(ast::Process::ChanDeclaration(
                "foo".to_owned(),
                Box::new(ast::Process::Loop(Box::new(ast::Process::Expr(
                    ast::Expression::IntExpr(ast::IntExpr::Lit(0))
                ))))
            ))
        );

        assert!(parse("chan x 17 ").is_err());
        //assert!(parse("chan x( 17 ").is_err());
        //assert!(parse("chan x 42 )").is_err());

        assert_eq!(
            parse("chan _ .9"),
            Ok(ast::Process::ChanDeclaration(
                "_".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Lit(9)
                )))
            ))
        );

        assert_eq!(
            parse("chan _F .-42"),
            Ok(ast::Process::ChanDeclaration(
                "_F".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Neg(Box::new(IntExpr::Lit(42)))
                )))
            ))
        );

        assert_eq!(
            parse("chan b3 .-42"),
            Ok(ast::Process::ChanDeclaration(
                "b3".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Neg(Box::new(IntExpr::Lit(42)))
                )))
            ))
        );

        assert_eq!(
            parse("chan b3/* commenting */.-42"),
            Ok(ast::Process::ChanDeclaration(
                "b3".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Neg(Box::new(IntExpr::Lit(42)))
                )))
            ))
        );

        assert_eq!(
            parse("chan chanx .-42"),
            Ok(ast::Process::ChanDeclaration(
                "chanx".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Neg(Box::new(IntExpr::Lit(42)))
                )))
            ))
        );

        assert_eq!(
            parse("chan looppy .-42"),
            Ok(ast::Process::ChanDeclaration(
                "looppy".to_owned(),
                Box::new(ast::Process::Expr(ast::Expression::IntExpr(
                    ast::IntExpr::Neg(Box::new(IntExpr::Lit(42)))
                )))
            ))
        );

        assert!(parse("chan 6 . 7").is_err());
        assert!(parse("chan 6a . 7").is_err());
        assert!(parse("chan A . 7").is_err());
        assert!(parse("chan Upp . 7").is_err());
        assert!(parse("chan X_0 . 7").is_err());
        assert!(parse("chan chan . 7").is_err());
        assert!(parse("chan loop . 7").is_err());
        assert!(parse("chan chan c . 7").is_err());
        assert!(parse("chan loop l . 7").is_err());
        assert!(parse("chan . . 7").is_err());
        assert!(parse("chan ._ . 7").is_err());
        assert!(parse("chan , . 7").is_err());
        assert!(parse("chan ; . 7").is_err());
        assert!(parse("chan : . 7").is_err());
        assert!(parse("chan :aaa . 7").is_err());
        assert!(parse("chan @ . 7").is_err());
        assert!(parse("chan # . 7").is_err());
        assert!(parse("chan ? . 7").is_err());
        assert!(parse("chan ! . 7").is_err());
        assert!(parse("chan !x . 7").is_err());
        assert!(parse("chan $ . 7").is_err());
        assert!(parse("chan ( . 7").is_err());
        assert!(parse("chan () . 7").is_err());
        assert!(parse("chan ~ . 7").is_err());
        assert!(parse("chan ^ . 7").is_err());
        assert!(parse("chan  . 7").is_err());
        assert!(parse("chan + . 7").is_err());
        assert!(parse("chan - . 7").is_err());
        assert!(parse("chan -e . 7").is_err());
        assert!(parse("chan -8 . 7").is_err());
        assert!(parse("chan * . 7").is_err());
        assert!(parse("chan / . 7").is_err());
        assert!(parse("chan % . 7").is_err());
        assert!(parse("chan < . 7").is_err());
        assert!(parse("chan <p . 7").is_err());
        assert!(parse("chan > . 7").is_err());
        assert!(parse("chan = . 7").is_err());
        assert!(parse("chan =c . 7").is_err());
        assert!(parse("chan c c . 7").is_err());
        assert!(parse("chan _ c . 7").is_err());
        assert!(parse("chan i 777 . 7").is_err());
    }

    #[test]
    fn should_parse_parallel_op() {
        assert_eq!(
            parse("loop (17 + 9) | chan x. chan y . loop (1-1) "),
            Ok(ast::Process::Par(
                Box::new(ast::Process::Loop(Box::new(ast::Process::Expr(
                    ast::Expression::IntExpr(ast::IntExpr::Add(
                        Box::new(ast::IntExpr::Lit(17)),
                        Box::new(ast::IntExpr::Lit(9))
                    ))
                )))),
                Box::new(ast::Process::ChanDeclaration(
                    "x".to_owned(),
                    Box::new(ast::Process::ChanDeclaration(
                        "y".to_owned(),
                        Box::new(ast::Process::Loop(Box::new(ast::Process::Expr(
                            ast::Expression::IntExpr(ast::IntExpr::Sub(
                                Box::new(ast::IntExpr::Lit(1)),
                                Box::new(ast::IntExpr::Lit(1))
                            ))
                        ))))
                    ))
                ))
            ))
        );

        assert_eq!(
            parse("loop (7 | chan x .0)"),
            Ok(Process::Loop(Box::new(Process::Par(
                Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(7)))),
                Box::new(Process::ChanDeclaration(
                    "x".to_owned(),
                    Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(0))))
                ))
            ))))
        );

        assert_eq!(
            parse("-6|/**/9"),
            Ok(Process::Par(
                Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Neg(
                    Box::new(IntExpr::Lit(6))
                )))),
                Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(9))))
            ))
        );

        assert_eq!(
            parse("chan c . 6 | 7"),
            Ok(Process::ChanDeclaration(
                "c".to_owned(),
                Box::new(Process::Par(
                    Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(6)))),
                    Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Lit(7))))
                ))
            ))
        );

        assert_eq!(
            parse("chan c . - 6 /8 | 2 -3"),
            Ok(Process::ChanDeclaration(
                "c".to_owned(),
                Box::new(Process::Par(
                    Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Div(
                        Box::new(IntExpr::Neg(Box::new(IntExpr::Lit(6)))),
                        Box::new(IntExpr::Lit(8))
                    )))),
                    Box::new(Process::Expr(ast::Expression::IntExpr(IntExpr::Sub(
                        Box::new(IntExpr::Lit(2)),
                        Box::new(IntExpr::Lit(3))
                    ))))
                ))
            ))
        );

        assert!(parse("  | 9").is_err());
        assert!(parse("999 - 0 |").is_err());
        assert!(parse("999 +| 7").is_err());
        assert!(parse("chan dd .8| ").is_err());
    }
}
