use std::{num::ParseIntError, str::FromStr};

use pest::{error::Error, iterators::Pairs, Parser};
use pest_derive::Parser;

use crate::interpreter::ast;

#[derive(Parser)]
#[grammar = "interpreter/grammar.pest"]
struct LangParser;

pub type ParserErr = Error<Rule>;

// A Pair is a pair where one component is the input matched input and the other
// component is the matched rule.

pub fn parse<'a>(src: &'a str) -> Result<ast::Process, ParserErr> {
    let parse_result = LangParser::parse(Rule::process, src);

    match parse_result {
        Ok(mut tokens) => {
            let pair = tokens.next().expect("Expecting a program");
            make_process(pair.into_inner())
        }
        Result::Err(x) => Result::Err(x),
    }
}

fn make_process(mut pairs: Pairs<Rule>) -> Result<ast::Process, ParserErr> {
    let pair = pairs.next().expect("Expecting a process");
    let rule = pair.as_rule();

    match rule {
        Rule::expression => {
            let tokens = pair.into_inner();
            let expr = make_expression(tokens);
            expr.map(|x| ast::Process::Eval(x))
        }
        _ => {
            unreachable!("Unexpected syntax error, expression rule should be matched")
        }
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
        _ => unreachable!("Unexpected syntax error, value rule should be matched"),
    }
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
        Rule::int_literal => {
            let lit_res = make_i32_literal(pairs_clone);
            lit_res.map_or_else(
                |p| {
                    Err(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: p.to_string(),
                        },
                        pair.as_span(),
                    ))
                },
                |i| Ok(i),
            )
        }
        _ => unreachable!("Unexpected syntax error, string literal rule should be matched"), //Result::Err(Err(pair)),
    }
}

fn make_f32_literal(pairs: Pairs<Rule>) -> Result<ast::Value, std::num::ParseFloatError> {
    f32::from_str(pairs.as_str()).map(ast::Value::Float32)
}

fn make_i32_literal(pairs: Pairs<Rule>) -> Result<ast::Value, ParseIntError> {
    let splitted: Vec<_> = pairs.as_str().split_whitespace().collect();
    let joined = splitted.join("");
    let src = joined.as_str();
    i32::from_str_radix(src, 10).map(ast::Value::Int32)
}

// Lifetime elision in Err type
fn make_string_literal(pairs: Pairs<Rule>) -> ast::Value {
    //Result<ast::Value, Err> {
    let str = String::from(pairs.as_str());
    ast::Value::Str(str)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;
    use crate::interpreter::ast;

    #[test]
    fn should_parse_string_lit_correctly() {
        assert_eq!(
            parse("\"\""),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Str(
                String::from_str("").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"hello world!\""),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Str(
                String::from_str("hello world!").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("     \"hello world!\"   "),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Str(
                String::from_str("hello world!").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"À\""),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Str(
                String::from_str("À").expect("Testing expect"),
            ))))
        );

        assert_eq!(
            parse("\"\\n\""),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Str(
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
            parse("+234"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                234
            ))))
        );

        assert_eq!(
            parse("234"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                234
            ))))
        );

        assert_eq!(
            parse("   234     "),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                234
            ))))
        );

        assert_eq!(
            parse("0"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                0
            ))))
        );

        assert_eq!(
            parse("-0"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                0
            ))))
        );

        assert_eq!(
            parse("+0"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                0
            ))))
        );

        assert_eq!(
            parse("-432"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                -432
            ))))
        );

        assert_eq!(
            parse("-                 432"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                -432
            ))))
        )
    }

    #[test]
    fn should_not_parse_i32_lit_correctly() {
        assert!(parse("2147483648").is_err());

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                42
            ))))
        );

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                -7
            ))))
        );

        assert_ne!(
            parse("42-7"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Int32(
                7
            ))))
        )
    }
}
