use core::num;
use std::{
    f32::INFINITY,
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

use pest::{error::Error, iterators::Pairs, Parser, Span};
use pest_derive::Parser;

use crate::interpreter::ast;

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
        Rule::float_literal => {
            let lit_res = make_f32_literal(pairs_clone);
            lit_res.map_or_else(
                |p| {
                    Err(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: p.to_string(),
                        },
                        pair.as_span(),
                    ))
                },
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
    let owned = remove_whitespaces(pairs);
    let src = owned.as_str();
    i32::from_str_radix(src, 10).map(ast::Value::Int32)
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
    /* Actually, the check for other characters is useless, because it should
    be already performed by the parser. Thus, that check is not done. */
    match str.chars().next() {
        Some(char) => Ok(ast::Value::Char(char)),
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

#[cfg(test)]
mod tests {
    use core::panic;
    use std::str::FromStr;

    use pest::{iterators::Pair, Parser};

    use super::*;
    use crate::interpreter::ast;

    #[test]
    fn should_parse_char_lit_correctly() {
        assert_eq!(
            parse("'c'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'c',
            ))))
        );

        assert_eq!(
            parse("'0'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '0',
            ))))
        );

        assert_eq!(
            parse("'Z'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'Z',
            ))))
        );

        assert_eq!(
            parse("' '"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                ' ',
            ))))
        );

        assert_eq!(
            parse("'$'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '$',
            ))))
        );

        assert_eq!(
            parse("'Γ'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'Γ',
            ))))
        );

        assert_eq!(
            parse("'·'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '·',
            ))))
        );

        assert_eq!(
            parse("'é'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'é',
            ))))
        );

        assert_eq!(
            parse("'ट'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'ट',
            ))))
        );

        assert_eq!(
            parse("'あ'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'あ',
            ))))
        );

        assert_eq!(
            parse("'€'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '€',
            ))))
        );

        assert_eq!(
            parse("'漢'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '漢',
            ))))
        );

        assert_eq!(
            parse("'Д'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'Д',
            ))))
        );

        assert_eq!(
            parse("'\\n'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '\n',
            ))))
        );

        assert_eq!(
            parse("'\\t'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                '\t',
            ))))
        );

        assert_eq!(
            parse("'Ճ'"),
            Ok(ast::Process::Eval(ast::Expression::Val(ast::Value::Char(
                'Ճ',
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
        );

        assert!(parse("4 2").is_err());
    }

    #[test]
    fn should_parse_float_literal_correctly() {
        assert_eq!(
            parse("123.456"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(123.456)
            )))
        );

        assert_eq!(
            parse("   78.038           "),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(78.038)
            )))
        );

        assert_eq!(
            parse("0.450099"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(0.450099)
            )))
        );

        assert_eq!(
            parse("-0.4"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-0.4)
            )))
        );

        assert_eq!(
            parse("   -     7.05   "),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-7.05)
            )))
        );

        assert_eq!(
            parse("-2005.0002301"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-2005.0002301)
            )))
        );

        assert_eq!(
            parse("02.7"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(2.7),
            )))
        );

        assert_ne!(
            parse("0.0001"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-2005.0002301)
            )))
        );

        println!(
            ">>>>>>>>>>>>>>>>>>>> {:?}",
            parse("9324920392048932084210489058709438490850932490932850.2112").unwrap()
        );
        assert_eq!(
            parse("9324920392048932084210489058709438490850932490932850.2112"),
            Ok(ast::Process::Eval(ast::Expression::Val(
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
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(7.4)
            )))
        );
        assert!(parse("7..4").is_err());

        assert_ne!(
            parse("(-8).2"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-8.2)
            )))
        );
        assert!(parse("(-8).2").is_err());

        assert_ne!(
            parse("7   .948"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(7.948)
            )))
        );
        assert!(parse("7   .948").is_err());

        assert_ne!(
            parse("-8   .606"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-8.606)
            )))
        );
        assert!(parse("-8   .606").is_err());

        assert_ne!(
            parse("-1776   .  6906"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(-1776.6906)
            )))
        );
        assert!(parse("-1776   .  6906").is_err());

        assert_ne!(
            parse("6.8.7"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(68.7)
            )))
        );
        assert_ne!(
            parse("6.8.7"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(6.87)
            )))
        );
        assert!(parse("6.8.7").is_err());

        assert_ne!(
            parse("5.  1"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(5.1)
            )))
        );
        assert!(parse("5.  1").is_err());

        assert_ne!(
            parse("5 .01"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(5.01)
            )))
        );
        assert!(parse("5 .01").is_err());

        assert_ne!(
            parse("0 .  111"),
            Ok(ast::Process::Eval(ast::Expression::Val(
                ast::Value::Float32(0.111)
            )))
        );
        assert!(parse("0 .  111").is_err());
    }
}
