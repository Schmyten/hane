use pest::Parser;
use pest_derive::Parser;
use crate::{Expr, ExprVariant, Span, SpanError, Command, CommandVariant};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct HaneParser;

type Pair<'i> = pest::iterators::Pair<'i, Rule>;

//pub type ParseError = pest::error::Error<Rule>;
type ParseError = SpanError<pest::error::ErrorVariant<Rule>>;

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(err: pest::error::Error<Rule>) -> Self {
        use crate::Location;
        let span = match (err.location, err.line_col) {
            (pest::error::InputLocation::Pos(pos), pest::error::LineColLocation::Pos((line, col))) =>
                Span { start: Location { pos, line, col }, end: Location { pos, line, col } },
            (pest::error::InputLocation::Span((start_pos, end_pos)), pest::error::LineColLocation::Span((start_line, start_col), (end_line, end_col))) =>
                Span { start: Location { pos: start_pos, line: start_line, col: start_col }, end: Location { pos: end_pos, line: end_line, col: end_col }},
            _ => unreachable!(),
        };
        SpanError {
            span,
            err: err.variant
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<Command>, ParseError> {
    let pairs = HaneParser::parse(Rule::commands, input)?;
    Ok(pairs.take_while(|pair|pair.as_rule() != Rule::EOI).map(parse_command).collect())
}

pub fn parse_command(pair: Pair) -> Command {
    let span = Span::from_pest(pair.as_span());
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    let variant = match rule {
        Rule::command_definition => {
            pairs.next();
            let name = pairs.next().unwrap().as_str().to_owned();
            let ttype = parse_expr(pairs.next().unwrap());
            let value = parse_expr(pairs.next().unwrap());
            CommandVariant::Definition(name, ttype, value)
        },
        Rule::command_axiom => {
            pairs.next();
            let name = pairs.next().unwrap().as_str().to_owned();
            let ttype = parse_expr(pairs.next().unwrap());
            CommandVariant::Axiom(name, ttype)
        },
        r => unreachable!("{:?}", r),
    };
    Command { span, variant }
}

fn parse_expr(pair: Pair) -> Expr {
    debug_assert!(pair.as_rule() == Rule::expr, "Unexpected rule: {:?}", pair.as_rule());
    let mut exprs = pair.into_inner().map(parse_expr_inner);
    let (f_span, f) = exprs.next().unwrap();
    exprs.fold(f, |f, (v_span, v)|
        Expr {
            span: Span { start: f_span.start, end: v_span.end },
            variant: Box::new(ExprVariant::App(f, v)),
        }
    )
}

fn parse_expr_inner(pair: Pair) -> (Span, Expr) {
    let span = Span::from_pest(pair.as_span());
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    let variant = match rule {
        Rule::expr_paren => return (span, parse_expr(pairs.next().unwrap())),
        Rule::expr_prop => ExprVariant::Prop,
        Rule::expr_var => ExprVariant::Var(pairs.next().unwrap().as_str().to_owned()),
        Rule::expr_product => {
            pairs.next();
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Product(x, x_tp, t)
        },
        Rule::expr_abstract => {
            pairs.next();
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Abstract(x, x_tp, t)
        },
        Rule::expr_bind => {
            pairs.next();
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let x_val = parse_expr(pairs.next().unwrap());
            pairs.next();
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Bind(x, x_tp, x_val, t)
        },
        r => unreachable!("{:?}", r),
    };
    (span.clone(), Expr { span, variant: Box::new(variant) })
}
