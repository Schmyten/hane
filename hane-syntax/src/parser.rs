use pest::Parser;
use pest_derive::Parser;
use crate::{Expr, ExprVariant, Span, SpanError};

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

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let mut pairs = HaneParser::parse(Rule::expr, input)?;
    Ok(parse_expr(pairs.next().unwrap()))
}

fn parse_expr(pair: Pair) -> Expr {
    debug_assert!(pair.as_rule() == Rule::expr);
    let mut exprs = pair.into_inner().map(parse_expr_inner);
    let f = exprs.next().unwrap();
    exprs.fold(f, |f, v|
        Expr {
            span: Span { start: f.span.start, end: v.span.end },
            variant: Box::new(ExprVariant::App(f, v)),
        }
    )
}

fn parse_expr_inner(pair: Pair) -> Expr {
    if pair.as_rule() == Rule::expr {
        return parse_expr(pair);
    }

    let span = Span::from_pest(pair.as_span());
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    let variant = match rule {
        Rule::expr_prop => ExprVariant::Prop,
        Rule::expr_var => ExprVariant::Var(pairs.next().unwrap().as_str().to_owned()),
        Rule::expr_product => {
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Product(x, x_tp, t)
        },
        Rule::expr_abstract => {
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Abstract(x, x_tp, t)
        },
        Rule::expr_bind => {
            let x = pairs.next().unwrap().as_str().to_owned();
            let x_tp = parse_expr(pairs.next().unwrap());
            let x_val = parse_expr(pairs.next().unwrap());
            let t = parse_expr(pairs.next().unwrap());
            ExprVariant::Bind(x, x_tp, x_val, t)
        },
        r => unreachable!("{:?}", r),
    };
    Expr { span, variant: Box::new(variant) }
}
