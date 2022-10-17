use crate::{
    Binder, Command, CommandVariant, Expr, ExprVariant, IndBody, IndConstructor, Sort, Span,
    SpanError,
};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct HaneParser;

type Pair<'i> = pest::iterators::Pair<'i, Rule>;

type ParseError = SpanError<pest::error::ErrorVariant<Rule>>;

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(err: pest::error::Error<Rule>) -> Self {
        use crate::Location;
        let span = match (err.location, err.line_col) {
            (
                pest::error::InputLocation::Pos(pos),
                pest::error::LineColLocation::Pos((line, col)),
            ) => Span {
                start: Location { pos, line, col },
                end: Location { pos, line, col },
            },
            (
                pest::error::InputLocation::Span((start_pos, end_pos)),
                pest::error::LineColLocation::Span((start_line, start_col), (end_line, end_col)),
            ) => Span {
                start: Location {
                    pos: start_pos,
                    line: start_line,
                    col: start_col,
                },
                end: Location {
                    pos: end_pos,
                    line: end_line,
                    col: end_col,
                },
            },
            _ => unreachable!(),
        };
        SpanError {
            span,
            err: err.variant,
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<Command>, ParseError> {
    let pairs = HaneParser::parse(Rule::commands, input)?;
    Ok(pairs
        .take_while(|pair| pair.as_rule() != Rule::EOI)
        .map(parse_command)
        .collect())
}

pub fn parse_command(pair: Pair) -> Command {
    let span = Span::from_pest(pair.as_span());
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    let variant = match rule {
        Rule::command_definition => {
            let kw = pairs.next();
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("Definition"));
            let name = pairs.next().unwrap().as_str().to_owned();
            let ttype = parse_expr(pairs.next().unwrap());
            let value = parse_expr(pairs.next().unwrap());
            CommandVariant::Definition(name, ttype, value)
        }
        Rule::command_axiom => {
            let kw = pairs.next();
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("Axiom"));
            let name = pairs.next().unwrap().as_str().to_owned();
            let ttype = parse_expr(pairs.next().unwrap());
            CommandVariant::Axiom(name, ttype)
        }
        Rule::command_inductive => {
            // Skips the `Inductive` keyword and steps over the `with` keywords.
            CommandVariant::Inductive(pairs.skip(1).step_by(2).map(parse_ind_body).collect())
        }
        r => unreachable!("{:?}", r),
    };
    Command { span, variant }
}

fn parse_ind_body(pair: Pair) -> IndBody {
    debug_assert_eq!(pair.as_rule(), Rule::inductive_body);
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_owned();
    let params = parse_binders(pairs.next().unwrap());
    let ttype = parse_expr(pairs.next().unwrap());
    let constructors = pairs
        .next()
        .unwrap()
        .into_inner()
        .map(|pair| {
            debug_assert_eq!(pair.as_rule(), Rule::inductive_constructor);
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_owned();
            let ttype = parse_expr(pairs.next().unwrap());
            IndConstructor { name, ttype }
        })
        .collect();

    IndBody {
        name,
        params,
        ttype,
        constructors,
    }
}

fn parse_expr(pair: Pair) -> Expr {
    debug_assert!(
        pair.as_rule() == Rule::expr,
        "Unexpected rule: {:?}",
        pair.as_rule()
    );
    let mut exprs = pair.into_inner().map(parse_expr_inner);
    let (f_span, f) = exprs.next().unwrap();
    exprs.fold(f, |f, (v_span, v)| Expr {
        span: Span {
            start: f_span.start,
            end: v_span.end,
        },
        variant: Box::new(ExprVariant::App(f, v)),
    })
}

fn parse_expr_inner(pair: Pair) -> (Span, Expr) {
    let span = Span::from_pest(pair.as_span());
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    let variant = match rule {
        Rule::expr_paren => return (span, parse_expr(pairs.next().unwrap())),
        Rule::sort => ExprVariant::Sort(parse_sort(pairs.next().unwrap())),
        Rule::expr_var => ExprVariant::Var(pairs.next().unwrap().as_str().to_owned()),
        Rule::expr_product => {
            let kw = pairs.next(); // Skip forall keyword
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("forall"));
            let binders = parse_binders(pairs.next().unwrap()); // parse binders
            let t = parse_expr(pairs.next().unwrap()); // parse body
            ExprVariant::Product(binders, t)
        }
        Rule::expr_abstract => {
            let kw = pairs.next(); // Skip fun keyword
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("fun"));
            let binders = parse_binders(pairs.next().unwrap()); // parse arguments
            let t = parse_expr(pairs.next().unwrap()); // parse body
            ExprVariant::Abstract(binders, t)
        }
        Rule::expr_let_bind => {
            let kw = pairs.next(); // Skip let keyword
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("let"));
            let x = pairs.next().unwrap().as_str().to_owned(); // parse var name
            let x_tp = parse_expr(pairs.next().unwrap()); // parse type
            let x_val = parse_expr(pairs.next().unwrap()); // parse value
            let kw = pairs.next(); // Skip in keyword
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("let"));
            let t = parse_expr(pairs.next().unwrap()); // parse rest of body
            ExprVariant::Bind(x, x_tp, x_val, t)
        }
        r => unreachable!("{:?}", r),
    };
    (
        span.clone(),
        Expr {
            span,
            variant: Box::new(variant),
        },
    )
}

fn parse_sort(pair: Pair) -> Sort {
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    match rule {
        Rule::sort_prop => Sort::Prop,
        Rule::sort_set => Sort::Set,
        Rule::sort_type => {
            let kw = pairs.next(); // skip Type keyword
            debug_assert_eq!(kw.map(|s| s.as_str()), Some("Type"));
            let universe = pairs.next().unwrap().as_str().parse().unwrap(); // parse universe
            Sort::Type(universe)
        }
        r => unreachable!("{:?}", r),
    }
}

fn parse_binders(pair: Pair) -> Vec<Binder> {
    let rule = pair.as_rule();
    debug_assert!(
        rule == Rule::binders_opt || rule == Rule::binders,
        "{rule:?}"
    );
    pair.into_inner()
        .map(|p| {
            debug_assert_eq!(p.as_rule(), Rule::binder_base);
            let mut pairs = p.into_inner();
            Binder {
                name: pairs.next().unwrap().as_str().to_owned(),
                ttype: parse_expr(pairs.next().unwrap()),
            }
        })
        .collect()
}
