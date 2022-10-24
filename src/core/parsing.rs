use std::{
    collections::{HashMap, HashSet},
    f64::consts::{E, PI},
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
    rc::Rc,
};

use mexprp::{Answer, Calculation, Config, Context, Expression, MathError, Num, Term};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::{alpha1, multispace0},
    combinator::{map_res, opt, peek, recognize, verify},
    error::{Error, ErrorKind, ParseError},
    multi::fold_many0,
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::errors::ParsedFuncError;

use super::differentiable::AD;

pub const VALID_VARIABLE_SYMBOLS: &str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzαβγεζηϝθλμνξρστωψφυ";

pub fn single_var_ad_func<A>(func: A) -> impl Fn(&[Term<AD>], &Context<AD>) -> Calculation<AD>
where
    A: Fn(AD) -> AD,
{
    return move |args: &[Term<AD>], ctx: &Context<AD>| {
        if args.len() != 1 {
            return Err(MathError::IncorrectArguments);
        }
        let x = match args[0].eval_ctx(ctx)? {
            Answer::Single(x) => x,
            Answer::Multiple(_) => return Err(MathError::IncorrectArguments),
        };

        Ok(Answer::Single(func(x)))
    };
}

// enum CompiledExpression<T> {

// }

// fn compile_expression1_1(expr: Expression<AD>, par: char ) -> Result<impl Fn(AD) -> AD, ParsedFuncError> {
//     let compile = |expr: &Expression<AD>| {
//         match expr.term {
//             Term::Var(name) => if name == String::from(par) {
//                 Ok(Box::new(|(x, _)| (x, AD(x, 1f64))))
//             } else if let Some(term) = expr.ctx.vars.get(&name){
//                 let val = match term.eval()? {
//                     Answer::Single(x) => x,
//                     _ => return Err(ParsedFuncError::ValueError { err: "Single answer expected".to_string() })
//                 };
//                 Ok(Box::new(|(x, _)| (x, val)))
//             },
//             Term::Num(ans) => match ans {
//                 Answer::Single(x) => Ok(Box::new(|(x, _)| (x, *AD))),
//                 _ => return Err(ParsedFuncError::ValueError { err: "Single answer expected".to_string() }),
//             },
//             Term::Operation(rc_op) => {
//                 let op = match Rc::try_unwrap(rc_op) {
//                     Ok(op) => op,
//                 }
//             }

//         }
//     }

// }

impl Display for AD {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<AD> for Answer<AD> {
    fn from(x: AD) -> Self {
        Answer::Single(x)
    }
}

impl Num for AD {
    fn from_f64(t: f64, _ctx: &Context<Self>) -> mexprp::Calculation<Self> {
        Calculation::Ok(Answer::Single(AD::from(t)))
    }

    fn from_f64_complex(_t: (f64, f64), _ctx: &Context<Self>) -> mexprp::Calculation<Self> {
        Calculation::Err(MathError::Unimplemented {
            op: "parse".to_string(),
            num_type: "complex".to_string(),
        })
    }

    fn typename() -> String {
        "AD".to_string()
    }

    fn add(&self, other: &Self, _ctx: &Context<Self>) -> Calculation<Self> {
        Ok((*self + *other).into())
    }

    fn sub(&self, other: &Self, _ctx: &Context<Self>) -> Calculation<Self> {
        Ok((*self - *other).into())
    }

    fn mul(&self, other: &Self, _ctx: &Context<Self>) -> Calculation<Self> {
        Ok((*self * *other).into())
    }

    fn div(&self, other: &Self, _ctx: &Context<Self>) -> Calculation<Self> {
        Ok((*self / *other).into())
    }

    fn pow(&self, other: &Self, _ctx: &Context<Self>) -> Calculation<Self> {
        if other.1 == 0f64 && (other.0 as i32) as f64 == other.0 {
            Ok(AD::powi(*self, other.0 as i32).into())
        } else {
            Ok(AD::pow(*self, *other).into())
        }
    }
}

pub fn standard_context() -> Context<AD> {
    let cfg = Config {
        implicit_multiplication: true,
        sqrt_both: false,
        ..Default::default()
    };

    let mut context: Context<AD> = Context {
        vars: HashMap::new(),
        funcs: HashMap::new(),
        cfg,
    };

    context.set_var("pi", AD::from(PI));
    context.set_var("π", AD::from(PI));
    context.set_var("e", AD::from(E));

    context.set_func("sin", single_var_ad_func(AD::sin));
    context.set_func("cos", single_var_ad_func(AD::cos));
    context.set_func("tan", single_var_ad_func(AD::tan));
    context.set_func("asin", single_var_ad_func(AD::asin));
    context.set_func("acos", single_var_ad_func(AD::acos));
    context.set_func("atan", single_var_ad_func(AD::atan));
    context.set_func("sinh", single_var_ad_func(AD::sinh));
    context.set_func("cosh", single_var_ad_func(AD::cosh));
    context.set_func("tanh", single_var_ad_func(AD::tanh));
    context.set_func("atanh", single_var_ad_func(AD::asinh));
    context.set_func("acosh", single_var_ad_func(AD::acosh));
    context.set_func("atanh", single_var_ad_func(AD::atanh));
    context.set_func("sqrt", single_var_ad_func(AD::sqrt));
    context.set_func("abs", single_var_ad_func(AD::abs));
    context.set_func("ln", single_var_ad_func(AD::ln));

    context
}

pub fn parse_ctx_expr(
    expr: &str,
    vars: &str,
    mut ctx: Context<AD>,
) -> Result<impl FnMut(&[AD]) -> Result<AD, ParsedFuncError> + Clone, ParsedFuncError> {
    let mut valid_vars = HashSet::new();
    let mut pars: Vec<String> = Vec::with_capacity(vars.len());
    valid_vars.extend(VALID_VARIABLE_SYMBOLS.chars());

    for var in vars.chars() {
        let svar = var.to_string();

        if !valid_vars.contains(&var) {
            return Err(ParsedFuncError::InvalidVariable { var: svar.clone() });
        }

        if ctx.vars.contains_key(&svar) {
            return Err(ParsedFuncError::VariableTaken {
                var: var,
                in_var_decs: true,
            });
        }

        if ctx.funcs.contains_key(&svar) {
            return Err(ParsedFuncError::VariableTaken {
                var: var,
                in_var_decs: false,
            });
        }

        ctx.set_var(&svar, AD::from(0f64));
        pars.push(svar);
    }
    let expr = Term::parse_ctx(expr, &ctx)?;

    let pvar_len = pars.len();
    let f = move |xv: &[AD]| -> Result<AD, ParsedFuncError> {
        if xv.len() != pvar_len {
            return Err(ParsedFuncError::IncorrectParameters {
                count: xv.len(),
                expected: pars.clone(),
            });
        }
        for (x, name) in xv.into_iter().zip(pars.iter()) {
            if let Some(term) = ctx.vars.get_mut(name) {
                *term = Term::Num(Answer::Single(x.clone().into()));
            }
        }

        match expr.eval_ctx(&ctx)? {
            Answer::Single(x) => Ok(x),
            _ => Err(ParsedFuncError::ValueError {
                err: "Expected singular answer".to_string(),
            }),
        }
    };
    Ok(f)
}

pub fn parse_expr(
    expr: &str,
    vars: &str,
) -> Result<impl FnMut(&[AD]) -> Result<AD, ParsedFuncError> + Clone, ParsedFuncError> {
    parse_ctx_expr(expr, vars, standard_context())
}

enum Expr<'a, T> {
    Var(usize),
    Const(T),
    UOp(Box<Expr<'a, T>>, &'a dyn Fn(T) -> T),
    BiOp(Box<Expr<'a, T>>, Box<Expr<'a, T>>, &'a dyn Fn(T, T) -> T),
    Powi(Box<Expr<'a, T>>, i32),
}


pub trait BasicArithmetic:
    Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self, Output = Self>
    + Div<Self, Output = Self>
    + Neg<Output = Self>
    + Clone
    + Sized
{
    fn powf(self, rhs: Self) -> Self;
    fn powi(self, n: i32) -> Self;
}

impl<'a, T: BasicArithmetic> Expr<'a, T> {
    pub fn eval(&self, pars: &[T]) -> T {
        match self {
            Expr::Const(c) => c.clone(),
            Expr::Var(i) => pars[*i].clone(),
            Expr::UOp(inner, op) => op(inner.eval(pars)),
            Expr::BiOp(inner1,inner2 , op) => op(inner1.eval(pars), inner2.eval(pars)),
            Expr::Powi(inner, exponent) => inner.eval(pars).powi(*exponent),
        }
    }

    pub fn safe_eval(&self, pars: &[T]) -> Option<T> {
        match self {
            Expr::Const(c) => Some(c.clone()),
            Expr::Var(i) => pars.get(*i).cloned(),
            Expr::UOp(inner, op) => inner.safe_eval(pars).map(|x| op(x)),
            Expr::BiOp(inner1,inner2 , op) => inner1.safe_eval(pars).zip(inner2.safe_eval(pars)).map(|(x1, x2)| op(x1, x2)),
            Expr::Powi(inner, exponent) => inner.safe_eval(pars).map(|x| x.powi(*exponent)),
        }
    }
}

pub trait Parsable: BasicArithmetic + From<f64> {}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

fn parse_parenth<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let expr = match tag::<_, _, Error<&str>>("(")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => return Err(e.map(|_| format!("No opening parenthesis found at: '...{}'", expr))),
    };
    let (expr, res) = parse_expression(expr, context)?;
    let expr = match tag::<_, _, Error<&str>>("(")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => return Err(e.map(|_| format!("No closing parenthesis found at: '...{}'", expr))),
    };
    Ok((expr, res))
}

fn parse_neg_count(expr: &str) -> IResult<&str, usize> {
    fold_many0(tag("-"), || 0usize, |accu, _| (accu + 1))(expr)
}

fn parse_name(expr: &str) -> IResult<&str, &str, String> {
    match alpha1::<_, Error<&str>>(expr) {
        Ok(v) => Ok(v),
        Err(e) => Err(e.map(|_| format!("Could not find a valid name at '...{}'", expr))),
    }
}

fn parse_func<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, name) = parse_name(expr)?;

    let func = match context.get(name) {
        Some(ContextElement::UOp(f)) => (*f).clone(),
        _ => {
            return Err(nom::Err::Error(format!(
                "No unary operator with name '{name}' exists"
            )))
        }
    };

    let expr = match tag::<_, _, Error<&str>>("(")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => {
            return Err(
                e.map(|_| format!("Expected function '{name}' opening parenthesis at '...{expr}'"))
            )
        }
    };
    let (expr, res) = parse_expression(expr, context)?;
    let expr = match tag::<_, _, Error<&str>>(")")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => {
            return Err(
                e.map(|_| format!("Expected function '{name}' closing parenthesis at '...{expr}'"))
            )
        }
    };

    Ok((expr, Expr::UOp(Box::new(res), func)))
}

fn parse_var<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, name) = parse_name(expr)?;

    let res = match context.get(name) {
        Some(ContextElement::Const(x)) => Expr::Const(x.clone()),
        Some(ContextElement::Var(i)) => Expr::Var(*i),
        _ => {
            return Err(nom::Err::Error(format!(
                "No variable or parameter with name '{name}' exists"
            )))
        }
    };

    Ok((expr, res))
}

fn parse_const<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    match double::<_, Error<&str>>(expr) {
        Ok((expr, c)) => Ok((expr, Expr::Const(T::from(c)))),
        _ => return Err(nom::Err::Error(format!("Error parsing f64 at '...{expr}'"))),
    }
}

fn parse_pow<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let expr = match tag::<_, _, Error<&str>>("^")(expr) {
        Ok((expr, _)) => expr,
        Err(_) => {
            return Err(nom::Err::Error(format!(
                "Missing exponentiation sign '^' at '...{expr}'"
            )))
        }
    };
    parse_expression(expr, context)
}

fn parse_powi<'a>(expr: &'a str) -> IResult<&'a str, i32, String> {
    let expr = match tag::<_, _, Error<&str>>("**")(expr) {
        Ok((expr, _)) => expr,
        Err(_) => {
            return Err(nom::Err::Error(format!(
                "Missing exponentiation sign '^' at '...{expr}'"
            )))
        }
    };
    nom::character::complete::i32::<_, Error<&str>>(expr)
        .map_err(|_| nom::Err::Error(format!("Could not build a valid i32 at '...{expr}'")))
}

fn parse_term<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
    allow_neg: bool,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, negations) = match verify(parse_neg_count, |negations| {
        *negations == 0 || (allow_neg && *negations == 1)
    })(expr)
    {
        Ok(v) => v,
        Err(e) => return Err(e.map(|_| format!("Too many negations at: '...{}'", expr))),
    };

    let mut term = parse_parenth(expr, context)
        .or_else(|_| parse_func(expr, context))
        .or_else(|_| parse_var(expr, context))
        .or_else(|_| parse_const(expr, context))
        .map_err(|_| nom::Err::Error(format!("Error parsing singular term at '...{expr}'")))?;

    // Exponentiation
    if let Ok((expr, exponent)) = parse_pow(expr, context) {
        term.0 = expr;
        term.1 = Expr::BiOp(Box::new(term.1), Box::new(exponent), &T::powf);
    } else if let Ok((expr, exponent)) = parse_powi(expr) {
        term.0 = expr;
        term.1 = Expr::Powi(Box::new(term.1), exponent);
    }

    // Negation
    if negations == 1 {
        term.1 = Expr::UOp(Box::new(term.1), &T::neg);
    }

    Ok(term)
}

fn parse_terms_mul<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
    allow_neg: bool,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let mut terms = parse_term(expr, context, allow_neg)?;
    while let Ok((nexpr, op)) = alt::<_, _, Error<&str>, _>((tag("*"), tag("/")))(expr) {
        let term = parse_term(nexpr, context, true)?;
        let func: &dyn Fn(T, T) -> T = match op {
            "*" => &T::div,
            "/" => &T::mul,
            _ => unreachable!(),
        };
        terms.0 = term.0;
        terms.1 = Expr::BiOp(Box::new(terms.1), Box::new(term.1), func);
    }
    Ok(terms)
}

fn parse_expression<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let mut terms = parse_terms_mul(expr, context, true)?;
    while let Ok((nexpr, op)) = alt::<_, _, Error<&str>, _>((tag("+"), tag("-")))(expr) {
        let term = parse_terms_mul(nexpr, context, false)?;
        let func: &dyn Fn(T, T) -> T = match op {
            "+" => &T::add,
            "-" => &T::sub,
            _ => unreachable!(),
        };
        terms.0 = term.0;
        terms.1 = Expr::BiOp(Box::new(terms.1), Box::new(term.1), func);
    }
    Ok(terms)
}

fn parse_expression_outer<'a, T: Parsable>(
    expr: &'a str,
    context: &'a HashMap<String, ContextElement<'a, T>>,
) -> Result<Expr<'a, T>, ParsedFuncError> {
    let term  = parse_expression(expr, context)?;
    if !term.0.is_empty() {
        return Err(ParsedFuncError::ResidueError(term.0.to_string()));
    }
    Ok(term.1)
}

pub enum ContextElement<'a, T: Parsable> {
    Const(T),
    Var(usize),
    UOp(&'a dyn Fn(T) -> T)
}

pub fn compile_expression<'a, T: Parsable, F: Fn(&[T]) -> T>(
    expr: &'a str,
    context: HashMap<String, ContextElement<'a, T>>
) -> Result<F, ParsedFuncError> {
    for name in context.keys() {
        if parse_name(name).is_err() {
            return Err(ParsedFuncError::BadName(name.to_string()));
        }
    }
    let mut expr = expr.to_string();
    expr.retain(|c| !c.is_whitespace());

    let term = parse_expression_outer::<T>(&expr, &context)?;
    Ok(move |pars|term.eval(pars))
}
