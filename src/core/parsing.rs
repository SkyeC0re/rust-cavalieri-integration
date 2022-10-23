use std::{
    collections::{HashMap, HashSet},
    f64::consts::{E, PI},
    fmt::Display,
    ops::{Add, Div, Mul, Neg},
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
    sequence::{delimited, preceded, terminated, tuple},
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
}

pub trait BasicArithmetic: Add<Self> + Mul<Self> + Div<Self> + Neg<Output = Self> + Sized {
    fn powf(self, rhs: Self) -> Self;
    fn powi(self, n: i32) -> Self;
}

trait Parsable: BasicArithmetic + From<f64> + Clone {}

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
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let expr = match tag::<_, _, Error<&str>>("(")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => return Err(e.map(|_| format!("No opening parenthesis found at: '...{}'", expr))),
    };
    let (expr, res) = parse_expression(expr, varops)?;
    let expr = match tag::<_, _, Error<&str>>("(")(expr) {
        Ok((expr, _)) => expr,
        Err(e) => return Err(e.map(|_| format!("No closing parenthesis found at: '...{}'", expr))),
    };
    Ok((expr, res))
}

fn parse_terms_add<'a, T: Parsable>(
    expr: &'a str,
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    todo!()
}

fn parse_terms_mul<'a, T: Parsable>(
    expr: &'a str,
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    todo!()
}

fn parse_neg_count(expr: &str) -> IResult<&str, usize> {
    fold_many0(tag("-"), || 0usize, |accu, _| (accu + 1))(expr)
}

fn parse_name(expr: &str) -> IResult<&str, &str, String> {
    match alpha1::<_, Error<&str>>(expr) {
        Ok(v) => Ok(v),
        Err(e) => Err(e.map(|_| format!("Could not find a valid function name at '...{}'", expr))),
    }
}

fn parse_func<'a, T: Parsable>(
    expr: &'a str,
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, name) = parse_name(expr)?;

    let func = match varops.get(name) {
        Some(Expr::UOp(_, f)) => (*f).clone(),
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
    let (expr, res) = parse_expression(expr, varops)?;
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
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, name) = parse_name(expr)?;

    let res = match varops.get(name) {
        Some(Expr::Const(x)) => Expr::Const(x.clone()),
        Some(Expr::Var(i)) => Expr::Var(*i),
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
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    match double::<_, Error<&str>>(expr) {
        Ok((expr, c)) => Ok((expr, Expr::Const(T::from(c)))),
        _ => return Err(nom::Err::Error(format!("Error parsing f64 at '...{expr}'"))),
    }
}

fn parse_pow<'a, T: Parsable>(
    expr: &'a str,
    varops: &'a HashMap<String, Expr<'a, T>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let expr = match tag::<_, _, Error<&str>>("^")(expr) {
        Ok((expr, _)) => expr,
        Err(_) => {
            return Err(nom::Err::Error(format!(
                "Missing exponentiation sign '^' at '...{expr}'"
            )))
        }
    };
    parse_expression(expr, varops)
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
    varops: &'a HashMap<String, Expr<'a, T>>,
    allow_neg: bool,
) -> IResult<&'a str, Expr<'a, T>, String> {
    let (expr, negations) = match verify(parse_neg_count, |negations| {
        *negations == 0 || (allow_neg && *negations == 1)
    })(expr)
    {
        Ok(v) => v,
        Err(e) => return Err(e.map(|_| format!("Too many negations at: '...{}'", expr))),
    };

    let mut term = parse_parenth(expr, varops)
        .or_else(|e| parse_func(expr, varops))
        .or_else(|e| parse_var(expr, varops))
        .or_else(|e| parse_const(expr, varops))
        .map_err(|_| nom::Err::Error(format!("Error parsing singular term at '...{expr}'")))?;

    // Exponentiation
    if let Ok((expr, exponent)) = parse_pow(expr, varops) {
        term.0 = expr;
        term.1 = Expr::BiOp(Box::new(term.1), Box::new(exponent), &T::powf);
    } else if let Ok((expr, exponent)) = parse_powi(expr) {
        term.0 = expr;
        term.1 = Expr::UOp(Box::new(term.1), move |x: T| x.powi(exponent));
    }

    // Negation
    if negations == 1 {
        term.1 = Expr::UOp(Box::new(term.1), &T::neg);
    }

    Ok(term)
}

fn parse_expression<'a, T: Parsable>(
    expr: &'a str,
    varops: &'a HashMap<String, Expr<impl Parsable>>,
) -> IResult<&'a str, Expr<'a, T>, String> {
    todo!()
}

fn compile_expression<T: Parsable>(
    expr: &str,
    consts: &[(&str, T)],
    vars: &[&str],
    ops: &[(&str, &dyn Fn(T) -> T)],
) -> Result<(), ParsedFuncError> {
    let mut uniq = HashMap::new();

    for (name, val) in consts {
        if uniq
            .insert(name.to_string(), Expr::Const(val.clone()))
            .is_some()
        {
            panic!()
        }
    }

    for (i, name) in vars.iter().copied().enumerate() {
        if uniq.insert(name.to_string(), Expr::Var(i)).is_some() {
            panic!()
        }
    }

    for (name, func) in ops {
        if uniq
            .insert(name.to_string(), Expr::UOp(Box::new(Expr::Var(0)), *func))
            .is_some()
        {
            panic!()
        }
    }

    let mut expr = expr.to_string();
    expr.retain(|c| !c.is_whitespace());

    // let eval = |mut expr: &str| -> IResult<&str, Box<Expr<'a, T>>> {

    // }

    todo!()
}
