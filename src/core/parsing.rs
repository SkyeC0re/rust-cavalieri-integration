use std::{
    collections::{HashMap, HashSet},
    f64::consts::{E, PI},
    fmt::Display,
    ops::{Add, Neg},
};

use mexprp::{Answer, Calculation, Config, Context, MathError, Num, Term};

use crate::errors::ParsedFuncError;

use super::differentiable::AD;

pub const VALID_VARIABLE_SYMBOLS: &str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzαβγεζηϝθλμνξρστωψφυ";

pub fn single_var_ad_func<A>(
    func: A,
) -> impl Fn(&[Term<AD>], &Context<AD>) -> Calculation<AD>
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
        if other.1 == 0f64 && (other.0 as i32) as f64 == other.0  {
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
    context.set_func(
        "abs",
        single_var_ad_func(AD::abs)
    );
    context.set_func("ln", single_var_ad_func(AD::ln));

    context
}

pub fn parse_ctx_expr<'a>(
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
