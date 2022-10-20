use peroxide::fuga::RootError;
use pyo3::{
    exceptions::{PyException, PyRuntimeError},
    PyErr,
};
use thiserror::Error;

/// An error that can occur during parsing
#[derive(Debug, Error)]
pub enum ParsedFuncError {
    /// Variable is not in list of allowed variables.
    #[error("Variable '{}' not allowed", var)]
    InvalidVariable { var: String },

    /// Variable has been previously declared.
    #[error(
        "Variable '{}' already present in {} declarations",
        var,
        if *in_var_decs {"variable"} else {"function"},
    )]
    VariableTaken { var: char, in_var_decs: bool },

    #[error("Mexprp parsing error: {:?}", err)]
    MexParsingError { err: mexprp::ParseError },

    #[error("Mexprp parsing error: {:?}", err)]
    MexMathError { err: mexprp::MathError },

    #[error(
        "Wrong amount of input parameters used, got {}, expected {:?}",
        count,
        expected,
    )]
    IncorrectParameters { count: usize, expected: Vec<String> },

    #[error("Unexpected value: {}", err)]
    ValueError { err: String },
}

impl From<mexprp::ParseError> for ParsedFuncError {
    fn from(err: mexprp::ParseError) -> Self {
        Self::MexParsingError { err }
    }
}

impl From<mexprp::MathError> for ParsedFuncError {
    fn from(err: mexprp::MathError) -> Self {
        Self::MexMathError { err }
    }
}

#[derive(Debug, Error)]
pub enum Display2DError {
    #[error("Maximum allowed bisections exceeded")]
    MaxHintBisectionsExceeded,

    #[error("Root did not converge: Peroxide Error: {0}")]
    RootError(String),

    #[error("Non monotone region found after root detection.")]
    NonMonotone,

    #[error("BadInput: {0}")]
    BadInput(String),
}

impl From<RootError> for Display2DError {
    fn from(re: RootError) -> Self {
        Display2DError::RootError(format!("{}", re))
    }
}

impl From<Display2DError> for PyErr {
    fn from(e: Display2DError) -> Self {
        PyRuntimeError::new_err(format!("{}", e))
    }
}

impl From<ParsedFuncError> for PyErr {
    fn from(e: ParsedFuncError) -> Self {
        PyRuntimeError::new_err(format!("{}", e))
    }
}
