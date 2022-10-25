use peroxide::fuga::RootError;
use pyo3::{exceptions::PyRuntimeError, PyErr};
use roots::SearchError;
use thiserror::Error;

/// An error that can occur during parsing
#[derive(Debug, Error)]
pub enum ParsedFuncError {

    #[error("Unexpected value: {}", err)]
    ValueError { err: String },

    #[error("Variable, constant or function name '{0}' is not valid")]
    BadName(String),

    #[error("Name '{0}' already present in variable, constant and function name declarations")]
    NameTaken(String),

    #[error("Nom parsing error: {0}")]
    NomError(String),

    #[error("Parsing did not consume entire expression at '...{0}'")]
    ResidueError(String),

    #[error("Parameter '{0}' has index {1} which is out of bounds for [T; {2}]")]
    ParameterOutOfBounds(String, usize, usize),
}

impl From<nom::Err<String>> for ParsedFuncError {
    fn from(err: nom::Err<String>) -> Self {
        ParsedFuncError::NomError(format!("{:?}", err))
    }
}

#[derive(Debug, Error)]
pub enum Display2DError {
    #[error("Maximum allowed bisections exceeded")]
    MaxHintBisectionsExceeded,

    #[error("Root did not converge: Peroxide Error: {0}")]
    RootError(String),

    #[error("Non monotone region found after root detection")]
    NonMonotone,

    #[error("BadInput: {0}")]
    BadInput(String),

    #[error("Integration Error: {}", err)]
    IntegrationError { err: IntegError },
}

impl From<RootError> for Display2DError {
    fn from(re: RootError) -> Self {
        Display2DError::RootError(format!("{}", re))
    }
}

impl From<SearchError> for Display2DError {
    fn from(se: SearchError) -> Self {
        Display2DError::RootError(format!("{}", se))
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

#[derive(Debug, Error)]
pub enum IntegError {
    #[error("Integral did not converge within permitted iterations")]
    ConvergenceError,

    #[error("NaN value encountered during integration procedure")]
    NaNError,
}

impl From<IntegError> for Display2DError {
    fn from(ie: IntegError) -> Self {
        Display2DError::IntegrationError { err: ie }
    }
}


#[derive(Debug, Error)]
pub enum TriangulationError {
    #[error("Overlapping lines found at {0:?}")]
    Overlap([f64;2]),

    #[error("Duplicate point found in polygon set: {0:?}")]
    DuplicatePoint(([f64; 2])),

    #[error("NaN value encountered during triangulation procedure")]
    NaNError,
}