use std::fmt::{Display, Debug};

use pyo3::{exceptions::PyRuntimeError, PyErr};
use roots::SearchError;
use thiserror::Error;

use crate::core::triangulation::{PType, Pt};
pub struct PyProxyError(String);

impl Debug for PyProxyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for PyProxyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<ParsedFuncError> for PyProxyError {
    fn from(e: ParsedFuncError) -> Self {
        Self(format!("{}", e))
    }
}

impl From<Display2DError> for PyProxyError {
    fn from(e: Display2DError) -> Self {
        PyProxyError(format!("{}", e))
    }
}

impl From<Display3DError> for PyProxyError {
    fn from(e: Display3DError) -> Self {
        PyProxyError(format!("{}", e))
    }
}

impl From<PyProxyError> for PyErr {
    fn from(e: PyProxyError) -> Self {
        PyRuntimeError::new_err(format!("{}", e))
    }
}

/// An error that can occur during parsing
#[derive(Debug, Error)]
pub enum ParsedFuncError {
    #[error("Unexpected value: {0}")]
    ValueError(String),

    #[error("Variable, constant or function name '{0}' is not valid")]
    BadName(String),

    #[error("Name '{0}' already present in variable, constant and function name declarations")]
    NameTaken(String),

    #[error("{0}")]
    ParsingError(String),

    #[error("Parsing did not consume entire expression at '...{0}'")]
    ResidueError(String),

    #[error("Parameter '{0}' has index {1} which is out of bounds for [T; {2}]")]
    ParameterOutOfBounds(String, usize, usize),
}

impl From<nom::Err<String>> for ParsedFuncError {
    fn from(err: nom::Err<String>) -> Self {
        ParsedFuncError::ParsingError(format!("{:?}", err))
    }
}

#[derive(Debug, Error)]
pub enum Display2DError {
    #[error("Maximum allowed bisections exceeded")]
    MaxHintBisectionsExceeded,

    #[error("Root did not converge: {0}")]
    RootError(String),

    #[error("Non monotone region found after root detection")]
    NonMonotone,

    #[error("BadInput: {0}")]
    BadInput(String),

    #[error("{0}")]
    IntegrationError(IntegError),
}

impl From<SearchError> for Display2DError {
    fn from(se: SearchError) -> Self {
        Display2DError::RootError(format!("{}", se))
    }
}

impl From<IntegError> for Display2DError {
    fn from(ie: IntegError) -> Self {
        Display2DError::IntegrationError(ie)
    }
}

// impl From<Display2DError> for PyErr {
//     fn from(e: Display2DError) -> Self {
//         PyRuntimeError::new_err(format!("{}", e))
//     }
// }

// impl From<ParsedFuncError> for PyErr {
//     fn from(e: ParsedFuncError) -> Self {
//         PyRuntimeError::new_err(format!("{}", e))
//     }
// }

#[derive(Debug, Error)]
pub enum Display3DError {
    #[error("{0}")]
    TriangulationError(TriangulationError),

    #[error("{0}")]
    IntegrationError(IntegError),
}

impl From<TriangulationError> for Display3DError {
    fn from(err: TriangulationError) -> Self {
        Self::TriangulationError(err)
    }
}

impl From<IntegError> for Display3DError {
    fn from(ie: IntegError) -> Self {
        Display3DError::IntegrationError(ie)
    }
}

// impl From<Display3DError> for PyErr {
//     fn from(e: Display3DError) -> Self {
//         PyRuntimeError::new_err(format!("{}", e))
//     }
// }

#[derive(Debug, Error)]
pub enum IntegError {
    #[error("Integral did not converge within permitted iterations")]
    ConvergenceError,

    #[error("NaN value encountered during integration procedure")]
    NaNError,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TriangulationError {
    #[error("Overlap found during evaluation of {0:?} point {1}")]
    Overlap(PType, Pt),

    #[error("Duplicate point found in polygon set: {0}")]
    DuplicatePoint(Pt),

    #[error("Non finite polygon point value enocuntered")]
    NonFiniteInputError,

    #[error("Encountered a polygon with less than 3 points")]
    NoPolygon,

    #[error("Point type could not be derived for point {0}")]
    NoPointType(Pt),
}
