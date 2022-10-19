use peroxide::fuga::RootError;
use thiserror::Error;

/// An error that can occur during parsing
#[derive(Debug, Error)]
pub enum ParseError {
	/// Variable is not in list of allowed variables.
	#[error("Variable '{}' not allowed", var)]
	InvalidVariable {
		var: String,
	},

    /// Variable has been previously declared.
    #[error(
        "Variable '{}' already present in {} declarations",
        var,
        if *in_var_decs {"variable"} else {"function"},
    )]
    VariableTaken {
		var: String,
        in_var_decs: bool,
    },

    #[error("Mexprp parsing error: {:?}", err)]
    MexParsingError {
        err: mexprp::ParseError,
    },
}

impl From<mexprp::ParseError> for ParseError {
    fn from(err: mexprp::ParseError) -> Self {
        Self::MexParsingError { err }
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