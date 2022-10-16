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