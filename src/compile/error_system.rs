use crate::compile::{CompileError, FileInfo};
use std::collections::BTreeMap;

#[derive(Clone)]
///A collection of all compiler warnings encountered and the mechanism to handle them.
pub struct ErrorSystem {
    ///All compilation errors
    errors: Vec<CompileError>,
    ///All compilation warnings
    warnings: Vec<CompileError>,
    ///Config for displaying errors,
    pub config: ErrorConfig,
}

#[derive(Clone)]
pub struct WarningSystem {
    ///All compilation warnings
    warnings: Vec<CompileError>,
    ///Config for displaying errors,
    pub config: ErrorConfig,
}

#[derive(Clone)]
pub struct ErrorConfig {
    ///Whether these should halt compilation
    pub warnings_are_errors: bool,
    ///The color to use when highlighting parts of the body text
    pub warn_color: &'static str,
    ///File information that helps the error system pretty-print errors and warnings
    pub file_info_chart: BTreeMap<u64, FileInfo>,
}

impl From<WarningSystem> for ErrorSystem {
    fn from(from: WarningSystem) -> Self {
        let WarningSystem { warnings, config } = from;
        Self {
            errors: vec![],
            warnings,
            config,
        }
    }
}

impl ErrorSystem {
    pub fn new(warnings_are_errors: bool) -> Self {
        ErrorSystem {
            errors: vec![],
            warnings: vec![],
            config: ErrorConfig {
                warnings_are_errors,
                warn_color: match warnings_are_errors {
                    true => CompileError::PINK,
                    false => CompileError::YELLOW,
                },
                file_info_chart: BTreeMap::new(),
            },
        }
    }
    pub fn print(&self) {
        for warning in &self.warnings {
            warning.print(
                &self.config.file_info_chart,
                self.config.warnings_are_errors,
            );
        }
        for error in &self.errors {
            error.print(
                &self.config.file_info_chart,
                self.config.warnings_are_errors,
            );
        }
    }
    pub fn push_error(&mut self, error: CompileError) {
        self.errors.push(error);
    }
    pub fn push_warning(&mut self, error: CompileError) {
        self.warnings.push(error);
    }
    pub fn map_error<T>(mut self, res: Result<T, CompileError>) -> Result<T, ErrorSystem> {
        res.map_err(|e| {
            self.push_error(e);
            self.clone()
        })
    }
    pub fn extend_warnings(&mut self, system: Vec<CompileError>) {
        self.warnings.extend(system)
    }
    pub fn warnings(&self) -> &[CompileError] {
        &self.warnings
    }
    pub fn _errors(&self) -> &[CompileError] {
        &self.errors
    }
}

impl WarningSystem {
    pub fn new(warnings_are_errors: bool) -> Self {
        WarningSystem {
            warnings: vec![],
            config: ErrorConfig {
                warnings_are_errors,
                warn_color: match warnings_are_errors {
                    true => CompileError::PINK,
                    false => CompileError::YELLOW,
                },
                file_info_chart: BTreeMap::new(),
            },
        }
    }
    pub fn new_from_config(config: ErrorConfig) -> Self {
        WarningSystem {
            warnings: vec![],
            config,
        }
    }
    pub fn _print(&self) {
        for warning in &self.warnings {
            warning.print(
                &self.config.file_info_chart,
                self.config.warnings_are_errors,
            );
        }
    }
    pub fn push_warning(&mut self, error: CompileError) {
        self.warnings.push(error);
    }
    pub fn extend_warnings(&mut self, system: Vec<CompileError>) {
        self.warnings.extend(system)
    }
    pub fn warnings(&self) -> &[CompileError] {
        &self.warnings
    }
}
