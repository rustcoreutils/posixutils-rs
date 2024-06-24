use std::process::Termination;

use super::constants::{EXIT_STATUS_DIFFERENCE, EXIT_STATUS_NO_DIFFERENCE, EXIT_STATUS_TROUBLE};

#[allow(dead_code)]
#[derive(Clone, Copy)]
pub enum DiffExitStatus {
    NotDifferent,
    Different,
    Trouble,
}

impl DiffExitStatus {
    pub fn status_code(&self) -> i32 {
        match self {
            DiffExitStatus::NotDifferent => EXIT_STATUS_NO_DIFFERENCE,
            DiffExitStatus::Different => EXIT_STATUS_DIFFERENCE,
            DiffExitStatus::Trouble => EXIT_STATUS_TROUBLE,
        }
    }
}

impl Termination for DiffExitStatus {
    fn report(self) -> std::process::ExitCode {
        if self.status_code() == EXIT_STATUS_TROUBLE {
            return std::process::ExitCode::FAILURE;
        }

        std::process::ExitCode::SUCCESS
    }
}
