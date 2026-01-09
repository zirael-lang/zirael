use crate::directives::LineDirection;
use crate::test::TestId;

#[derive(Debug, Clone)]
pub enum TestStatus {
  Passed,
  Failed(Vec<FailureType>),
  Skipped,
}

#[derive(Debug, Clone)]
pub enum FailureType {
  OtherCompilerError,
  ExpectedErrorNotFound {
    line: usize,
    pattern: String,
    direction: LineDirection,
  },
  ExpectedErrorsButCompiled,
  UnexpectedErrors(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct TestResult {
  pub result: TestStatus,
  pub output: Vec<u8>,
  pub test_id: TestId,
}
