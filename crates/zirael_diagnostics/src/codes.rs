use crate::DiagnosticCode;

// NOTE: Codes are global (0001..9999). Do NOT reuse or renumber existing codes.
// If you change meaning, add a new code.

// --- Lexer (0001..0099) ---
macro_rules! define_codes {
  ($($name:ident => $value:literal),+ $(,)?) => {
    $(
      pub const $name: DiagnosticCode = DiagnosticCode::new($value);
    )+

    pub const ALL_CODES: &[DiagnosticCode] = &[
      $( $name, )+
    ];
  };
}

define_codes! {
  LEX_UNTERMINATED_STRING => 1,
  LEX_UNTERMINATED_CHAR => 2,
  LEX_UNTERMINATED_BLOCK_COMMENT => 3,
  LEX_INVALID_ESCAPE => 4,
  LEX_INVALID_BYTE_VALUE => 5,
  LEX_EMPTY_CHAR_LITERAL => 6,
  LEX_MULTI_CHAR_LITERAL => 7,
  LEX_MISSING_DIGITS_AFTER_BASE => 8,
  LEX_INVALID_DIGIT_FOR_BASE => 9,
  LEX_MALFORMED_EXPONENT => 10,
  LEX_INVALID_UTF8 => 11,
  LEX_UNEXPECTED_BOM => 12,
  LEX_DISALLOWED_CODE_POINT => 13,
  LEX_NON_ASCII_WHITESPACE => 14,
  LEX_INVALID_IDENTIFIER_START => 15,
  LEX_INVALID_IDENTIFIER_CONTINUE => 16,
  LEX_JOIN_CONTROL_IN_IDENTIFIER => 17,
  LEX_UNATTACHED_DOC_COMMENT => 18,
  LEX_UNEXPECTED_CHARACTER => 19,

  // --- Parser (0100..0199) ---
  PARSE_UNEXPECTED_TOKEN => 100,
  PARSE_MOD_STRING_LIT => 101,
  PARSE_MOD_EXPECTED_IDENTIFIER => 102,
  PARSE_CONST_ALONE_IN_A_TYPE => 103,
  PARSE_CONST_EXPECTED_FUNC_OR_IDENT => 104,
  PARSE_CONST_NEED_TYPE_ANNOTATIONS => 105,
  PARSE_CONST_CANNOT_BE_UNINITIALIZED => 106,
  PARSE_EXPECTED_TYPE => 107,
  PARSE_SELF_AND_PACKAGE_ROOT_ONLY => 108,
  PARSE_EXPECTED_SUPER_OR_IDENT_PATH => 109,
  PARSE_FUNCTION_CAMEL_CASE => 110,
  PARSE_EXPECTED_IDENTIFIER_IN_GENERIC => 111,
  PARSE_EXPECTED_TYPE_PATH_FOR_BOUND => 112,
  PARSE_TRAILING_PLUS_IN_TYPE_BOUND => 113,
  PARSE_EXPECTED_IDENT_OR_DOTS => 114,
  PARSE_VARIADIC_NO_DEFAULT => 115,
  PARSE_ALL_VARS_INIT => 116
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::HashSet;

  #[test]
  fn codes_are_unique_and_in_range() {
    let mut seen = HashSet::new();

    for &code in ALL_CODES {
      assert!((1..=9999).contains(&code.get()));
      assert!(seen.insert(code), "duplicate diagnostic code: {code}");

      let formatted = code.to_string();
      assert_eq!(formatted.len(), 4);
      assert!(formatted.chars().all(|c| c.is_ascii_digit()));
    }
  }
}
