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
