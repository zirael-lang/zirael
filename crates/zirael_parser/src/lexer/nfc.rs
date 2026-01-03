pub fn is_xid_start(ch: char) -> bool {
  use unicode_xid::UnicodeXID as _;
  ch.is_xid_start()
}

pub fn is_xid_continue(ch: char) -> bool {
  use unicode_xid::UnicodeXID as _;
  ch.is_xid_continue()
}

pub fn normalize_nfc(s: &str) -> String {
  use unicode_normalization::UnicodeNormalization as _;
  s.nfc().collect()
}
