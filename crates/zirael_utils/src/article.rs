pub fn article(input: &str) -> String {
  match input.chars().next() {
    Some(c) if "aeiouAEIOU".contains(c) => format!("an {input}"),
    Some(_) | None => format!("a {input}"),
  }
}
