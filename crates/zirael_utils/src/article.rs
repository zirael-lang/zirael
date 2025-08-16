pub fn article(input: &str) -> String {
    match input.chars().next() {
        Some(c) if "aeiouAEIOU".contains(c) => format!("an {input}"),
        Some(_) => format!("a {input}"),
        None => format!("a {input}"),
    }
}
