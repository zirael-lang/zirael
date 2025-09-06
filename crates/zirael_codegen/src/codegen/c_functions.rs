const STANDARD_C_FUNCTIONS: &[&str] = &[
  // <stdlib.h>
  "malloc", "free", "calloc", "realloc", "exit", "abort", "atoi", "atol", "atof", "rand", "srand",
  // <string.h>
  "strlen", "strcpy", "strncpy", "strcat", "strncat", "strcmp", "strncmp", "strchr", "strrchr",
  "memcpy", "memmove", "memset", "memcmp", // <stdio.h>
  "printf", "scanf", "puts", "gets", "fopen", "fclose", "fread", "fwrite", "fgets", "fputs",
  "fprintf", "fscanf", "sprintf", "sscanf", // <math.h>
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "sqrt", "pow", "exp", "log",
  "log10", // <ctype.h>
  "isalpha", "isdigit", "isalnum", "isspace", "isupper", "islower", "toupper", "tolower",
];

pub fn is_standard_c_function(name: &str) -> bool {
  STANDARD_C_FUNCTIONS.contains(&name)
}
