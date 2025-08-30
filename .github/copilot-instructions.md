# Zirael Compiler Development Guide

Zirael is a systems programming language compiler written in Rust that compiles `.zr` source files to C code.
Understanding the multi-stage compilation pipeline and crate architecture is essential for effective development.

## Architecture Overview

The compiler follows a traditional multi-pass architecture with these core crates:

- `zirael_parser` - Lexing, parsing, and AST generation
- `zirael_core` - Compilation orchestration and semantic analysis passes
- `zirael_type_checker` - Type inference and monomorphization
- `zirael_hir` - High-level IR lowering and optimizations
- `zirael_codegen` - IR-to-C code generation
- `zirael_utils` - Shared utilities (error reporting, logging, symbol tables)

## Key Compilation Pipeline

The `CompilationUnit::compile()` method in `crates/zirael_core/src/unit.rs` orchestrates the full pipeline:

1. **Module Discovery** - Parse dependency graph from entry point
2. **Declaration Collection** - Gather all symbols and types
3. **Name Resolution** - Resolve identifiers to symbols
4. **Memory Analysis** - Analyze ownership and lifetimes
5. **Type Inference** - Infer types and handle generics via monomorphization
6. **HIR Lowering** - Convert AST to high-level intermediate representation
7. **IR Lowering** - Convert HIR to low-level IR
8. **Code Generation** - Emit C code from IR

Each pass uses the AST walker pattern (`impl_ast_walker!` macro) for traversal.

## Development Workflows

### Building and Testing

```bash
# Build entire workspace
cargo check --workspace --all-features --all-targets

# Run compiler on test file
cargo run -p zirael playground/test.zr --name playground -o playground/build

# Development convenience commands (requires just)
just comp-run                    # Compile playground/test.zr
just fix                        # Auto-fix formatting and lints
just lint                       # Run clippy with strict settings
```

When running multiple commends don't use statement separators.

### Language File Extension

- Source files use `.zr` extension (defined as `FILE_EXTENSION` constant)
- Standard library is in `std/src/lib.zr`
- Example files in `playground/` directory

## Critical Patterns

### Error Reporting

Use the `Reports` system from `zirael_utils::reports` for all diagnostics:

```rust
reports.add(source_id, ReportBuilder::error()
.with_message("Type mismatch")
.with_label(Label::new(span).with_message("Expected int, found string")));
```

### Symbol Table Access

All passes receive symbols through constructor: `PassName::new(symbols, reports, sources)`

- Lookup: `self.symbol_table.lookup_symbol(name)`
- Symbol kinds: Function, Struct, Variable, etc.

### Type System

- Generic types use monomorphization table (`MonomorphizationTable`)
- Type inference creates `MonomorphizedSymbol` for concrete generic instantiations
- Type equality checking via `TypeInference::eq()` handles monomorphization

### AST Walker Pattern

Implement semantic analysis passes using the walker macro:

```rust
impl_ast_walker!(YourPass, {
    // your context fields
});
```

Override specific visit methods like `visit_function`, `visit_struct_definition`.

## Integration Points

### CLI Interface (`compiler_cli/src/cli.rs`)

- Entry point accepts `.zr` file, compilation mode, output path
- Supports dependency packages via `-d` flag: `name:write_to=entrypoint`
- Modes: debug/release affect IR generation

### Context Sharing

`Context` object (in `zirael_core`) provides shared access to:

- Source files (`Sources`)
- Symbol table (`SymbolTable`)
- Error reports (`Reports`)
- Package registry (`Packages`)

### Code Generation

Final stage outputs C headers/source to specified directory. Generated C code structure depends on `LibType` (
static/dynamic library).

## Project Conventions

- Use `anyhow::Result` for error handling
- Extensive linting via workspace Clippy configuration
- No `unsafe` code allowed (workspace lint rule)
- ID arenas for efficient symbol/type storage
- Parking lot for internal synchronization primitives
