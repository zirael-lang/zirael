use crate::codegen::Codegen;
use anyhow::Result;
use std::path::PathBuf;
use zirael_parser::MainFunction;
use zirael_utils::prelude::CompilationInfo;

pub struct CodegenWriter;

impl CodegenWriter {
  pub fn write_includes(
    implementation: &mut Codegen,
    header_file: &PathBuf,
    used_externals: &[String],
  ) {
    implementation.writeln(&format!("#include \"{}\"", header_file.display()));
    for external in used_externals {
      let file = PathBuf::from(external.clone()).with_extension("h");
      implementation.writeln(&format!("#include \"{}\"", file.display()));
    }
    implementation.writeln("");
  }

  pub fn write_main_function(implementation: &mut Codegen, main_fn: &Option<MainFunction>) {
    if let Some(func) = main_fn
      && let MainFunction::Mangled(mangled) = func
    {
      implementation.writeln("int main() {");
      implementation.indent();
      implementation.writeln(&format!("{mangled}();"));
      implementation.writeln("return 0;");
      implementation.dedent();
      implementation.writeln("}");
    }
  }

  pub fn write_files(
    info: &CompilationInfo,
    header: &Codegen,
    implementation: &Codegen,
  ) -> Result<PathBuf> {
    let header_file = PathBuf::from(info.name.clone()).with_extension("h");
    let output_file = info.write_to.join(info.name.clone()).with_extension("c");

    fs_err::create_dir_all(&info.write_to)?;
    fs_err::write(info.write_to.join(header_file), header.content.buffer())?;
    fs_err::write(output_file.clone(), implementation.content.buffer())?;

    Ok(output_file)
  }
}
