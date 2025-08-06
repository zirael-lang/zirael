use crate::inference::TypeInference;
use zirael_parser::Type;
use zirael_utils::prelude::{Colorize, ReportBuilder, ReportKind, SourceFileId, Span, resolve};

impl<'reports> TypeInference<'reports> {
    fn file_id(&self) -> SourceFileId {
        self.processed_file.unwrap()
    }

    pub fn format_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::Float => "float".to_string(),
            Type::Void => "void".to_string(),
            Type::Reference(reference) => format!("&{}", self.format_type(&reference)),
            Type::Pointer(pointer) => format!("*{}", self.format_type(&pointer)),
            Type::Array(ty, size) => format!("[{}, {}]", self.format_type(&ty), size.unwrap_or(0)),
            Type::Inferred => "_".to_string(),
            Type::Function { params, return_type } => {
                let mut params_str = String::new();
                for param in params {
                    params_str.push_str(&self.format_type(&param));
                }
                format!("fn({}) -> {}", params_str, self.format_type(&return_type))
            }
            Type::Named { name, generics } => {
                let mut gens = vec![];
                for generic in generics {
                    gens.push(self.format_type(&generic));
                }
                format!("{}<{}>", resolve(name), gens.join(", "))
            }
            Type::Error => "error".bright_red().bold().to_string(),
        }
    }

    pub fn type_mismatch(&mut self, expected: &Type, found: &Type, span: Span) {
        let report = ReportBuilder::builder(
            &format!(
                "expected type {}, found {}",
                self.format_type(expected).dimmed().bold(),
                self.format_type(found).dimmed().bold()
            ),
            ReportKind::Error,
        )
        .label("here", span);

        self.reports.add(self.file_id(), report);
    }

    pub fn return_type_mismatch(&mut self, expected: &Type, found: &Type, span: Span) {
        let report = ReportBuilder::builder(
            &format!(
                "expected return type {}, found {}",
                self.format_type(expected).dimmed().bold(),
                self.format_type(found).dimmed().bold()
            ),
            ReportKind::Error,
        )
        .label("here", span);

        self.reports.add(self.file_id(), report);
    }
}
