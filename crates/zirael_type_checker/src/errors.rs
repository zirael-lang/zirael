use crate::TypeInference;
use zirael_parser::Type;
use zirael_utils::prelude::{
    Colorize, ReportBuilder, ReportKind, SourceFileId, Span, debug, resolve,
};

impl<'reports> TypeInference<'reports> {
    fn file_id(&self) -> SourceFileId {
        self.processed_file.unwrap()
    }

    pub fn format_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Uint => "uint".to_string(),
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
                let params_str =
                    params.iter().map(|p| self.format_type(p)).collect::<Vec<_>>().join(", ");
                format!("fn({}) -> {}", params_str, self.format_type(&return_type))
            }
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    resolve(name).to_string()
                } else {
                    let gens =
                        generics.iter().map(|g| self.format_type(g)).collect::<Vec<_>>().join(", ");
                    format!("{}<{}>", resolve(name), gens)
                }
            }
            Type::Variable { name, .. } => {
                format!("'{}", resolve(name))
            }
            Type::BoundedVariable { name, bounds, .. } => {
                let bounds_str = if bounds.is_empty() {
                    "".to_string()
                } else {
                    let constraints = bounds
                        .iter()
                        .map(|b| format!("{}", resolve(&b.name)))
                        .collect::<Vec<_>>()
                        .join(" + ");
                    format!(": {}", constraints)
                };
                format!("'{}{}", resolve(name), bounds_str)
            }
            Type::MonomorphizedSymbol(sym) => self.format_type(&sym.display_ty),
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

        debug!("mismatched return type: \n  expected: {:?}\n  found: {:?}", expected, found);

        self.reports.add(self.file_id(), report);
    }
}
