//! File Type Dictionary — YAML-driven, runtime-loaded.
//!
//! Single source of truth for file type knowledge. Follows the same pattern
//! as `fs_types.rs`: tries disk first (`data/filetypes.yaml`), falls back
//! to the embedded copy compiled into the binary.
//!
//! **To add a new file type: edit `data/filetypes.yaml`. No Rust changes needed.**

use std::collections::HashMap;
use serde::Deserialize;

use crate::type_expr::TypeExpr;

// ---------------------------------------------------------------------------
// Embedded fallback
// ---------------------------------------------------------------------------

const FILETYPES_YAML: &str = include_str!("../data/filetypes.yaml");

// ---------------------------------------------------------------------------
// YAML schema (serde)
// ---------------------------------------------------------------------------

/// Top-level YAML structure.
#[derive(Debug, Deserialize)]
struct FileTypesYaml {
    #[allow(dead_code)]
    categories: Option<Vec<String>>,
    #[serde(default)]
    format_families: HashMap<String, String>,
    filetypes: Vec<FileTypeRaw>,
}

/// Raw YAML entry before post-processing.
#[derive(Debug, Deserialize)]
struct FileTypeRaw {
    ext: String,
    category: String,
    description: String,
    type_expr: String,
    #[serde(default)]
    relevant_ops: Vec<String>,
    #[serde(default)]
    tools: Vec<String>,
}

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// A file type category.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FileTypeCategory {
    Image,
    Video,
    Audio,
    Document,
    Archive,
    SourceCode,
    Data,
    Config,
    Markup,
    Database,
    Font,
    Executable,
    DiskImage,
    Ebook,
}

impl FileTypeCategory {
    fn from_str(s: &str) -> Option<FileTypeCategory> {
        match s {
            "image" => Some(FileTypeCategory::Image),
            "video" => Some(FileTypeCategory::Video),
            "audio" => Some(FileTypeCategory::Audio),
            "document" => Some(FileTypeCategory::Document),
            "archive" => Some(FileTypeCategory::Archive),
            "source_code" => Some(FileTypeCategory::SourceCode),
            "data" => Some(FileTypeCategory::Data),
            "config" => Some(FileTypeCategory::Config),
            "markup" => Some(FileTypeCategory::Markup),
            "database" => Some(FileTypeCategory::Database),
            "font" => Some(FileTypeCategory::Font),
            "executable" => Some(FileTypeCategory::Executable),
            "disk_image" => Some(FileTypeCategory::DiskImage),
            "ebook" => Some(FileTypeCategory::Ebook),
            _ => None,
        }
    }

    /// Human-readable label.
    pub fn label(&self) -> &'static str {
        match self {
            FileTypeCategory::Image => "Image",
            FileTypeCategory::Video => "Video",
            FileTypeCategory::Audio => "Audio",
            FileTypeCategory::Document => "Document",
            FileTypeCategory::Archive => "Archive",
            FileTypeCategory::SourceCode => "Source Code",
            FileTypeCategory::Data => "Data",
            FileTypeCategory::Config => "Config",
            FileTypeCategory::Markup => "Markup",
            FileTypeCategory::Database => "Database",
            FileTypeCategory::Font => "Font",
            FileTypeCategory::Executable => "Executable",
            FileTypeCategory::DiskImage => "Disk Image",
            FileTypeCategory::Ebook => "Ebook",
        }
    }
}

/// A loaded file type entry with parsed TypeExpr.
#[derive(Debug, Clone)]
pub struct FileTypeEntry {
    pub ext: String,
    pub category: FileTypeCategory,
    pub description: String,
    pub type_expr: TypeExpr,
    pub type_expr_str: String,
    pub relevant_ops: Vec<String>,
    pub tools: Vec<String>,
}

/// The loaded file type dictionary.
pub struct FileTypeDictionary {
    /// Extension → entry (lowercase, no dot). Includes compound exts like "tar.gz".
    by_ext: HashMap<String, FileTypeEntry>,
    /// Compound extensions sorted longest-first for greedy matching.
    compound_exts: Vec<String>,
    /// Archive format → tool family (e.g., "Cbz" → "zip", "TarGz" → "tar_gz").
    format_families: HashMap<String, String>,
}

// ---------------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------------

/// Load error.
#[derive(Debug)]
pub struct FileTypeLoadError(pub String);

impl std::fmt::Display for FileTypeLoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "filetypes load error: {}", self.0)
    }
}

impl std::error::Error for FileTypeLoadError {}

/// Load the file type dictionary from a YAML string.
pub fn load_filetypes_str(yaml: &str) -> Result<FileTypeDictionary, FileTypeLoadError> {
    let raw: FileTypesYaml = serde_yaml::from_str(yaml)
        .map_err(|e| FileTypeLoadError(format!("YAML parse error: {}", e)))?;

    let mut by_ext = HashMap::new();
    let mut compound_exts = Vec::new();

    for entry in raw.filetypes {
        let category = FileTypeCategory::from_str(&entry.category)
            .ok_or_else(|| FileTypeLoadError(format!(
                "unknown category '{}' for ext '{}'", entry.category, entry.ext
            )))?;

        let type_expr = TypeExpr::parse(&entry.type_expr)
            .map_err(|e| FileTypeLoadError(format!(
                "type_expr parse error for '{}': {}", entry.ext, e
            )))?;

        let ext_lower = entry.ext.to_lowercase();
        if ext_lower.contains('.') {
            compound_exts.push(ext_lower.clone());
        }

        by_ext.insert(ext_lower, FileTypeEntry {
            ext: entry.ext,
            category,
            description: entry.description,
            type_expr,
            type_expr_str: entry.type_expr,
            relevant_ops: entry.relevant_ops,
            tools: entry.tools,
        });
    }

    // Sort compound extensions longest-first for greedy matching
    compound_exts.sort_by(|a, b| b.len().cmp(&a.len()));

    Ok(FileTypeDictionary { by_ext, compound_exts, format_families: raw.format_families })
}

/// Load from disk, falling back to embedded YAML.
pub fn load_filetypes() -> FileTypeDictionary {
    if let Ok(yaml) = std::fs::read_to_string("data/filetypes.yaml") {
        if let Ok(dict) = load_filetypes_str(&yaml) {
            return dict;
        }
    }
    load_filetypes_str(FILETYPES_YAML)
        .expect("embedded filetypes.yaml should always parse")
}

// ---------------------------------------------------------------------------
// Query API
// ---------------------------------------------------------------------------

impl FileTypeDictionary {
    /// Look up a file type by bare extension (no dot). Case-insensitive.
    ///
    /// ```text
    /// lookup("rs")     → Some(FileTypeEntry { ... })
    /// lookup("tar.gz") → Some(FileTypeEntry { ... })
    /// lookup("xyzzy")  → None
    /// ```
    pub fn lookup(&self, ext: &str) -> Option<&FileTypeEntry> {
        self.by_ext.get(&ext.to_lowercase())
    }

    /// Look up a file type by file path or filename.
    ///
    /// Tries compound extensions first (longest match), then simple extension.
    ///
    /// ```text
    /// lookup_by_path("archive.tar.gz") → tar.gz entry
    /// lookup_by_path("photo.png")      → png entry
    /// lookup_by_path("/tmp/data.json") → json entry
    /// ```
    pub fn lookup_by_path(&self, path: &str) -> Option<&FileTypeEntry> {
        let path_lower = path.to_lowercase();

        // Try compound extensions first (longest match)
        for compound in &self.compound_exts {
            if path_lower.ends_with(&format!(".{}", compound)) {
                if let Some(entry) = self.by_ext.get(compound) {
                    return Some(entry);
                }
            }
        }

        // Simple extension
        if let Some(dot_pos) = path_lower.rfind('.') {
            let ext = &path_lower[dot_pos + 1..];
            if !ext.is_empty() {
                return self.by_ext.get(ext);
            }
        }

        None
    }

    /// Check if an extension is known. Case-insensitive, no dot.
    pub fn is_known_extension(&self, ext: &str) -> bool {
        self.by_ext.contains_key(&ext.to_lowercase())
    }

    /// Check if a path has a known file extension.
    pub fn has_known_extension(&self, path: &str) -> bool {
        self.lookup_by_path(path).is_some()
    }

    /// Get all extensions for a given category.
    pub fn extensions_for_category(&self, category: &FileTypeCategory) -> Vec<&str> {
        self.by_ext.values()
            .filter(|e| &e.category == category)
            .map(|e| e.ext.as_str())
            .collect()
    }

    /// Human-readable description of a file type, with ops and tools.
    ///
    /// ```text
    /// describe_file_type("pdf")
    /// → "PDF document — ops: open_file, copy, checksum, stat, reveal, spotlight_search | tools: pdftotext, pdfgrep, ghostscript"
    /// ```
    pub fn describe_file_type(&self, ext: &str) -> String {
        match self.lookup(ext) {
            Some(entry) => {
                let mut parts = vec![entry.description.clone()];
                if !entry.relevant_ops.is_empty() {
                    parts.push(format!("ops: {}", entry.relevant_ops.join(", ")));
                }
                if !entry.tools.is_empty() {
                    parts.push(format!("tools: {}", entry.tools.join(", ")));
                }
                parts.join(" — ")
            }
            None => format!("Unknown file type: .{}", ext),
        }
    }

    /// All known extensions (no dots, lowercase).
    pub fn all_extensions(&self) -> Vec<&str> {
        self.by_ext.keys().map(|k| k.as_str()).collect()
    }

    /// Total number of entries.
    pub fn len(&self) -> usize {
        self.by_ext.len()
    }

    /// Whether the dictionary is empty.
    pub fn is_empty(&self) -> bool {
        self.by_ext.is_empty()
    }

    /// Look up the tool family for an archive format primitive.
    ///
    /// ```text
    /// format_family("Cbz")   → Some("zip")
    /// format_family("TarGz") → Some("tar_gz")
    /// format_family("Rar")   → Some("rar")
    /// format_family("Text")  → None  (not an archive format)
    /// ```
    pub fn format_family(&self, format_name: &str) -> Option<&str> {
        self.format_families.get(format_name).map(|s| s.as_str())
    }
}

// ---------------------------------------------------------------------------
// Global singleton (thread-safe lazy init)
// ---------------------------------------------------------------------------

use std::sync::OnceLock;

static DICTIONARY: OnceLock<FileTypeDictionary> = OnceLock::new();

/// Get the global file type dictionary (loaded once, cached).
pub fn dictionary() -> &'static FileTypeDictionary {
    DICTIONARY.get_or_init(load_filetypes)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn dict() -> &'static FileTypeDictionary {
        dictionary()
    }

    // --- Happy path ---

    #[test]
    fn test_lookup_rs() {
        let entry = dict().lookup("rs").expect("rs should exist");
        assert_eq!(entry.category, FileTypeCategory::SourceCode);
        assert_eq!(entry.type_expr_str, "File(Text)");
        assert!(entry.relevant_ops.contains(&"read_file".to_string()));
        assert!(entry.relevant_ops.contains(&"search_content".to_string()));
    }

    #[test]
    fn test_lookup_pdf() {
        let entry = dict().lookup("pdf").expect("pdf should exist");
        assert_eq!(entry.category, FileTypeCategory::Document);
        assert_eq!(entry.type_expr_str, "File(PDF)");
        assert!(entry.relevant_ops.contains(&"open_file".to_string()));
    }

    #[test]
    fn test_lookup_mp4() {
        let entry = dict().lookup("mp4").expect("mp4 should exist");
        assert_eq!(entry.category, FileTypeCategory::Video);
        assert!(entry.tools.contains(&"ffmpeg".to_string()));
    }

    #[test]
    fn test_lookup_json() {
        let entry = dict().lookup("json").expect("json should exist");
        assert_eq!(entry.category, FileTypeCategory::Data);
        assert_eq!(entry.type_expr_str, "File(Json)");
        assert!(entry.relevant_ops.contains(&"jq_query".to_string()));
    }

    #[test]
    fn test_lookup_flac() {
        let entry = dict().lookup("flac").expect("flac should exist");
        assert_eq!(entry.category, FileTypeCategory::Audio);
        assert!(entry.tools.contains(&"sox".to_string()));
    }

    #[test]
    fn test_lookup_case_insensitive() {
        assert!(dict().lookup("RS").is_some());
        assert!(dict().lookup("Py").is_some());
        assert!(dict().lookup("JSON").is_some());
    }

    // --- Scheme & Racket ---

    #[test]
    fn test_scheme_scm() {
        let entry = dict().lookup("scm").expect("scm should exist");
        assert_eq!(entry.category, FileTypeCategory::SourceCode);
        assert!(entry.description.contains("Scheme"));
        assert!(entry.relevant_ops.contains(&"read_file".to_string()));
    }

    #[test]
    fn test_scheme_ss() {
        let entry = dict().lookup("ss").expect("ss should exist");
        assert_eq!(entry.category, FileTypeCategory::SourceCode);
        assert!(entry.description.contains("Scheme"));
    }

    #[test]
    fn test_racket_rkt() {
        let entry = dict().lookup("rkt").expect("rkt should exist");
        assert_eq!(entry.category, FileTypeCategory::SourceCode);
        assert!(entry.description.contains("Racket"));
        assert!(entry.tools.contains(&"racket".to_string()));
    }

    // --- Compound extensions ---

    #[test]
    fn test_lookup_tar_gz_compound() {
        let entry = dict().lookup("tar.gz").expect("tar.gz should exist");
        assert_eq!(entry.category, FileTypeCategory::Archive);
        assert_eq!(entry.type_expr_str, "File(Archive(Bytes, TarGz))");
    }

    #[test]
    fn test_lookup_by_path_tar_gz() {
        let entry = dict().lookup_by_path("archive.tar.gz").expect("should match tar.gz");
        assert_eq!(entry.ext, "tar.gz");
        assert_eq!(entry.category, FileTypeCategory::Archive);
    }

    #[test]
    fn test_lookup_by_path_tar_bz2() {
        let entry = dict().lookup_by_path("/tmp/backup.tar.bz2").expect("should match tar.bz2");
        assert_eq!(entry.ext, "tar.bz2");
    }

    #[test]
    fn test_lookup_by_path_tar_xz() {
        let entry = dict().lookup_by_path("~/data.tar.xz").expect("should match tar.xz");
        assert_eq!(entry.ext, "tar.xz");
    }

    #[test]
    fn test_lookup_by_path_simple() {
        let entry = dict().lookup_by_path("photo.png").expect("should match png");
        assert_eq!(entry.ext, "png");
        assert_eq!(entry.category, FileTypeCategory::Image);
    }

    #[test]
    fn test_lookup_by_path_full_path() {
        let entry = dict().lookup_by_path("/Users/me/Documents/report.pdf")
            .expect("should match pdf");
        assert_eq!(entry.ext, "pdf");
    }

    // --- Negative ---

    #[test]
    fn test_lookup_unknown() {
        assert!(dict().lookup("xyzzy").is_none());
    }

    #[test]
    fn test_lookup_empty() {
        assert!(dict().lookup("").is_none());
    }

    #[test]
    fn test_lookup_by_path_no_extension() {
        assert!(dict().lookup_by_path("Makefile").is_none());
        assert!(dict().lookup_by_path("/usr/bin/ls").is_none());
    }

    #[test]
    fn test_lookup_by_path_dot_only() {
        assert!(dict().lookup_by_path("file.").is_none());
    }

    #[test]
    fn test_describe_unknown() {
        let desc = dict().describe_file_type("xyzzy");
        assert!(desc.contains("Unknown"));
    }

    // --- is_known_extension ---

    #[test]
    fn test_is_known_extension() {
        assert!(dict().is_known_extension("rs"));
        assert!(dict().is_known_extension("py"));
        assert!(dict().is_known_extension("mp4"));
        assert!(!dict().is_known_extension("xyzzy"));
    }

    #[test]
    fn test_has_known_extension_path() {
        assert!(dict().has_known_extension("photo.heic"));
        assert!(dict().has_known_extension("video.webm"));
        assert!(dict().has_known_extension("code.rkt"));
        assert!(!dict().has_known_extension("noext"));
    }

    // --- Category queries ---

    #[test]
    fn test_extensions_for_source_code() {
        let exts = dict().extensions_for_category(&FileTypeCategory::SourceCode);
        assert!(exts.len() >= 35, "expected 35+ source code exts, got {}", exts.len());
        assert!(exts.contains(&"rs"));
        assert!(exts.contains(&"scm"));
        assert!(exts.contains(&"ss"));
        assert!(exts.contains(&"rkt"));
    }

    #[test]
    fn test_extensions_for_image() {
        let exts = dict().extensions_for_category(&FileTypeCategory::Image);
        assert!(exts.len() >= 15, "expected 15+ image exts, got {}", exts.len());
    }

    #[test]
    fn test_extensions_for_archive() {
        let exts = dict().extensions_for_category(&FileTypeCategory::Archive);
        assert!(exts.len() >= 15, "expected 15+ archive exts, got {}", exts.len());
    }

    // --- describe_file_type ---

    #[test]
    fn test_describe_pdf() {
        let desc = dict().describe_file_type("pdf");
        assert!(desc.contains("PDF document"));
        assert!(desc.contains("open_file"));
        assert!(desc.contains("pdftotext"));
    }

    #[test]
    fn test_describe_rs() {
        let desc = dict().describe_file_type("rs");
        assert!(desc.contains("Rust"));
        assert!(desc.contains("read_file"));
        assert!(desc.contains("cargo"));
    }

    // --- Dictionary size ---

    #[test]
    fn test_dictionary_size() {
        assert!(dict().len() >= 170, "expected 170+ entries, got {}", dict().len());
    }

    #[test]
    fn test_all_extensions() {
        let exts = dict().all_extensions();
        assert!(exts.len() >= 170);
    }

    // --- TypeExpr parsing ---

    #[test]
    fn test_type_expr_parses_correctly() {
        // Verify a few key type expressions actually parsed
        let rs = dict().lookup("rs").unwrap();
        assert_eq!(rs.type_expr, TypeExpr::file(TypeExpr::prim("Text")));

        let zip = dict().lookup("zip").unwrap();
        assert_eq!(zip.type_expr, TypeExpr::file(TypeExpr::archive(
            TypeExpr::prim("Bytes"),
            TypeExpr::prim("Zip"),
        )));

        let cbz = dict().lookup("cbz").unwrap();
        assert_eq!(cbz.type_expr, TypeExpr::file(TypeExpr::archive(
            TypeExpr::file(TypeExpr::prim("Image")),
            TypeExpr::prim("Cbz"),
        )));

        let png = dict().lookup("png").unwrap();
        assert_eq!(png.type_expr, TypeExpr::file(TypeExpr::prim("Image")));
    }

    // --- No duplicates (structural check) ---

    #[test]
    fn test_no_duplicate_extensions() {
        // The HashMap naturally deduplicates, but let's verify the YAML
        // doesn't have entries that silently shadow each other
        let yaml = include_str!("../data/filetypes.yaml");
        let raw: FileTypesYaml = serde_yaml::from_str(yaml).unwrap();
        let mut seen = std::collections::HashSet::new();
        for entry in &raw.filetypes {
            let ext = entry.ext.to_lowercase();
            assert!(seen.insert(ext.clone()), "duplicate extension in YAML: {}", ext);
        }
    }
}
